        SUBROUTINE ZCLBMD(PASS,READNO,CCRE,SAAFIL,DET,EXPPKT,FLAG,NS,
     *                    FLUX,BCK,PEDGRE,ISTAT)
*
*  Module number:
*
*  Module name: ZCLBMD
*
*  Keyphrase:
*  ----------
*       GHRS background subtraction using a background count rate
*       model INSTEAD of acquired data
*       
*  Description:
*  ------------
*       This routine determines the predicted background count rate
*       based upon an empirically-determined model.  The model is
*       a function of the geomagnetic coordinates of the telescope
*       at the time of the observation.
*
*  FORTRAN name: ZCLBMD.F
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*  CCRE                         I       background count rate model
*  SAAFIL                       I       SAA contour
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ZRCCRE, SPLIE2, SPLIN2, ZSTPOS, GEOPOS, ZRDSAA
*
*  SDAS:
*       ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*      1       Oct 96   M. De La Pena   Original Implementation
*      1.1     Nov 96   M. De La Pena   Added READNO,SAAFIL
*-------------------------------------------------------------------------------
*
* Inputs:
*       pass   - integer variable set to 1 on first call, -1 on last
*       readno - readout number
*       ccre   - table name (character*64)
*       saafil - calibration image name (character*64)
*       det    - GHRS detector 1 or 2 (integer)
*       exppkt - PKTTIMEs bounding the readout (two double precision values)
*       flag   - calibration flag
*       ns     - length of the flux array
*
* Inputs/Outputs:
*       flux   - flux array (real, size of ns)
*
* Outputs:
*       bck    - determined background count rate array (real, size of ns)
*       pedgre - pedigree of the input calibration table CCRE
*       istat  - error status (integer)
*
C-------------------------------------------------------------------------------
C
C PASSED PARAMETERS
C
      CHARACTER*12     FLAG
      CHARACTER*64     CCRE,SAAFIL
      CHARACTER*68     PEDGRE
      INTEGER          READNO,PASS,DET,NS,ISTAT
      REAL             FLUX(NS),BCK(NS)
      DOUBLE PRECISION EXPPKT(2)
C------------------------------------------------------------------------------
C
C IRAF MEM COMMON
C
      LOGICAL          MEMB(1)
      INTEGER*2        MEMS(1)
      INTEGER*4        MEMI(1)
      INTEGER*4        MEML(1)
      REAL             MEMR(1)
      DOUBLE PRECISION MEMD(1)
      COMPLEX          MEMX(1)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
C------------------------------------------------------------------------------
C
C ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER   STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER   STDERR
      PARAMETER (STDERR = 2)
C------------------------------------------------------------------------------
C
C                       /ORBPAR/
C Common block containing orbital/positional parameters
C
        DOUBLE PRECISION EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI
        DOUBLE PRECISION ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3
        DOUBLE PRECISION FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER
        DOUBLE PRECISION RASCASCN,SINEINCL,SEMILREC
        DOUBLE PRECISION PSANGLV3,RTASCNV1,DECLNV1
        CHARACTER*2      SAAAVOID
        COMMON /ORBPAR/ EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI,
     *  ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3,
     *  FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER,
     *  RASCASCN,SINEINCL,SEMILREC,
     *  PSANGLV3,RTASCNV1,DECLNV1,SAAAVOID
C------------------------------------------------------------------------------
C
C LOCAL VARIABLES
C
      INTEGER      TYDOUB
      PARAMETER    (TYDOUB = 7)
      INTEGER      FIRST
      PARAMETER    (FIRST  = 1)
      INTEGER      LAST
      PARAMETER    (LAST   = -1)
C
      INTEGER      XMAX 
      PARAMETER    (XMAX   = 359)
      INTEGER      YMAX
      PARAMETER    (YMAX   = 179)
C
      INTEGER      XADJ
      PARAMETER    (XADJ   = 181)
      INTEGER      YADJ
      PARAMETER    (YADJ   = 91)
C
      CHARACTER*80 CONTXT
      INTEGER      I,ISTATS(4),NLAT,NLONG,NBRATE,NINTVL,XLOC,YLOC
      INTEGER      WRNFLG,SAA(360,180)
      INTEGER      GMLAT,GMLONG,BRATES
C				     --->pointers to latitudes,longitudes,rates
      INTEGER      BRAT2D
C                                    --->pointer to 2D spline model of rates
      DOUBLE PRECISION HRPDY,MINPHR
      PARAMETER        (HRPDY  = 24.0) 
      PARAMETER        (MINPHR = 60.0)
C
      REAL             SCALE(2)
      DOUBLE PRECISION NMINS,DELTAT,TOTRAT,MNRATE
      DOUBLE PRECISION INTTIM,LAT,LNG
      DOUBLE PRECISION INTLAT,INTLNG,INTRAT
C-------------------------------------------------------------------------------
C
C If the first time in this routine, write message to denote method 
C of background used, read the calibration file, and initialize the model
C
        IF(PASS.EQ.FIRST)THEN
            CONTXT='Background Count Rate Model used to compute'//
     *             ' the background spectrum.'
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            WRITE(CONTXT,11)SAAAVOID
11          FORMAT('SAA Contour 07 used for Model.  SAA Contour ',
     *             A2,' used for observation.')
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
C
C Read CCRE - the background count rate model and scale factors for the
C specific GHRS detector
C
            CALL ZRCCRE(CCRE,GMLAT,NLAT,GMLONG,NLONG,BRATES,
     *              NBRATE,SCALE,PEDGRE,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCRE table '//CCRE
                GO TO 999
            ENDIF
C
C If the reference table contains dummy data, then skip this routine
C
            IF(PEDGRE(1:5).EQ.'DUMMY')THEN
                CONTXT='WARNING: PEDIGREE = DUMMY for '//CCRE
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                CONTXT='Use of Background Count Rate Model'//
     *                 ' will be SKIPPED.'
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                CONTXT='No background computed. '//
     *                 'BMD_CORR and BCK_CORR set to SKIPPED.'
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                GO TO 1000
            ENDIF
C
C
C Allocate memory for brat2d
C
            CALL UDMGET (NBRATE, TYDOUB, BRAT2D, ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR allocating dynamic memory'
                GO TO 999
            ENDIF
C
C Set up to interpolate
C
	    CALL SPLIE2(MEMD(GMLAT),MEMD(GMLONG),MEMD(BRATES),
     *              NLAT,NLONG,MEMD(BRAT2D))
C
C Read the SAA contour image
C
            CALL ZRDSAA(SAAFIL,FLAG,SAA,ISTAT)
            IF(ISTAT.NE.0) GO TO 1000
C
C End of first pass initializations
C
        ENDIF
C
C Divide the readout into approximate 1 minute intervals
C
        NMINS  = (EXPPKT(2) - EXPPKT(1)) * HRPDY * MINPHR
        NINTVL = NMINS + 1
        DELTAT = (NMINS / NINTVL) / MINPHR / HRPDY
C
C Loop over all the intervals using the midpoint time 
C
        TOTRAT  = 0.0
        WRNFLG  = 0
        DO 10 I = 1, NINTVL
           INTTIM = EXPPKT(1) + (I - 0.5) * DELTAT
C
C Compute the spacecraft position for each interval
C
            CALL ZSTPOS(INTTIM,LNG,LAT,ISTAT)
    	    IF(ISTAT.NE.0)THEN
	        CONTXT='ERROR calculating geographic position'
	        GO TO 999
	    ENDIF
C
C Check the location of the observation and issue a warning
C if near the SAA - only issue the warning once per readout
C
            XLOC = MIN(INT(LNG) + XADJ,XMAX)
            YLOC = MIN(INT(LAT) + YADJ,YMAX)
            IF((SAA(XLOC,YLOC).GT.0).AND.(WRNFLG.EQ.0))THEN
                WRNFLG = 1
                WRITE(CONTXT,12)READNO
12              FORMAT('WARNING: Observation near the SAA for ',
     *                 'readout:',I6)
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            ENDIF
C
C Compute the corresponding geomagnetic position
C
            CALL GEOPOS(LAT,LNG,INTLAT,INTLNG,ISTAT)
	    IF(ISTAT.NE.0)THEN
	        CONTXT='ERROR calculating geomagnetic position'
	        GO TO 999
            ENDIF
C
C Interpolate in predicted rates 
C 
            CALL SPLIN2(MEMD(GMLAT),MEMD(GMLONG),MEMD(BRATES),
     *                  MEMD(BRAT2D),NLAT,NLONG,INTLAT,INTLNG,INTRAT)
	    IF(INTRAT.LT.0.0)INTRAT=0.0
C
C Keep a running total for all intervals within the readout
C
            TOTRAT = TOTRAT + INTRAT
C
10      CONTINUE
C
C Compute the mean rate over all the intervals and scale the mean
C computed background rate for the appropriate GHRS detector
C
        MNRATE = (TOTRAT / NINTVL) * SCALE(DET)
C
C Expand the mean rate into a background vector and compute the net 
C flux
C
        DO 20 I = 1, NS
           BCK(I)  = MNRATE
           FLUX(I) = FLUX(I) - BCK(I)
20      CONTINUE
C
C Clean up memory on the last PASS
C
        IF(PASS.EQ.LAST)THEN
   	    IF (GMLAT  .NE. 0) CALL UDMFRE (GMLAT,  TYDOUB, ISTATS(1))
	    IF (GMLONG .NE. 0) CALL UDMFRE (GMLONG, TYDOUB, ISTATS(2))
	    IF (BRATES .NE. 0) CALL UDMFRE (BRATES, TYDOUB, ISTATS(3))
            IF (BRAT2D .NE. 0) CALL UDMFRE (BRAT2D, TYDOUB, ISTATS(4))
            DO 30 I = 1, 4
                IF(ISTATS(I).NE.0)THEN
                    CONTXT='ERROR deallocating dynamic memory'
                    GO TO 999
	        ENDIF
30	    CONTINUE
        ENDIF
C
        ISTAT=0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
