        SUBROUTINE ZRCCRE(CCRE,GMLAT,NLAT,GMLONG,NLONG,BRATES,NBRATE,
     $			  SCALE,PEDGRE,ISTAT)
*
*  Module number:
*
*  Module name: ZRCCRE
*
*  Keyphrase:
*  ----------
*       Index table CCRE containing predicted background count rates
*
*  Description:
*  ------------
*       This routine reads table CCRE and extracts the geomagnetic
*	latitudes and longitudes and predicted background rates.          
*       Sorted lists of latitude, longitude, and predicted background rates
*	are returned.  In addition, scale factors which are stored as
*       table keywords are returned.  The scale factors are needed to
*       adjust appropriately the model for the specific GHRS detector.
*
*  FORTRAN name: ZRCCRE.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCRE                    I       table containing CCRE coeffecients
*  Subroutines Called:
*  -------------------
*  CDBS
*       zmsput
*  SDAS:
*       uttopn, utpgti, utcfnd, utrgt*, uttclo
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Oct 96  M. De La Pena   Copied and modified from YRCCS8.FOR
*                                       written by S. Hulbert for CALFOS
*       1.1     May 98  M. De La Pena   Modified column name for CCRE table,
*                                       DETECTOR -> FOS_DETECTOR.
*-------------------------------------------------------------------------------
*
* Inputs:
*
*       ccre - table name (character*64)
*
* Outputs:
*
*	gmlat  - pointer to vector of geomagnetic latitudes (integer)
*	nlat   - number of latitudes (integer)
*	gmlong - pointer to vector of geomagnetic longitudes (integer)
*	nlong  - number of longitudes (integer)
*	brates - pointer to vector of predicted background rates (integer)
*	nbrate - number of rates (integer)
*       scale  - scale factors for model to accommodate GHRS detectors (real)
*       istat  - error status (integer)
*
*-----------------------------------------------------------------------------
C
C PASSED PARAMETERS
C
      INTEGER      NLAT,NLONG,NBRATE,ISTAT
      INTEGER      GMLAT,GMLONG,BRATES
      REAL         SCALE(2)
      CHARACTER*64 CCRE
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
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C------------------------------------------------------------------------------
C
C LOCAL VARIABLES
C
      INTEGER TYINT
      PARAMETER (TYINT = 4)
      INTEGER TYDOUB
      PARAMETER (TYDOUB = 7)
C
C FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
C
      REAL         DEFSCL(2),XLAT,XLONG
      INTEGER      I,J,IDIN,COLIDS(4),ISTATS(4),NROWS,ROW,IROW 
      INTEGER      INDEX,NB
      CHARACTER*5  DETECTOR
      CHARACTER*15 COLNAM(4)
      CHARACTER*68 PEDGRE,DESCRP
      CHARACTER*80 CONTXT
      LOGICAL      NULL,FOLD
      DATA         DEFSCL/0.93,1.67/
      DATA         COLNAM/'FOS_DETECTOR','GM_LONG','GM_LAT','BACK_RATE'/
C------------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCRE,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR opening CCRE table '//CCRE
            GO TO 998
        ENDIF
C
C Get PEDIGREE, DESCRIP, and SCALE header keywords
C NOTE: PEDIGREE and DESCRIP do not exist in older calibration files
C
        CALL UTHGTT(IDIN,'PEDIGREE',PEDGRE,ISTATS(1))
        CALL UTHGTT(IDIN,'DESCRIP',DESCRP,ISTATS(2))
        CALL UTHGTR(IDIN,'D1SCALE',SCALE(1),ISTATS(3))
        CALL UTHGTR(IDIN,'D2SCALE',SCALE(2),ISTATS(4))
        DO 10 I = 3, 4
            IF(ISTATS(I).NE.0)THEN
                J = I - 2
                WRITE(CONTXT,11) DEFSCL(J)
11              FORMAT('ERROR reading scaling factor keyword; ',
     *                 'keyword set to default value: ',f4.2)
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                SCALE(J) = DEFSCL(J)
            ENDIF
10      CONTINUE
C
C If the table contains dummy data, then don't bother reading it
C
	IF (PEDGRE(1:5).EQ.'DUMMY') GO TO 997
C
C Get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading CCRE table '//CCRE
            GO TO 999
        ENDIF
	IF(NROWS.LT.2)THEN
            CONTXT='ERROR -- not enough rows in CCRE table '//CCRE
            GO TO 999
	ENDIF
C
C Get column ids
C
        CALL UTCFND(IDIN,COLNAM,4,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR locating needed columns in CCRE table '//CCRE
            GO TO 999
	ENDIF
C
C Allocate memory for arrays
C
        CALL UDMGET (NROWS, TYDOUB, GMLAT, ISTATS(1))
        CALL UDMGET (NROWS, TYDOUB, GMLONG, ISTATS(2))
        CALL UDMGET (NROWS, TYDOUB, BRATES, ISTATS(3))
        CALL UDMGET (NROWS, TYINT, INDEX, ISTATS(4))
        DO 20 I = 1, 4
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR allocating dynamic memory'
                GO TO 999
            ENDIF
20      CONTINUE
C
C Sort the table on the first three columns
C
	DO 30 I=1,NROWS
	    MEMI(INDEX+I-1)=I
30	CONTINUE
	FOLD = .TRUE.
	CALL TBTSRT (IDIN, 3, COLIDS, FOLD, NROWS, MEMI(INDEX))
C
C Load the arrays
C
	NB=0
        DO 40 ROW=1,NROWS
	    IROW=MEMI(INDEX+ROW-1)
            CALL UTRGTT(IDIN,COLIDS(1),1,IROW,DETECTOR,NULL,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading DETECTOR from CCRE table '//CCRE
                GO TO 999
            ENDIF
	    NB=NB+1
            CALL UTRGTD(IDIN,COLIDS(3),1,IROW,MEMD(GMLAT+NB-1),
     $			NULL,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading GM_LAT from CCRE table '//CCRE
                GO TO 999
            ENDIF
            CALL UTRGTD(IDIN,COLIDS(2),1,IROW,MEMD(GMLONG+NB-1),
     $			NULL,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading GM_LONG from CCRE table '//
     $				CCRE
                GO TO 999
            ENDIF
            CALL UTRGTD(IDIN,COLIDS(4),1,IROW,MEMD(BRATES+NB-1),
     $		        NULL,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading BACK_RATE from CCRE table '//
     $				CCRE
                GO TO 999
            ENDIF
40      CONTINUE
	NBRATE=NB
C
C Sort for unique latitudes and longitudes and return just these values
C
	NLAT=1
	XLAT=MEMD(GMLAT+NLAT-1)
	DO 50 I=2,NBRATE
	    IF(MEMD(GMLAT+I-1).GT.XLAT)THEN
		NLAT=NLAT+1
		MEMD(GMLAT+NLAT-1)=MEMD(GMLAT+I-1)
		XLAT=MEMD(GMLAT+NLAT-1)
	    ENDIF
50	CONTINUE
	NLONG=1
	XLONG=MEMD(GMLONG+NLONG-1)
	DO 60 I=2,NBRATE
	    IF(MEMD(GMLONG+I-1).GT.XLONG)THEN
		NLONG=NLONG+1
		MEMD(GMLONG+NLONG-1)=MEMD(GMLONG+I-1)
		XLONG=MEMD(GMLONG+NLONG-1)
	    ENDIF
60	CONTINUE
C
        CALL UDMFRE (INDEX, TYINT, ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR allocating dynamic memory'
            GO TO 999
        ENDIF
C
997     CALL UTTCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
