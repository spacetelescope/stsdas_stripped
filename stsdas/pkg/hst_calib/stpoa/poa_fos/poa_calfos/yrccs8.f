C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YRCCS8(CCS8,DET,GMLAT,NLAT,GMLONG,NLONG,
     $			  BRATES,NBRATE,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCS8
*
*  Keyphrase:
*  ----------
*       Index table CCS8 containing predicted background count rates
*
*  Description:
*  ------------
*       This routine reads table CCS8 and extracts the geomagnetic
*	latitudes and longitudes and predicted background rates.          
*       Sorted lists of latitude, longitude, and predicted background rates
*	are returned.
*
*  FORTRAN name: YRCCS8.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCS8                    I       table containing CCS8 coeffecients
*  Subroutines Called:
*  -------------------
*  CDBS
*       ymsput
*  SDAS:
*       uttopn, utpgti, utcfnd, utrgt*, uttclo
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Aug 91  S. Hulbert      Designed and coded
*     1.1	Jan 92	S. Hulbert	Bug Fix -- bad counter while selecting
*					rows to use for interpolation
*	2	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       ccs8 - table name
*	maxrow - maximum allowed rows in table
*
* Output parameters
*
*	lat - pointer to vector of geomagnetic latitudes
*	nlat - number of latitudes
*	long - pointer to vector of geomagnetic longitudes
*	nlong - number of longitudes
*	brates - pointer to vector of predicted background rates
*	nbrate - number of rates  
*	nrows - number of rows found
*	pedgre - CCS8 PEDIGREE keyword
*	descrp - CCS8 DESCRIP  keyword
*       istat - error status
*
*-----------------------------------------------------------------------------
        INTEGER NLAT,NLONG,NBRATE,ISTAT
        CHARACTER*64 CCS8
	CHARACTER*68 PEDGRE,DESCRP
	CHARACTER*5 DET
        INTEGER GMLAT,GMLONG,BRATES
C------------------------------------------------------------------------------
C Get IRAF MEM common into main program.
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
      INTEGER TYINT
      PARAMETER (TYINT = 4)
      INTEGER TYDOUB
      PARAMETER (TYDOUB = 7)
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C     THESE MAY BE READ BY UTPGTI BUT MAY NOT BE SET:
C
C                                       NUMBER OF ROWS WRITTEN TO
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
C     END IRAF77.INC
C
C local variables
C
	REAL XLAT, XLONG
        INTEGER I,IDIN,COLIDS(4),ISTATS(4),NROWS,ROW,IROW
	INTEGER INDEX, NB
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(4)
        CHARACTER*5 DETECTOR
        LOGICAL NULL, FOLD
        DATA COLNAM/'DETECTOR','GM_LONG','GM_LAT','BACK_RATE'/
C
C---------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCS8,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCS8 table '//CCS8
                GO TO 998
        ENDIF
C
C get PEDIGREE and DESCRIP header keywords
C
        CALL UTHGTT(IDIN,'PEDIGREE',PEDGRE,ISTAT)
        CALL UTHGTT(IDIN,'DESCRIP',DESCRP,ISTAT)
C
C if the table contains dummy data, then don't bother reading it
C
	IF (PEDGRE(1:5).EQ.'DUMMY') GO TO 997
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCS8 table '//CCS8
                GO TO 999
        ENDIF
	IF(NROWS.LT.2)THEN
            CONTXT='ERROR -- not enough rows in CCS8 table '//CCS8
            GO TO 999
	ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,4,COLIDS,ISTATS)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR locating needed columns in CCS8 table '//CCS8
            GO TO 999
	ENDIF
C
C allocate memory for arrays
C
        CALL UDMGET (NROWS, TYDOUB, GMLAT, ISTATS(1))
        CALL UDMGET (NROWS, TYDOUB, GMLONG, ISTATS(2))
        CALL UDMGET (NROWS, TYDOUB, BRATES, ISTATS(3))
        CALL UDMGET (NROWS, TYINT, INDEX, ISTATS(4))
        DO 150 I = 1, 4
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR allocating dynamic memory'
                GO TO 999
            ENDIF
150     CONTINUE
C
C Sort the table on the first three columns
C
	DO 110 I=1,NROWS
	    MEMI(INDEX+I-1)=I
110	CONTINUE
	FOLD = .TRUE.
	CALL TBTSRT (IDIN, 3, COLIDS, FOLD, NROWS, MEMI(INDEX))
C
C Load the arrays
C
	NB=0
        DO 10 ROW=1,NROWS
	    IROW=MEMI(INDEX+ROW-1)
            CALL UTRGTT(IDIN,COLIDS(1),1,IROW,DETECTOR,NULL,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading DETECTOR from CCS8 table '//CCS8
                GO TO 999
            ENDIF
	    IF(DETECTOR.EQ.DET)THEN
		NB=NB+1
                CALL UTRGTD(IDIN,COLIDS(3),1,IROW,MEMD(GMLAT+NB-1),
     $				NULL,ISTAT)
                IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR reading GM_LAT from CCS8 table '//CCS8
                    GO TO 999
                ENDIF
                CALL UTRGTD(IDIN,COLIDS(2),1,IROW,MEMD(GMLONG+NB-1),
     $				NULL,ISTAT)
                IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR reading GM_LONG from CCS8 table '//
     $				CCS8
                    GO TO 999
                ENDIF
                CALL UTRGTD(IDIN,COLIDS(4),1,IROW,MEMD(BRATES+NB-1),
     $				NULL,ISTAT)
                IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR reading BACK_RATE from CCS8 table '//
     $				CCS8
                    GO TO 999
                ENDIF
	    ENDIF
10      CONTINUE
	NBRATE=NB
C
C sort for unique latitudes and longitudes and return just these values
C
	NLAT=1
	XLAT=MEMD(GMLAT+NLAT-1)
	DO 20 I=2,NBRATE
	        IF(MEMD(GMLAT+I-1).GT.XLAT)THEN
		    NLAT=NLAT+1
		    MEMD(GMLAT+NLAT-1)=MEMD(GMLAT+I-1)
		    XLAT=MEMD(GMLAT+NLAT-1)
	        ENDIF
20	CONTINUE
	NLONG=1
	XLONG=MEMD(GMLONG+NLONG-1)
	DO 22 I=2,NBRATE
	        IF(MEMD(GMLONG+I-1).GT.XLONG)THEN
		    NLONG=NLONG+1
		    MEMD(GMLONG+NLONG-1)=MEMD(GMLONG+I-1)
		    XLONG=MEMD(GMLONG+NLONG-1)
	        ENDIF
22	CONTINUE
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
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
