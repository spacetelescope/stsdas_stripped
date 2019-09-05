        SUBROUTINE ZCLDQI(PASS,FLAG,NINIT,XOFF,DQIFIL,DET,NBINS,
     *     EPS,EPSET,ISTAT)
*
*  Module number:
*
*  Module name: ZCLDQI
*
*  Keyphrase:
*  ----------
*       data quality initialization
*  Description:
*  ------------
*       This routine performs the data quality initialization by
*       flagging each data value with the values in the
*       data quality initialization file.  The file contains
*       a data quality value for each diode.  If the present
*       data quality value in EPS or EPSET is larger than the
*       data qualify initialization file value for the diode,
*       it is left unchanged.  Quality values are never decreased.
*
*  FORTRAN name: zcldqi.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*   DQIFIL                      I       data quality initialization file
*                                       It contains one group of 512 values.
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  SDAS:
*       ZMSPUT, uimopn, uigl1r, uimgid, uimclo
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Feb 89  D. Lindler      Designed and coded
*      1.1      Sep 91  S. Hulbert      Implemented PASS flag
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       first - logical variable set to TRUE for first call to the routine
*       flag - The calibration flag.
*       ninit - number of initial deflections
*       xoff - diode offsets for the combaddition pattern
*       dqifil - reference file name character*64
*       det - detector number (integer 1 or 2)
*       nbins - number of bins (integer 1 to 7)
*
* Input/Output parameters
*
*       eps - epsilon array for main diodes (real*4 500 x nbins)
*       epset - epsilon array for trailer (real*4 24 x nbins)
*
* Output parameter
*
*       istat - error status (integer)
*-------------------------------------------------------------------------------
        INTEGER PASS
        CHARACTER*64 DQIFIL
        CHARACTER*12 FLAG
        INTEGER DET,NBINS,ISTAT,XOFF(7),NINIT
        REAL EPS(500,NBINS),EPSET(24,NBINS)
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C HRS epsilons
C
        INTEGER EPSDED
        PARAMETER (EPSDED = 400)
        INTEGER EPSCMB
        PARAMETER (EPSCMB =  30)
C
C LOCAL VARIABLES
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
        CHARACTER*80 CONTXT
        REAL DQI(512),MDQI(512),TOTDED(512)
C                                    --->512 data quality initialization values
        INTEGER IDIN,DIMEN(8),NAXIS,DTYPE
C                                    --->file I/O parameters
        INTEGER DIODE
C                                    --->Diode number
        INTEGER I
C                                    --->loop index
        INTEGER IDET,IPOS,J
C------------------------------------------------------------------------------
C
C If first call, read reference file
C
        IF(PASS.EQ.FIRST)THEN
C
C open file
C
           CALL ZREFOP(DQIFIL,'DQIHFILE','DQI_CORR',FLAG,IDIN,ISTAT)
           IF(FLAG.NE.'PERFORM')THEN
              ISTAT=0
              GO TO 1000
           ENDIF
           IF(ISTAT.NE.0)THEN
              CONTXT='ERROR opening data quality init. file '//DQIFIL
              GO TO 999
           ENDIF
C
C Get file size parameters and verify
C
            CALL UIMGID(IDIN,DTYPE,NAXIS,DIMEN,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading data quality init. file '//DQIFIL
                GO TO 998
            ENDIF
            IF((NAXIS.NE.1).OR.(DIMEN(1).NE.512)) THEN
                CONTXT='ERROR: data quality init. file has '//
     *                  'invalid dimensions '//DQIFIL
                GO TO 998
            ENDIF
C
C get detector number from file and verify
C
            CALL UHDGSI(IDIN,'DETECTOR',IDET,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='WARNING: DETECTOR keyword missing from the '//
     *                  'data quality init. file'
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              ELSE
                IF(IDET.NE.DET)THEN
                    CONTXT='WARNING: DETECTOR keyword of data '//
     *                  ' quality init. file does not match observation'
                    CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                ENDIF
            ENDIF
C
C Read data quality values
C
            CALL UIGL1R(IDIN,DQI,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading data quality init. file '//DQIFIL
                GO TO 998
            ENDIF
            CALL UIMCLO(IDIN,ISTAT)
C
C Determine which data points are effected due to comb-addition
C
            DO 70 I=1,512
                MDQI(I)=0
                TOTDED(I)=0
70          CONTINUE
            DO 80 I=1,512
                IF(DQI(I).GT.0)THEN
                    DO 75 J=1,NINIT
                        IPOS=I+XOFF(J)
                        IF((IPOS.GE.7).AND.(IPOS.LE.506))THEN
                            IF(MDQI(IPOS).LT.DQI(I))MDQI(IPOS)=DQI(I)
C
C keep track of contributions by dead diodes
C
			    IF (DQI(I) .EQ. EPSDED) 
     $				    TOTDED(IPOS) = TOTDED(IPOS)+1
                        ENDIF
75                  CONTINUE
                ENDIF
80          CONTINUE
        ENDIF
C
C reset epsilon for diodes with some contributions by dead diodes
C
	DO 85 I=7,506
	    IF (MDQI(I) .EQ. EPSDED .AND. TOTDED(I) .LT. NINIT)
     $		    MDQI(I) = EPSCMB
85	CONTINUE
C
C We are now ready to use the initialization values
C
C                                               first set of 6 special diodes
        DO 100 DIODE=1,6
            IF(DQI(DIODE).NE.0.0)THEN
                DO 50 I=1,NBINS
                    IF(EPSET(DIODE,I).LT.DQI(DIODE))
     *                          EPSET(DIODE,I)=DQI(DIODE)
50              CONTINUE
            ENDIF
100     CONTINUE
C                      use multiplexed or combaddition MDQI   main diode array
        DO 200 DIODE=7,506
            IF(MDQI(DIODE).NE.0.0)THEN
                IPOS=DIODE-6
                DO 150 I=1,NBINS
                    IF(EPS(IPOS,I).LT.MDQI(DIODE))
     *                      EPS(IPOS,I)=MDQI(DIODE)
150             CONTINUE
            ENDIF
200     CONTINUE
C                                               last six special diodes
        DO 300 DIODE=507,512
            IF(DQI(DIODE).NE.0.0)THEN
                IPOS=DIODE-500
                DO 250 I=1,NBINS
                    IF(EPSET(IPOS,I).LT.DQI(DIODE))
     *                          EPSET(IPOS,I)=DQI(DIODE)
250             CONTINUE
           ENDIF
300     CONTINUE
        IF(PASS.EQ.FIRST)THEN
           CONTXT='Data quality initialization performed using '//DQIFIL
           CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF
        ISTAT=0
        GO TO 1000
C
C ERROR SECTION
C
998     CALL UIMCLO(IDIN,ISTAT)
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
