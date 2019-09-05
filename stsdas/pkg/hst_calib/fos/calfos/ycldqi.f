        SUBROUTINE YCLDQI(FRAME,NAME1,NAME2,FILL,EPS,
     *                    PEDGR1,PEDGR2,DESCR1,DESCR2,ISTAT)
*
*  Module number:
*
*  Module name: YCLDQI
*
*  Keyphrase:
*  ----------
*       data quality initialization
*
*  Description:
*  ------------
*       This routine performs the data quality initialization by
*       changing values in the data quality vector, EPS, to
*       values (tabulated by diode) in the reference file.
*
*  FORTRAN name: ycldqi.for
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       yrddqi, ymsput
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jul 89  D. Lindler      Designed and coded
*	2	May 90  S. Hulbert	Added second DQI file
*     2.1	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1).
*					Added declaration of YPOS.
*	3	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*     3.1       Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* inputs:
*       frame - frame number
*       name1, name2 - reference file names
*       fill - flag for fill data
*
* input/output:
*       eps - data quality vector
*
* outputs:
*	pedgr1 - pedigree keyword string from reference file 1
*	pedgr2 - pedigree keyword string from reference file 2
*	descr1 - descrip  keyword string from reference file 1
*	descr2 - descrip  keyword string from reference file 2
*       istat - error status
*-----------------------------------------------------------------------------
        INTEGER ISTAT,FRAME
        CHARACTER*64 NAME1, NAME2
	CHARACTER*68 PEDGR1, PEDGR2, DESCR1, DESCR2
        REAL FILL,EPS(*)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Common block containing confiquration parameters
C
        CHARACTER*5 DET
        CHARACTER*3 FGWAID,APERID,YTYPE(3)
        CHARACTER*1 POLID
        INTEGER FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME
        LOGICAL HEADER,TRAILR,DEFDDT,KYDPLY
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
        COMMON /CONFG2/FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME,HEADER,TRAILR,
     *          DEFDDT
        INTEGER NX,NOBJ,NSKY,NBCK
        COMMON /CONFG3/NX,NOBJ,NSKY,NBCK
        LOGICAL PAIRED
        REAL YUPPER, YLOWER
        COMMON /CCS1CM/ PAIRED, YUPPER, YLOWER
C
C local variables
C
        CHARACTER*80 CONTXT
	REAL YPOS
        REAL DQI(5000,2)
        INTEGER DQINUM, YOFF, SOFF, I, II, IS, IY
        LOGICAL FOUND(2)
C-----------------------------------------------------------------------------
C
C Read reference data on first call
C
        IF(FRAME.EQ.1)THEN
        	FOUND(1) = .FALSE.
         	FOUND(2) = .FALSE.
                CALL YRDDQI ('DQ1HFILE ',NAME1,5000,FOUND,DQI,
     *                        PEDGR1,DESCR1,ISTAT)
                IF(ISTAT.NE.0)GO TO 1000
                CONTXT='Data quality initialization done using '//
     *        	        	        	NAME1
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C Get 2nd reference file for paired-aperture or polarimetry data
C
        	IF (PAIRED .OR. (POLID .NE. 'C')) THEN
                    CALL YRDDQI ('DQ2HFILE ',NAME2,5000,FOUND,DQI,
     *                            PEDGR2,DESCR2,ISTAT)
                    IF(ISTAT.NE.0)GO TO 1000
                    CONTXT='                                   and '//
     *        	        	        	NAME2
                    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
        	    IF ((.NOT. FOUND(1)) .OR. (.NOT. FOUND(2))) THEN
        	        CONTXT = 'ERROR: both data quality ref. '//
     *        	           'files are for the same aper. or pass dir.'
        	        GO TO 999
        	    ENDIF
        	ENDIF
C
C If the reference files contain dummy data, then fill with zeroes so
C that correction is effectively skipped.
C
		IF(PEDGR1(1:5).EQ.'DUMMY'.OR.PEDGR2(1:5).EQ.'DUMMY')THEN
		IF (PEDGR1(1:5).EQ.'DUMMY') THEN
		    CONTXT='WARNING: PEDIGREE = DUMMY for the DQ1HFILE;'
		    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		    DO 10 I = 1, NX
10		       DQI(I,1) = 0.0
		END IF
		IF (PEDGR2(1:5).EQ.'DUMMY') THEN
		    CONTXT='WARNING: PEDIGREE = DUMMY for the DQ2HFILE;'
		    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		    DO 20 I = 1, NX
20		       DQI(I,2) = 0.0
		END IF
		CONTXT='         DQI initialization will be skipped'
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		END IF
        ENDIF
C------------------ end of frame 1 only processing ----------------------------
C
C LOOP ON SLICES
C
        DO 500 IS = 1, SLICES
            SOFF = NX * YSTEPS * (IS-1)
C        	        	        --->OFFSET FOR THE SLICE
C
C LOOP ON YSTEPS
C
            DO 400 IY = 1, YSTEPS
C
C Determine which of the 2 possible DQI files to use
C
                DQINUM = 1
                IF(PAIRED)THEN
                    YPOS=YBASE+(YRANGE*32)/YSTEPS*(IY-1)
                    IF(ABS(YPOS-YLOWER).LT.ABS(YPOS-YUPPER))
     *                                              DQINUM = 2
                ENDIF
                IF((POLID.NE.'C').AND.(IY.GT.1))DQINUM = 2
C
C Perform initialization
C
        	YOFF = SOFF + (IY-1) * NX
        	DO 100 I = 1, NX
        	    II = YOFF + I
        	    IF (EPS(II) .LT. DQI(I,DQINUM)) THEN
        	        EPS(II) = DQI(I,DQINUM)
        	    ENDIF
  100           CONTINUE
  400        CONTINUE
  500   CONTINUE
C
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT = 1
1000    RETURN
        END
