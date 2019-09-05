        SUBROUTINE ZCLOPN(ROOT,ISTAT)
*
*  Module number:
*
*  Module name: zclopn
*
*  Keyphrase:
*  ----------
*       Open data files for calibration
*
*  Description:
*  ------------
*       This routine opens the input date files for the
*       rootname ROOT given in common block HRSIO.
*
*  FORTRAN name: zclopn.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <root>.shh              Input   standard header file
*       <root>.ulh              Input   unique data log file
*       <root>.d0h              Input   science data file
*       <root>.q0h              Input   quality file for .d0h
*       <root>.x0h              Input   Eng. trailer file
*       <root>.xqh              Input   Quality file for .xqh
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*               zfname
*  SDAS:
*       ZMSPUT, uimopn, uhdgst, uimgid, uimclo
*  Others:
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jan 89  D. Lindler      Designed and coded
*	1.1		S. Hulbert	Check for no science data
*	1.2	May 91	S. Hulbert	Get aperture from header
*					Use new data quality and OBSMODES
*	1.3	Sep 91	S. Hulbert	Don't get OBSMODE from shp
*-------------------------------------------------------------------------------
*
* Input parameter
*
*       ROOT - rootname of file (ch*64)
*
* Output parameter
*       ISTAT - error status
*
        INTEGER ISTAT
        CHARACTER*64 ROOT
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
      INTEGER RDONLY
      PARAMETER (RDONLY = 1)
C
C                       /HRSIO/
C Common Block containting input/output parameters
C
C   IDS(20) - input file IDs
C               1 - .shh
C               2 - .ulh
C               3 - .d0h
C               4 - .q0h
C               5 - .x0h
C               6 - .xqh
C               7 - .c0h
C               8 - .c1h
C               9 - .cqh
C               10 - .c2h
C               11 - .c3h
C               12 - .c4h
C               13 - .c5h
C   GCOUNT(20) - group counts for input files
C   MERGE - Number of bins merged in output spectra
C   OBSRPT - observation repeats
C   NGOUT - number of output groups
C   NGSDT - number of output groups for the special diode files
C   READNO - readout number
C   NSOUT - number of samples in the output data sets
C
        INTEGER IDS(20),MERGE,OBSRPT,NGOUT,GCOUNT(20),READNO,NSOUT,NGSDT
        COMMON /HRSIO/ IDS,GCOUNT,MERGE,OBSRPT,NGOUT,READNO,NSOUT,NGSDT
C
C                       /HRSMOD/
C Common block containing observing mode parameters
C
C   GRAT - grating mode
C   DET - detector
C   SCLAMP - spectral calibration lamp
C   CARPOS - carrousel position
C   OBSMOD - observation mode (DIR, ACC, TAR)
C   APER - aperture (LSA, SSA, SC1, SC2)
C
        CHARACTER*3 OBSMOD,APER
        CHARACTER*5 GRAT
        INTEGER DET,SCLAMP,CARPOS
        COMMON /HRSMOD/ DET,SCLAMP,CARPOS
        COMMON /HRSMD1/ GRAT,OBSMOD,APER
C
C local variables
C
        INTEGER DIMEN(8),NAXIS,DTYPE,I,ISTATS(6)
        CHARACTER*80 CONTXT
        CHARACTER*64 NAME
	LOGICAL SCIDATA
C
C VALID FILE SIZES AND TYPES AND ERRORS
C
        INTEGER SIZES(6),ERRORS(10),NDIM(6),NS(6)
        CHARACTER*3 TYPES(6)
        DATA TYPES /'shh','ulh','d0h','q0h','x0h','xqh'/
        DATA SIZES /965,80,500,500,24,24/
C
C---------------------------------------------------------------------------
C
C MARK FILES AS  NOT HAVING AN ERROR
C
        DO 1 I=1,10
1                ERRORS(I)=0
C
C Open input files (.shh file is already opened)
C
        DO 10 I=2,6
                CALL ZFNAME(ROOT,TYPES(I),1,0,NAME)
                CALL UIMOPN(NAME,RDONLY,IDS(I),ISTAT)
                IF(ISTAT.NE.0) IDS(I)=-1
10      CONTINUE
C
C check for missing files
C
        DO 20 I=2,6
                IF(IDS(I).EQ.-1)THEN
                        CONTXT='ERROR opening '//TYPES(I)//
     *                          ' file for rootname '//ROOT
                        CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                ENDIF
20      CONTINUE
C
C If d0h is missing check in SHP to see if it should be missing 
C and terminate processing without an error.
C Otherwise, flag the error.
C
        IF(IDS(3).EQ.-1.OR.IDS(5).EQ.-1)THEN
                CALL UHDGSB(IDS(1),'SCIDATA',SCIDATA,ISTAT)
                IF (ISTAT.NE.0) THEN
        	    CONTXT='ERROR: Unable to get SCIDATA keyword'//
     $			 ' from shh header'
        	    GO TO 2000
                ENDIF
		IF (.NOT.SCIDATA) THEN
                    CONTXT='WARNING: No science data for '//
     $				'observation '//ROOT 
                    CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                    GO TO 1500
		ELSE
            	    CONTXT='ERROR: cannot continue without '//
     $				'd0h or x0h file'
                    GO TO 2000
		ENDIF
        ENDIF
C
C get obsmode from d0h
C
        CALL UHDGST(IDS(3),'OBSMODE',OBSMOD,ISTAT)
        IF(ISTAT.NE.0.OR.OBSMOD.EQ.'   ')THEN
            CONTXT='ERROR: Unable to get OBSMODE from the d0h header'
            GO TO 2000
	ENDIF
C
C reset OBSMODE: reclassify OBSMODE as one of following: TAR, ACC, DIR
C
        IF(OBSMOD.EQ.'IMA'.OR.OBSMOD.EQ.'ACQ'.OR.OBSMOD.EQ.'DEF')THEN
            OBSMOD = 'TAR'
	ELSE IF (OBSMOD.EQ.'PHO'.OR.OBSMOD.EQ.'SPY') THEN
            OBSMOD = 'ACC'
         ELSE IF (OBSMOD.EQ.'RAP')THEN
            OBSMOD = 'DIR'
	ENDIF
C
C If obsmode is target acquisition we are done
C
        IF(OBSMOD.EQ.'TAR')GO TO 1000
C
C Read input data sizes
C
        DO 30 I=1,6
            IF(IDS(I).NE.-1)THEN
                CALL UIMGID(IDS(I),DTYPE,NAXIS,DIMEN,ISTATS(1))
                NDIM(I)=NAXIS
                NS(I)=DIMEN(1)
                CALL UHDGSI(IDS(I),'GCOUNT',GCOUNT(I),ISTATS(2))
                IF((ISTATS(1).NE.0).OR.(ISTATS(2).NE.0))THEN
                        CALL UIMCLO(IDS(I),ISTAT)
                        IDS(I)=-1
                        ERRORS(I)=1
                        CONTXT='Error reading input file '//NAME
                        CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                ENDIF
            ENDIF
30      CONTINUE
C
C If d0h or x0h file had error we can not continue
C
        IF((ERRORS(3).EQ.1).OR.(ERRORS(5).EQ.1))THEN
            CONTXT='ERROR: cannot continue due to error reading'//
     *          ' d0h or x0h file'
            GO TO 2000
        ENDIF
        IF(GCOUNT(3).LT.1)THEN
            CONTXT='ERROR: d0h file is empty' 
            GO TO 2000
        ENDIF
C
C Don't use any file where error occurred
C
        DO 40 I=1,6
            IF(ERRORS(I).NE.0)THEN
                IF(IDS(I).NE.-1)CALL UIMCLO(IDS(I),ISTAT)
                IDS(I)=-1
             ENDIF
40      CONTINUE
C
C Check for valid data types
C
        DO 50 I=1,6
            IF(( (NDIM(I).NE.1) .OR. (NS(I).NE.SIZES(I)) )
     *                  .AND. (IDS(I).NE.-1) )THEN
                CONTXT='ERROR: invalid '//TYPES(I) //' file'
                GO TO 2000
           ENDIF
50      CONTINUE
C
C Check file constency .x0h .xqh .d0h .q0h should have the
C same number of groups
C
        IF(GCOUNT(5).NE.GCOUNT(3))THEN
           CONTXT='ERROR: inconsistent gcount between x0h and '//
     $				'd0h files'
           GO TO 2000
        ENDIF
        IF((GCOUNT(4).NE.GCOUNT(3)).AND.(IDS(4).NE.-1))THEN
           CONTXT='ERROR: incorrect group count for q0h file'
           CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CALL ZMSPUT('       q0h file will not be used',
     $				STDOUT+STDERR,0,ISTAT)
           CALL UIMCLO(IDS(4),ISTAT)
           IDS(4)=-1
        ENDIF
        IF((GCOUNT(6).NE.GCOUNT(3)).AND.(IDS(6).NE.-1))THEN
           CONTXT='ERROR: incorrect group count for xqh file'
           CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CALL ZMSPUT('       xqh file will not be used',
     $				STDOUT+STDERR,0,ISTAT)
           CALL UIMCLO(IDS(6),ISTAT)
           IDS(6)=-1
        ENDIF
C
C get detector, grating mode spectral cal lamp, aperture
C
        CALL UHDGSI(IDS(3),'DETECTOR',DET,ISTAT)
        IF(ISTAT.NE.0)THEN
             CONTXT='ERROR -- unable to get DETECTOR from d0h header'
             GO TO 2000
        ENDIF
        IF((DET.NE.1).AND.(DET.NE.2))THEN
                CONTXT='ERROR -- invalid detector number in d0h header'
                GO TO 2000
        ENDIF
        CALL UHDGST(IDS(3),'GRATING',GRAT,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR -- unable to get GRATING from d0h header'
                GO TO 2000
        ENDIF
        CALL UHDGSI(IDS(3),'SCLAMP',SCLAMP,ISTAT)
        IF (ISTAT.NE.0) THEN
                CONTXT='ERROR -- unable to get spectral '//
     $		'cal lamp from d0h header'
                GO TO 2000
        ENDIF
        CALL UHDGST(IDS(3),'APERTURE',APER,ISTAT)
        IF (ISTAT.NE.0) THEN
                CONTXT='ERROR -- unable to get APERTURE'//
     $			 ' from d0h header'
                GO TO 2000
        ENDIF
C
C DONE
C
1000    ISTAT=0
        GO TO 3000
C
C No science data - don't return an error
C
1500    ISTAT = -1
        GO TO 3000
C
C ERROR
C
2000    CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
3000    RETURN
        END
