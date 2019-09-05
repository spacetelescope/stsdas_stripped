        SUBROUTINE YCLOPN(ROOT,ISTAT)
*
*  Module number:
*
*  Module name: YCLOPN
*
*  Keyphrase:
*  ----------
*       open input data files
*
*  Description:
*  ------------
*       This routine opens the input data files (except the .d0h
*       file which is already open.
*
*  FORTRAN name: yclopn.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.d1h          I        FOS trailer file
*       <rootname>.ulh          I        FOS UDL file
*       <rootname>.q0h          I        data quality file
*       <rootname>.q1h          I        trailer data quality
*       <rootname>.shh          I        standard header packet file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput,yfname
*  SDAS:
*       uimopn, uhdgsr, uimgid
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jul 89  D. Lindler      Designed and coded
*	1.1	Mar 91	S. Hulbert	If UDL is not present turn off DEFDDT
*	1.2	Sep 91	S. Hulbert	Change when to open UDL              
*	1.3	Nov 91	S. Hulbert	Nevermind, just call error if UDL
*					needed but not found
*       1.4     Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
* inputs:
*       ROOT - root name of the file
*
* outputs:
*       ISTAT - error status
*
        CHARACTER*64 ROOT
        INTEGER ISTAT
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
      INTEGER RDONLY
      PARAMETER (RDONLY = 1)
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
C
C Common block containing input/output file descriptions
C
C       IDS - file id numbers
C       GCOUNT - group counts
C       NAXIS - naxis
C       NAXIS1 - first dimensions
C       NAXIS2 - second dimensions
C       FILL - Fill values
C
        INTEGER IDS(30),NAXIS(30),NAXIS1(30),NAXIS2(30),GCOUNT(30)
        REAL FILL(30)
        COMMON /IOCOM/IDS,GCOUNT,NAXIS,NAXIS1,NAXIS2,FILL
C
C Local variables
C
        CHARACTER*3 QUAL(6)
        INTEGER DIMEN(8),DTYPE,I
        CHARACTER*64 NAME
        CHARACTER*80 CONTXT
        DATA QUAL/'d0h','q0h','d1h','q1h','ulh','shh'/
C---------------------------------------------------------------------------
C
C loop on input files
C
      DO 100 I=2,6
C
C does trailer exist
C
        IF(((I.EQ.3).OR.(I.EQ.4)).AND.(.NOT.TRAILR))GO TO 100
C
C do we need to open the udl?
C
	IF((I.EQ.5).AND.(.NOT.DEFDDT))GO TO 100
C
C Open the file
C
        CALL YFNAME(ROOT,QUAL(I),1,0,NAME)
        CALL UIMOPN(NAME,RDONLY,IDS(I),ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR opening data file '//NAME
            GO TO 999
        ENDIF
C
C get dimensions of the file
C
        CALL UIMGID(IDS(I),DTYPE,NAXIS(I),DIMEN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading file '//NAME
                GO TO 999
        ENDIF
        IF(NAXIS(I).GT.2)THEN
                CONTXT='INVALID naxis for file '//NAME
                GOTO 999
        ENDIF
        NAXIS1(I)=DIMEN(1)
        NAXIS2(I)=DIMEN(2)
C
C get number of groups in the file
C
        CALL UHDGSI(IDS(I),'GCOUNT',GCOUNT(I),ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting GCOUNT from file '//NAME
                GO TO 999
        ENDIF
100    CONTINUE
C
C check files for consistency ------------------------------------------
C
        DO 200 I=2,4
            IF((IDS(I).GT.0).AND.(GCOUNT(I).NE.GCOUNT(1)))THEN
                CONTXT='ERROR -- inconsistent number of groups in '//
     *                  '.d0h  and .'//QUAL(I)//' files'
                GO TO 999
            ENDIF
200     CONTINUE
        IF ((IDS(5).GT.0) .AND. ((NAXIS(5) .GT. 1) .OR. 
     $        	        (NAXIS1(5) .NE. 965))) THEN
                CONTXT='Invalid dimensions for .ulh file'
                GO TO 999
        ENDIF
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
