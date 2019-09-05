C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLWRT(ROOT,FRAME,PFLAGS,DATA,IDNUM,DTYPE,ISTAT)
*
*  Module number:
*
*  Module name: YCLWRT
*
*  Keyphrase:
*  ----------
*       Write CALFOS data files
*
*  Description:
*  ------------
*       This routine write the output data files produced by CALFOS
*
*  FORTRAN name: yclwrt
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.<Qual>       O       output data file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput, yogpar, yfname, yuhead
*  SDAS:
*       uuimcp, uuopgr, uip1r
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jul 89  D. Lindler      Designed and coded
*		Sep 90	S. Hulbert	Changed datamin/datamax calculation
*	1.2	May 91	S. Hulbert	Added 'OSO' dtype
*	1.2.1	Sep 91	S. Hulbert	Fix error message for special data sets
*	1.2.2   Jan 92	S. Hulbert	Bug Fix--don't update GIMP-related
*					group parameters for c4h file
*	1.2.3	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1),
*					and declare local variable IGIMP.
*	1.3	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*       1.4     Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* INPUTS:
*       root - root name of the files
*       frame - frame number
*       pflags - processing flags
*       data - data array
*       idnum - output file id number
*       dtype - data type
*               'ALL','OBJ','SKY','BCK', 'RBK', 'RAP', 'TIM', or 'POL'
*		'OSO' = 'OBJ' + 'SKY'
*
* OUTPUTS:
*       istat - error status
*----------------------------------------------------------------------
        CHARACTER*64 ROOT
        INTEGER FRAME,IDNUM,ISTAT
        CHARACTER*8 PFLAGS(*)
        REAL DATA(*)
        CHARACTER*3 DTYPE
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
      INTEGER TYREAL
      PARAMETER (TYREAL = 6)
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
        REAL DMIN,DMAX
        INTEGER IS,IY,IPOS,IGIMP,GLAST(30)
        CHARACTER*64 NAME
        CHARACTER*80 CONTXT
        CHARACTER*3 QUAL(10),QF,YTYPE1
        DATA QUAL/'c0h','c1h','c2h','cqh','c3h','c4h','c5h','c6h',
     *            'c7h','c8h'/
C
C----------------------------------------------------------------------------
C
C Check GCOUNT; if set to zero we don't need this output file so
C don't create it.
C
        IF(GCOUNT(IDNUM).EQ.0)RETURN
C
        QF=QUAL(IDNUM-10)
C			  ---> index of output file
C Create data set if first call
C
        IF(FRAME.EQ.1)THEN
                CALL YFNAME(ROOT,QF,1,GCOUNT(IDNUM),NAME)
C
C open using .d0h file as the template
C
                CALL UUIMCP(NAME,TYREAL,1,NAXIS1(IDNUM),IDS(1),
     *                          IDS(IDNUM),ISTAT)
                IF(ISTAT.NE.0)THEN
                        CONTXT='ERROR opening '//QF//
     *                          ' file using d0h file as template'
                        GO TO 999
                ENDIF
C
C Update header file
C
                CALL YUHEAD(IDS(IDNUM),IDNUM,PFLAGS,ISTAT)
                IF(ISTAT.NE.0)GO TO 1000
C
C now use GCOUNT(IDNUM) as group counter
C and use GLAST(IDNUM) to keep track of total number of groups
C
        	GLAST(IDNUM)=GCOUNT(IDNUM)
        	GCOUNT(IDNUM)=1
        ENDIF
C-------------------------------------------------------------------------
        IF((DTYPE.EQ.'ALL').OR.(DTYPE.EQ.'OSO').OR.(DTYPE.EQ.'OBJ')
     $        	.OR.(DTYPE.EQ.'SKY').OR.(DTYPE.EQ.'BCK'))THEN
C
C Loop on slices and ysteps
C
            DO 100 IS=1,SLICES
                DO 50 IY=1,YSTEPS
C
C DO we write this ystep
C
                    IF(YSTEPS.GT.3)THEN
                            YTYPE1='OBJ'
                        ELSE
                            YTYPE1=YTYPE(IY)
                    ENDIF
                    IF((DTYPE.EQ.'ALL').OR.(DTYPE.EQ.YTYPE1).OR.
     $                  ((DTYPE.EQ.'OSO').AND.((YTYPE1.EQ.'OBJ').OR.
     $                  (YTYPE1.EQ.'SKY'))))THEN
C
C move to correct output group
C
                        IF(GCOUNT(IDNUM).GT.1)THEN
        	           CALL UUMNMX(IDS(IDNUM),DMIN,DMAX,ISTAT)
                           IF(ISTAT.NE.0)THEN
                             CONTXT='ERROR calculating DATAMIN/'//
     $        	        	        'DATAMAX in '//QF//' file'
        	             GO TO 999
                           ENDIF
                           CALL UUOPGR(IDS(IDNUM),GCOUNT(IDNUM),DMIN,
     *                             DMAX,IDS(1),ISTAT)
                           IF(ISTAT.NE.0)THEN
                             CONTXT='ERROR Changing group within '//
     *                                  QF//' file'
                             GO TO 999
                           ENDIF
                         ENDIF
C
C Find starting position in the data array & write data to the file
C
                        IPOS=(IS-1)*NX*YSTEPS + (IY-1)*NX + 1
                        CALL UIPL1R(IDS(IDNUM),DATA(IPOS),ISTAT)
                        IF(ISTAT.NE.0)THEN
                            CONTXT='ERROR writing to the '//QF//' file'
                            GO TO 999
                        ENDIF
C
C Update group parameter block
C If this is the c4h file (idnum=16) set igimp to 0, so that the
C x_offset, y_offset, sct_val, and sct_err group parameters are NOT updated
C
			IF (IDNUM.NE.16) THEN
        	            IGIMP = (IS - 1) * YSTEPS + IY
			ELSE
			    IGIMP = 0
			ENDIF
                        CALL YOGPAR(IDS(IDNUM),IY,YTYPE1,IGIMP,ISTAT)
                        IF(ISTAT.NE.0)THEN
                             CONTXT='ERROR updating group parameter'
     *                                 //' in '//QF//' file'
                             GO TO 999
                        ENDIF
C
                        GCOUNT(IDNUM)=GCOUNT(IDNUM)+1
                    ENDIF
50              CONTINUE
100         CONTINUE
C
C put datamin/datamax into last group
C
            IF (GCOUNT(IDNUM) .GT. GLAST(IDNUM)) THEN
                CALL UUMNMX(IDS(IDNUM),DMIN,DMAX,ISTAT)
                IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR calculating DATAMIN/'//
     $        	        	'DATAMAX in '//QF//' file'
                    GO TO 999
                ENDIF
            ENDIF
C
        ELSE
C
C SPECIAL DATA SETS ---------------------------------------------------------
C
C move to correct group
C
            IF(FRAME.GT.1)THEN
               CALL UUMNMX(IDS(IDNUM),DMIN,DMAX,ISTAT)
               IF(ISTAT.NE.0)THEN
                  CONTXT='ERROR calculating DATAMIN/'//
     $        	        	'DATAMAX in '//QF//' file'
        	  GO TO 999
               ENDIF
               CALL UUOPGR(IDS(IDNUM),FRAME,DMIN,DMAX,IDS(1),ISTAT)
               IF(ISTAT.NE.0)THEN
                  CONTXT='ERROR moving to correct group in '//
     $				QF//' file'
                  GO TO 999
               ENDIF
            ENDIF
C
C write the data
C
            CALL UIPL1R(IDS(IDNUM),DATA,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR Writing the the '//QF//' file'
                GO TO 999
            ENDIF
C
C set group parameter values
C
            IY = 1
            IGIMP = 0
            IF((DTYPE.EQ.'POL').AND.(FRAME.GT.14))IY = 2
            IF(DTYPE.EQ.'RBK')THEN
                CALL YOGPAR(IDS(IDNUM),IY,'BCK',IGIMP,ISTAT)
              ELSE
                CALL YOGPAR(IDS(IDNUM),IY,'OBJ',IGIMP,ISTAT)
            ENDIF
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR Updating group parameters in '//
     $				QF//' file'
                GO TO 999
            ENDIF
C
C put datamin/datamax into last group
C
            IF (FRAME .EQ. GLAST(IDNUM)) THEN
                CALL UUMNMX(IDS(IDNUM),DMIN,DMAX,ISTAT)
                IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR calculating DATAMIN/'//
     $        	        	'DATAMAX in '//QF//' file'
                    GO TO 999
                ENDIF
            ENDIF
        ENDIF
C
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
