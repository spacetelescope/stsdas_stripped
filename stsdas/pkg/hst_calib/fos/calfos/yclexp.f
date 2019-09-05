        SUBROUTINE YCLEXP(FRAME,NAME,EPS,REJECT,EPSREJ,DATA,ERR,
     *                    PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: yclexp
*
*  Keyphrase:
*  ----------
*       convert to count rates
*
*  Description:
*  ------------
*       This routine divides by the exposure time for each data point
*       with a correction for disabled diodes.  If DEFDDTBL is True in
*       the input .d0h file, disabled diodes are taken from the UDL
*       file, other wise they are taken from a reference file.
*
*  FORTRAN name: yclexp.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       name                    I       Name of the dead diode reference file
*       <rootname>.ulh          I       unique data log file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ysmput, yrdddt, ymissd
*  SDAS:
*       uigs1r
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*    1          JUL 89  D. LINDLER      DESIGNED AND CODED
*        	Sep 90  S. Hulbert	update BUNITS
*    1.1	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1)
*    2		Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*    2.1        Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* inputs:
*       frame - frame number
*       name - name of disabled diode table file
*       eps - quality array
*       reject - reject vector
*       epsrej - quality vector for reject
*
* input/output:
*       data - data array
*       err - error array
*
* outputs:
*	pedgre - DDTHFILE PEDIGREE keyword string
*	descrp - DDTHFILE DESCRIP  keyword string
*       istat - error status
*------------------------------------------------------------------------------
        INTEGER FRAME,ISTAT
        REAL EPS(*),REJECT(*),EPSREJ(*),DATA(*),ERR(*)
        CHARACTER*64 NAME
	CHARACTER*68 PEDGRE, DESCRP
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
C brigtness units for output files
C
        CHARACTER * 20 BUNITS(10)
        COMMON /BUNITS/ BUNITS
C
C local variables
C
        REAL FILLD
C                                    --->Fill flag
        LOGICAL FOUND
        INTEGER N,ND,I,NREPS,CLEARS,IREAD,NREJ,NR
        CHARACTER*80 CONTXT
        REAL DDT(512)
C                                    --->Disabled diode table
        REAL SCALE,EXPO,GOOD
C
C Epsilon values
C   EPSBAD = 400   NMISSED = NREPS
C   EPSH = 50      0.5 < MISSED/NREPS
        REAL EPSBAD,EPSH
        DATA EPSBAD/400.0/,EPSH/50.0/
C------------------------------------------------------------------------------
C
C On frame 1 read the reference data
C
        IF(FRAME.EQ.1)THEN
            FILLD = FILL(1)
C
C If DEFDDT=T, read the dead diode list from the .ulh (UDL) file
C
            IF(DEFDDT)THEN
                CALL UIGS1R(IDS(5),322+FCHNL,833,DDT,ISTAT)
                IF(ISTAT.NE.0)THEN
                        CONTXT='ERROR reading from .ulh file '
                        GO TO 999
                ENDIF
                CONTXT='Conversion to count rates using ULH disabled'//
     *                  ' diode table'
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                DO 1 I=1,NCHNLS
                    IF((DDT(I).GT.32767.0).OR.(DDT(I).LT.0.0))THEN
                         DDT(I)=0.0
C				    ---> zero = disabled
                      ELSE
                         DDT(I)=1.0
C				    ---> one  = active  
                    ENDIF
1                CONTINUE
C
C Otherwise, read the dead diode list from the DDTHFILE reference file
C
             ELSE
                CALL YRDDDT(NAME,DDT,PEDGRE,DESCRP,ISTAT)
                IF(ISTAT.NE.0)GO TO 1000
                CONTXT='Conversion to count rates using DDTFIL '//NAME
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C If the reference file contains dummy data, set all diodes to active
C
		IF (PEDGRE(1:5).EQ.'DUMMY') THEN
		    CONTXT='WARNING: PEDIGREE = DUMMY for '//NAME
		    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		    CONTXT='         Correction for dead diodes will '
     *                     //'be skipped'
		    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		    DO 10 I = 1, 512
10		       DDT(I) = 1.0
		END IF
            ENDIF
C
            EXPO = LVTIME*7.8125E-6
C                                    --->exposure per repitition (seconds)
        ENDIF
C-------------------- End of frame 1 only processing ---------------------------
C
C compute number of reps
C
        CLEARS = (FRAME-1)/NREAD
C                                    --->number of clears so far
        IREAD = FRAME - CLEARS*NREAD
C                                    --->readout number
        NREPS = IREAD * OVERSN * NPAT * INTS
C
C Change rejects with fill data to 0 and print warning
C
        IF(TRAILR)THEN
            FOUND = .FALSE.
            NREJ = NXSTEP * SLICES * YSTEPS * OVERSN
            DO 20 I=1,NREJ
                IF(EPSREJ(I).GT.0.0)THEN
                        FOUND = .TRUE.
                        REJECT(I)=0
                ENDIF
20          CONTINUE
            IF(FOUND)THEN
                WRITE(CONTXT,99)FRAME
99              FORMAT('WARNING: fill data in reject array for frame',
     *                  I5,' assumed 0')
                CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            ENDIF
        ENDIF
C
C compute number of misses; reuse EPSREJ array to store misses
C
        ND = NCHNLS + OVERSN - 1
        NR = INTS * NPAT * IREAD
        CALL YMISSD(NXSTEP,ND,NCHNLS,YSTEPS,SLICES,OVERSN,NR,REJECT,
     *                  DDT,TRAILR,EPSREJ)
C
C divide data by exposure time, correcting for dead diodes and misses
C
        N = NX * YSTEPS * SLICES
        DO 500 I=1,N
            IF(EPS(I).GE.FILLD)THEN
                    ERR(I)=0.0
                    DATA(I)=0.0
                ELSE
                    IF(EPSREJ(I).GT.NREPS)THEN
                        CONTXT='ERROR: exposure computation; MISSED'//
     *                          ' is greater then NREPS'
                    ELSE
                        IF(EPSREJ(I).LT.NREPS)THEN
                                GOOD = NREPS - EPSREJ(I)
                                SCALE = EXPO*GOOD
                                DATA(I) = DATA(I)/SCALE
                                ERR(I) = ERR(I)/SCALE
                                IF (((EPSREJ(I)/NREPS).GE.0.5).AND.
     *                                (EPS(I).LT.EPSH))EPS(I)=EPSH
                           ELSE
                                DATA(I) = 0.0
                                ERR(I) = 0.0
                                IF(EPS(I).LT.EPSBAD)EPS(I)=EPSBAD
                        ENDIF
                    ENDIF
             ENDIF
500     CONTINUE
C
C update brightness units
C
        DO 101 I = 2, 10
            IF (I .EQ. 4) GO TO 101
            BUNITS(I) = 'COUNTS/S'
101     CONTINUE
C
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
