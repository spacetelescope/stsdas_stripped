C 	Copyright restrictions apply - see stsdas$copyright.stsdas, STECF CR 
C 
        SUBROUTINE YCLOFF(FRAME,NAME1,NAME2,XOFFS,YOFFS,NSPEC,
     $		DATA,ERR,EPS,N,PEDGR1,PEDGR2,DESCR1,DESCR2,GRNDMD,
     $          PFLAGS,ROOTO,ISTAT)
c*
c*  Module number:
c*
c*  Module name: YCLOFF
c*
c*  Keyphrase:
c*  ----------
c*       Perform FOS x-shift corrections
c*  Description:
c*  ------------
c*       This routine calculates and applies corrections to the X-location
c*       that stem from various sources (GIMP, wrong on-board GIMP, YBASE...)
c*
c*  FORTRAN name: YCLOFF.FOR
c*
c*  Keywords of accessed files and tables:
c*  --------------------------------------
c*  Name                         I/O     Description / Comments
c*       ccs7                    I       gimp corrections scale factors
c*       offparms       	        I	offset parameters
c
c*  Subroutines Called:
c*  -------------------
c*  CDBS:
c*       yrccs7, yrdoff, ygtorb, ygimp
c*
c*  STSDAS:
c*	udmget, udmfre, uhdgsb
c*
c*  History:
c*  --------
c*  Version      Date        Author          Description
c*       1       May 91	    S. Hulbert      Designed and coded
c*     1.1       Jun 91      S. Hulbert      Added y-offset
c*     1.1.2	Apr 93	    H. Bushouse	    Declare local variables
c*     1.2	Apr 93	    H. Bushouse     Corrected zero countrate roundoff
c*                                           errors
c*     1.3	May 93	    H. Bushouse	    Warn if onboard GIMP corr performed
c*	2	Mar 94	    H. Bushouse	    Mods to handle PEDIGREE keywords
c*     2.1      Mar 97      M. De La Pena   Added KYDEPLOY to CONFG1
c*     ////////////////////////////////////////////////////////////////////
c*           Start of Post Operational Archive versions (poa_calfos)
c*     ////////////////////////////////////////////////////////////////////
c*     1.0      May 00      M. Rosa         Rewrite, include additional POA 
c*                                          corrections
c*              Jun 00      M. Rosa         Adapted POA version of XOFF calc.
c*                                          activate PERFORM/OMIT switch 
c*     1.1      Oct 00      A. Alexov       Added ascii file for header output 
c*     1.2.1    Oct 01      A. Alexov       CONTXT blank must have one space
c*                                          character for Dec Alpha compilation
c*-------------------------------------------------------------------------------
c*
c* INPUTS:
c*       frame - frame number (this is actually the group number)
c*       name1 - name of the CCS7 reference table
c*       name2 - name of the OFFS_TAB optional reference table
c*       nspec - number of spectra per frame
c*       n - number of points to calibrate
c*       fill - flag value for fill data
c*
c* INPUT/OUTPUT:
c*       xoffs - xoffsets array
c*       data - data array
c*       err - error array
c*       eps - data quality vector
c*
c* OUTPUT:
c*	pedgr1 - CCS7 PEDIGREE keyword string
c*	pedgr2 - OFFS_TAB PEDIGREE keyword string
c*	descr1 - CCS7 DESCRIP  keyword string
c*	descr2 - OFFS_TAB DESCRIP  keyword string
c*       istat - error status
c*
c*----------------------------------------------------------------------------
        IMPLICIT NONE

        CHARACTER*8 PFLAGS(15)
        CHARACTER*64 NAME1, NAME2
        CHARACTER*64 ROOTO, NAME, FILE
	CHARACTER*68 PEDGR1,PEDGR2,DESCR1,DESCR2
        INTEGER N, FRAME, ISTAT, NSPEC, II
        REAL DATA(N), ERR(N), EPS(N), XOFFS(NSPEC), YOFFS(NSPEC)
C------------------------------------------------------------------------------
C Get IRAF MEM common into main program.
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
      INTEGER TYREAL
      PARAMETER (TYREAL = 6)
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
        CHARACTER*18 GRNDMD
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
C gimp epsilon
C
        REAL EPSGMP
        PARAMETER (EPSGMP = 700)

C
C the POA header keys get placed into an ascii txt output file
C
        CHARACTER*64 POA_TXT   
        COMMON /POA_T/ POA_TXT
        INTEGER GLOBAL_COUNT
        COMMON /GCOUNT/ GLOBAL_COUNT
C
C LOCAL VARIABLES ------------------------------------------------------
C
        INTEGER I, J, LASTFR, NOVS, II
        CHARACTER*80 CONTXT
c        CHARACTER*3 CHAR3
        REAL XSCALE,YSCALE
	REAL EXPOMAX
	INTEGER CLEARS, IREAD, NREPS
        INTEGER IXOFF, DATA0, ERR0, EPS0, DATA1, ERR1, EPS1
        INTEGER DATAOVS,ERROVS,EPSOVS
	INTEGER GDFRST, GDLST, DELTA, EDGE1, EDGE2
        REAL TEMP1, TEMP2
        INTEGER OFFSET, ISTATS(20)
	LOGICAL OBGIMP, CPOA
        LOGICAL FILEEX
C
C-----------------------------------------------------------------------
        CPOA = .TRUE.
        NOVS = 4*N
C read scale factors and/or offsets if first call
C determine which table is needed
        IF(FRAME.EQ.1)THEN
C we will want to watch for the last frame
            LASTFR= GCOUNT(1)
C buffer memory to hold the x offsets 
            CALL UDMGET (NSPEC, TYINT, IXOFF, ISTATS(1))
            IF (ISTATS(1).NE.0) THEN
                     CONTXT = 'Error allocating dynamic memory'
                     GO TO 999
            ENDIF

C create new ascii output file for header storage
C first, create the name, using ROOTO + .poa
C FIND END OF ROOTO (FIRST BLANK)
C
4       DO 10 II=1,64
10              IF( ROOTO(II:II) .EQ. ' ') GO TO 20
20      FILE=ROOTO
        WRITE(NAME,99)FILE(1:II-1),'.poa'
99      FORMAT(A,A4)
        POA_TXT=NAME
C check if the output txt file already exists, then open/create it
        INQUIRE (FILE=POA_TXT, EXIST=FILEEX)
        IF (FILEEX .EQ. 0) THEN
           OPEN(29,FILE=POA_TXT,STATUS='NEW')
        ELSE
           OPEN(29,FILE=POA_TXT,STATUS='OLD')
           REWIND(29)
        ENDIF
C Initialize the file with some information
        WRITE(29,129) 'Filename: ', POA_TXT
129     FORMAT(A10,A64)
        CLOSE(29)

C
C check whether onboard GIMP was activated
	    CALL UHDGSB (IDS(1),'YFGIMPEN',OBGIMP,ISTAT)
	    IF (ISTAT .NE. 0) THEN
		CONTXT = 'WARNING: keyword YFGIMPEN not found'//
     $                   ' in .d0h; assume pre-onboard-gimp data'
                CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                OBGIMP = .FALSE.
	    ENDIF
	    IF (OBGIMP) THEN
		CONTXT = 'Onboard GIMP was active'
		CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
	    ENDIF
C
C if multiple readouts set up buffers to hold previous
C raw and x-shift corrected frame
            IF (NREAD .GT. 1) THEN
C buffers for last readout not yet corrected
        	CALL UDMGET (N, TYREAL, DATA0, ISTATS(1))
        	CALL UDMGET (N, TYREAL, ERR0, ISTATS(2))
        	CALL UDMGET (N, TYREAL, EPS0, ISTATS(3))
C buffers for last readout x-shift corrected
        	CALL UDMGET (N, TYREAL, DATA1, ISTATS(4))
        	CALL UDMGET (N, TYREAL, ERR1, ISTATS(5))
        	CALL UDMGET (N, TYREAL, EPS1, ISTATS(6))
C buffers for oversampled intermediate step
        	CALL UDMGET (NOVS, TYREAL, DATAOVS, ISTATS(7))
        	CALL UDMGET (NOVS, TYREAL, ERROVS, ISTATS(8))
        	CALL UDMGET (NOVS, TYREAL, EPSOVS, ISTATS(9))
C
                DO 155 I = 1, 9
                    IF (ISTATS(I).NE.0) THEN
                        CONTXT = 'Error allocating dynamic memory'
                        GO TO 999
                    ENDIF
155        	CONTINUE
C
                DO 145 I = 1, N
        	    MEMR(DATA0+I-1) = 0.
        	    MEMR(ERR0+I-1) = 0.
        	    MEMR(EPS0+I-1) = 0.
        	    MEMR(DATA1+I-1) = 0.
        	    MEMR(ERR1+I-1) = 0.
        	    MEMR(EPS1+I-1) = 0.
 145             CONTINUE
                DO 147 I = 1, NOVS
        	    MEMR(DATAOVS+I-1) = 0.
        	    MEMR(ERROVS+I-1) = 0.
        	    MEMR(EPSOVS+I-1) = 0.
147             CONTINUE
            ENDIF
C
C        	COMPUT = .TRUE.
C ... and get orbital parameters from shp
                CALL YGTORB(ISTAT)
                IF(ISTAT.NE.0)GO TO 999
        ENDIF
C------------------- end of frame 1 only processing ---------------------------

       CONTXT = ' '
       CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
       CONTXT = '*** X-OFFSET corrections calculated from POA model ***'
       CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)

C--- here for all groups
       CALL YGIMP(XSCALE,YSCALE,XOFFS,YOFFS,NSPEC,GRNDMD,FRAME,ISTAT)
       IF (ISTAT.NE.0) THEN
          CONTXT = 'Error coming back from YGIMP'
          GO TO 999
       ENDIF

       OPEN(29,FILE=POA_TXT,STATUS='OLD',ACCESS='APPEND')
C
c convert the x-offsets to integer number of x-steps (no oversampling)
c Note:  poaoffx called by ygimp calculates offsets in pixels (=1/4 diode) 
c        hence image data (NXSTEP=1) need to shift their data by only
c        XOFFS/4
        DO 100 I = 1, NSPEC
            MEMI(IXOFF+I-1) = NINT(NXSTEP*XOFFS(I)/4.)
            WRITE(CONTXT,97)  GRNDMD(1:4),XOFFS(I),MEMI(IXOFF+I-1)
 97     FORMAT(' POA x-offset for MODE = ',A4,' is ',F6.3,
     *   ' 1/4 diodes = ',I3,' array pix')
            CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
100    CONTINUE
C
       CLOSE(29)

            CONTXT = '*** End of POA calculations ***'
            CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
            CONTXT = ' '
            CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)

C If OFF_CORR flag is 'OMIT' - do not apply the calculated corrections
       IF(PFLAGS(2).EQ.'PERFORM') THEN

C unravel non-destructive readouts (i.e. DEACCUM)
        IF (NREAD .GT. 1) THEN
C
C scale the data by the number of readouts and subtract from the
C previous readout. Square the errors, too.
C Save the subtracted values as the 'previous' frame for use
C with the next frame 
C
C first calculate maximum exposure time of readout
	    CLEARS = (LASTFR-1) / NREAD
	    IREAD  = LASTFR - CLEARS*NREAD
	    NREPS  = IREAD * INTS * OVERSN * NPAT
	    EXPOMAX = NREPS * (LVTIME*7.8125E-6)
C
            DO 190 I = 1, N
        	TEMP1 = DATA(I) * FRAME
        	TEMP2 = (ERR(I) * FRAME)*(ERR(I) * FRAME)
        	DATA(I) = TEMP1 - MEMR(DATA0+I-1)
        	ERR(I)  = TEMP2 - MEMR(ERR0+I-1)
C correct for roundoff errors by setting the data (and errors) to
C exactly zero if the number of counts is less than 0.5/expomax
		IF (DATA(I) .LT. 0.5/EXPOMAX) THEN
		    DATA(I) = 0.0
		    TEMP1 = MEMR(DATA0+I-1)
		    ERR(I) = 0.0
		    TEMP2 = MEMR(ERR0+I-1)
		ENDIF
        	IF (ERR(I) .LT. 0.) THEN
        	    ERR(I) = 0.
        	    TEMP2 = MEMR(ERR0+I-1)
        	ENDIF
        	MEMR(DATA0+I-1) = TEMP1
        	MEMR(ERR0+I-1) = TEMP2
        	MEMR(EPS0+I-1) = EPS(I)
190            CONTINUE
        ENDIF
C
C set limits but ignore the stuff that falls off the edge
        DO 200 I = 1, NSPEC
            OFFSET = MEMI(IXOFF+I-1)
c            WRITE(CONTXT,98) I, OFFSET
c 98         FORMAT('Applying to group #',I4,' an offset of ',I4,' pix')
c       CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)

            IF (OFFSET .NE. 0) THEN
        	IF (OFFSET .LT. 0) THEN
        	    GDFRST = (I-1) * N / NSPEC + 1 - OFFSET
        	    GDLST = I * N / NSPEC
        	    DELTA = 1
C set right edge count in offset space
        	    EDGE2 = I * N / NSPEC 
        	    EDGE1 =  I * N / NSPEC + OFFSET + 1
                  ELSE 
C fill this backwards
        	    GDFRST = I * N / NSPEC - OFFSET
        	    GDLST = (I-1) * N / NSPEC + 1
        	    DELTA = -1
C set left edge
        	    EDGE1 = 1
        	    EDGE2 = OFFSET
        	ENDIF
C offset
        	DO 210 J = GDFRST,GDLST,DELTA
        	        DATA(J+OFFSET) = DATA(J)
        	        ERR(J+OFFSET) = ERR(J)
        	        EPS(J+OFFSET) = EPS(J)
210        	CONTINUE
C
C now deal with the edges; fill with 0 
        	DO 211 J = EDGE1, EDGE2
        	        DATA(J) = 0.0
        	        ERR(J) = 0.0
        	        EPS(J) = EPSGMP 
211        	CONTINUE
            ENDIF
200        CONTINUE
C
C recombine non-destructive readouts
C add to the previous degimped, scale by the readout, take
C the square root of the error
C check the data quality for previous readouts and update latest
        IF (NREAD .GT. 1) THEN
            DO 191 I = 1, N
        	MEMR(DATA1+I-1) = DATA(I) + MEMR(DATA1+I-1)
        	MEMR(ERR1+I-1) = ERR(I) + MEMR(ERR1+I-1)
        	DATA(I) = MEMR(DATA1+I-1) / FRAME
        	ERR(I) = SQRT(MEMR(ERR1+I-1)) / FRAME
        	IF (EPS(I) .LT. MEMR(EPS1+I-1)) EPS(I) = MEMR(EPS1+I-1)
        	IF (EPS(I) .GE. EPSGMP) THEN
        	    DATA(I) = 0.0
        	    ERR(I) = 0.0
        	ENDIF
        	MEMR(EPS1+I-1) = EPS(I)
191            CONTINUE
         ENDIF
C end of applying the correction

       ENDIF
C
C clean up if all done
12345   CONTINUE
        IF (FRAME .EQ. LASTFR) THEN
            CALL UDMFRE (DATA0, TYREAL, ISTATS(1))
            CALL UDMFRE (ERR0, TYREAL, ISTATS(2))
            CALL UDMFRE (EPS0, TYREAL, ISTATS(3))
            CALL UDMFRE (DATA1, TYREAL, ISTATS(4))
            CALL UDMFRE (ERR1, TYREAL, ISTATS(5))
            CALL UDMFRE (EPS1, TYREAL, ISTATS(6))
            CALL UDMFRE (IXOFF, TYINT, ISTATS(7))
            DO 165 I = 1, 7
                IF (ISTATS(I).NE.0) THEN
                    CONTXT = 'Error deallocating dynamic memory'
                    GO TO 999
                ENDIF
165            CONTINUE
        ENDIF
C
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
