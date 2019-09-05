        SUBROUTINE GMPOFF(FRAME,NAME1,NAME2,XOFFS,YOFFS,NSPEC,
     $		DATA,ERR,EPS,N,ISTAT)
*
*  Module number:
*
*  Module name: GMPOFF
*
*  Keyphrase:
*  ----------
*       Perform FOS GIMP correction
*  Description:
*  ------------
*       This routine calculates and applies the GIMP correction.
*       Two possible offset choices:
*	  1) model calculation
*	  2) input table of offsets
*
*  FORTRAN name: GMPOFF.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       ccs7                    I       gimp corrections scale factors
*	offs			I	table of offsets (optional)

*  Subroutines Called:
*  -------------------
*  CDBS:
*       yrccs7, yroffs
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 91	    S. Hulbert      Designed and coded
*     1.1       Jun 91      S. Hulbert      Added y-offset
*     1.2       Mar 93      J. Eisenhamer   Ripped from CALFOS just for gimp.
*     1.4       Dec 2000    M. De La Pena   Added KYDPLY to CONFG1 common.
*-------------------------------------------------------------------------------
*
* INPUTS:
*       frame - frame number
*       name1 - name of the CCS7 reference table
*       name2 - name of the OFFS_TAB optional reference table
*	nspec - number of spectra per frame
*       n - number of points to calibrate
*       fill - flag value for fill data
*
* INPUT/OUTPUT:
*	xoffs - xoffsets array
*       data - data array
*       err - error array
*       eps - data quality vector
*
* OUTPUT:
*       istat - error status
*
*----------------------------------------------------------------------------
        CHARACTER*64 NAME1, NAME2
        INTEGER N, FRAME, ISTAT, NSPEC
        REAL DATA(N), ERR(N), EPS(N), XOFFS(NSPEC), YOFFS(NSPEC)
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
        CHARACTER*5 DET
        CHARACTER*3 FGWAID,APERID,YTYPE(3)
        CHARACTER*1 POLID
        INTEGER FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME
        LOGICAL HEADER,TRAILR,DEFDDT
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
C LOCAL VARIABLES ------------------------------------------------------
C
        INTEGER I
        CHARACTER*80 CONTXT
        CHARACTER*3 CHAR3
        REAL XSCALE,YSCALE
        INTEGER IXOFF, DATA0, ERR0, EPS0, DATA1, ERR1, EPS1
        LOGICAL COMPUT
        REAL TEMP1, TEMP2
        INTEGER OFFSET, ISTATS(20)
C
C
C-----------------------------------------------------------------------
C read scale factors and/or offsets if first call
C determine which table is needed
C
        IF(FRAME.EQ.1)THEN
C
C we will want to watch for the last frame
C
            LASTFR= GCOUNT(1)
C
C buffer memory to hold the x offsets (this is all we are using now)
C
            CALL UDMGET (NSPEC, TYINT, IXOFF, ISTATS(1))
            IF (ISTATS(1).NE.0) THEN
                     CONTXT = 'Error allocating dyanmic memory'
                     GO TO 999
            ENDIF
C
C if multiple readouts set up buffers to hold previous
C undegimped and degimped frame
C
            IF (NREAD .GT. 1) THEN
C
C buffers for last readout not yet degimped
C
        	CALL UDMGET (N, TYREAL, DATA0, ISTATS(1))
        	CALL UDMGET (N, TYREAL, ERR0, ISTATS(2))
        	CALL UDMGET (N, TYREAL, EPS0, ISTATS(3))
C
C buffers for last readout degimped
C
        	CALL UDMGET (N, TYREAL, DATA1, ISTATS(4))
        	CALL UDMGET (N, TYREAL, ERR1, ISTATS(5))
        	CALL UDMGET (N, TYREAL, EPS1, ISTATS(6))
C
                DO 155 I = 1, 6
                    IF (ISTATS(I).NE.0) THEN
                        CONTXT = 'Error allocating dyanmic memory'
                        GO TO 999
                    ENDIF
155        	CONTINUE
C
                DO 145 I = 1, N
        	    MEMR(DATA0+I-1) = 0.
        	    MEMR(ERR0+I-1) = 0.
        	    MEMR(EPS0+I-1) = 0.
145                CONTINUE
                DO 146 I = 1, N
        	    MEMR(DATA1+I-1) = 0.
        	    MEMR(ERR1+I-1) = 0.
        	    MEMR(EPS1+I-1) = 0.
146                CONTINUE
            ENDIF
C
C check to see if an table of offsets is available
C 'compute' is used to flag this
C
            CHAR3 = NAME2(1:3)
            IF ((CHAR3 .NE. 'n/a') .AND. (CHAR3 .NE. 'N/A')) THEN
        	COMPUT = .FALSE.
                CONTXT = 'GIMP correction done with offsets from '//
     $        	    'OFFS_TAB table '//NAME2
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
            ELSE
C
C else, read the scale factor ...
C
        	COMPUT = .TRUE.
                CONTXT = 'GIMP corrections calculated from model'
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                CALL YRCCS7(NAME1,DET,XSCALE,YSCALE,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                WRITE(CONTXT,99)XSCALE
99              FORMAT('GIMP x scale factor = ',E16.8)
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                WRITE(CONTXT,199)YSCALE
199             FORMAT('GIMP y scale factor = ',E16.8)
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                CONTXT = 'WARNING: GIMP y offsets computed but'//
     $                   ' not applied'
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C write out heading for the gimp magnitudes.
C
                CONTXT =
     *          '     XOFF       YOFF        B(V1)      B(V2)'//
     *          '      B(V3)'
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C ... and get orbital parameters from shp
C
                CALL YGTORB(ISTAT)
                IF(ISTAT.NE.0)GO TO 999
            ENDIF
        ENDIF
C
C if we're computing, go do it
C
        IF (COMPUT) THEN 
            CALL GMPGMP(XSCALE,YSCALE,XOFFS,YOFFS,NSPEC,ISTAT)
            IF (ISTAT.NE.0) GO TO 999
        ELSE
C
C else, read the offsets
C
            CALL YRDOFF(FRAME,NAME2,LASTFR,XOFFS,YOFFS,NSPEC,ISTAT)
            IF (ISTAT.NE.0) GO TO 999
        ENDIF
C
C convert the x-offsets to integer number of x-steps
C
        DO 100 I = 1, NSPEC
            MEMI(IXOFF+I-1) = NINT(NXSTEP*XOFFS(I))
100        CONTINUE
C
C unravel non-destructive readouts
C
        IF (NREAD .GT. 1) THEN
C
C scale the data by the number of readouts and subtract from the
C previous readout. 
C square the errors, too.
C save the subtracted values as the 'previous' frame for use
C with the next frame 
C
            DO 190 I = 1, N
        	TEMP1 = DATA(I) * FRAME
        	TEMP2 = (ERR(I) * FRAME)*(ERR(I) * FRAME)
        	DATA(I) = TEMP1 - MEMR(DATA0+I-1)
        	IF (DATA(I) .LT. 0.) THEN
        	    DATA(I) = 0.
        	    TEMP1 = MEMR(DATA0+I-1)
        	ENDIF
        	ERR(I) = TEMP2 - MEMR(ERR0+I-1)
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
C
        DO 200 I = 1, NSPEC
            OFFSET = MEMI(IXOFF+I-1)
            IF (OFFSET .NE. 0) THEN
        	IF (OFFSET .LT. 0) THEN
        	    GDFRST = (I-1) * N / NSPEC + 1 - OFFSET
        	    GDLST = I * N / NSPEC
        	    DELTA = 1
C
C set right edge count in offset space
C
        	    EDGE2 = I * N / NSPEC 
        	    EDGE1 =  I * N / NSPEC + OFFSET + 1
                  ELSE 
C
C fill this backwards
C
        	    GDFRST = I * N / NSPEC - OFFSET
        	    GDLST = (I-1) * N / NSPEC + 1
        	    DELTA = -1
C
C set left edge
C
        	    EDGE1 = 1
        	    EDGE2 = OFFSET
        	ENDIF
C
C offset
C
        	DO 210 J = GDFRST,GDLST,DELTA
        	        DATA(J+OFFSET) = DATA(J)
        	        ERR(J+OFFSET) = ERR(J)
        	        EPS(J+OFFSET) = EPS(J)
210        	CONTINUE
C
C now deal with the edges; fill with 0 
C
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
C
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
C
C clean up if all done
C
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
                    CONTXT = 'Error deallocating dyanmic memory'
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
