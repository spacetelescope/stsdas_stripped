	SUBROUTINE ZCLBKG(IREAD,CCRB,GRAT,CARPOS,APER,SPORD,DATA,
     *				ET,EPS,EPSET,SKYMDF,SKYMNF,
     *				SKYORD,INTMDF,INTMNF,INTORD,
     *				RATIO,NS,EPSF,FLUX,BCK,BCKID,ISTAT)
*
*  Module number:
*
*  Module name: zclbkg
*
*  Keyphrase:
*  ----------
*       GHRS background subtraction
*
*  Description:
*  ------------
*       This routine subtracts the background by one of 4 methods.
*       The first method applicable is used.
*
*       1) Subtract Sky spectra
*          This uses substep bins with a BINID or 5 or 6.
*          For this case the background is smoothed by a
*          Median filter of width SKYMDF followed by a mean
*          filter of width SKYMNF followed by a polynomial of order SKYORD.
*
*       2) Subtract Interorder from main diode array.
*          This uses the background bins with binids of 3 or 4.
*          For this case the background is smoothed by a
*          Median filter of width INTMDF followed by a mean
*          filter of width INTMNF followed by a polynomial of order INTORD.
*
*       3) Use background diodes from separate substep bins
*          with binids between 8 through 15.  The diodes to use
*          are selected on the basis of the binid.
*               binid=8       both upper
*                       9       both lower
*                       10      left upper
*                       11      left lower
*                       12      right upper
*                       13      right lower
*                       14      both left
*                       15      both right
*
*       4) Use all background diodes from same bins as gross spectrum.
*	
*	Scattered light correction is done for the last 3 of these methods
*	using the formulation: 
*
*	B(i) = 0.5(a*U(i)+b*L(i))-c*N(i)+d*Nave
*
*	where:
*
*	B(i) is the background at diode i
*	a,b,c,d are scattered light coefficients from table ccrb
*	U(i) is the upper interorder background
*	L(i) is the lower interorder background
*	N(i) is the net on-order count rate found from:
*		N(i) = O(i) - 0.5(U(i)+L(i))
*	Nave is the average of N(i) over all science diodes
*	
*
*  FORTRAN name: zclbkg.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*  Subroutines Called:
*  -------------------
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*	1.2	May 90	S. Hulbert	Added polynomial smoothing
*					and scattered light correction
*	1.3	Aug 90	S. Hulbert	Bug fix in scattered light correction
*       1.4     Apr 93  J. Eisenhamer   Bug fix corrected apparent typos:
*                                       NUPRIG --> NUPRIT
*                                       NLORIG --> NLORIT
*       1.5     Feb94   J. Eisenhamer   Returned the background subtracted.
*       1.6     Nov 96  M. De La Pena   Added STPTIM to HRSDEF
*-------------------------------------------------------------------------------
*
* INPUTS:
*       pass  - integer variable set to 1 on first call, -1 on last
*       iread - readout number
*	ccrb - name of table containing scattered light coefficients
*       data - data array (500xnbins)
*       et - trailer array (24xnbins)
*       eps - data quality for data array (500xnbins)
*       epset - data quality for trailer array
*       skymdf - sky median filter width
*       skymnf - sky mean filter width
*       skyord - sky polynomial filter order
*       intmdf - interorder median filter width
*       intmnf - interorder mean filter width
*       intord - interorder polynomial filter order
*       ratio - ratio of size of LSA/SSA
*       ns - legnth of the spectra
*       epsf - epsilon array for flux vector
*
* INPUT/OUTPUTS:
*       flux - flux array (ns real)
*
* OUTPUT:
*       bck - Background that was subtracted.
*       bckid - Binid containing the background group information.
*       istat - error status
*---------------------------------------------------------------------------
        CHARACTER*5 GRAT
        CHARACTER*3 APER
        CHARACTER*64 CCRB 
        INTEGER SPORD, CARPOS
        REAL DATA(500,7),ET(24,7),EPS(500,7),EPSET(24,7)
        INTEGER SKYMDF,SKYMNF,INTMDF,INTMNF,NS,ISTAT,IREAD
        INTEGER SKYORD,INTORD,BCKID
        REAL RATIO,FLUX(NS),EPSF(NS),BCK(NS)
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
      PARAMETER (TYINT=4)
      INTEGER TYREAL
      PARAMETER (TYREAL=6)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT=1)
      INTEGER STDERR
      PARAMETER (STDERR=2)
C
C                       /HRSDEF/
C Deflection pattern common block
C       NBINS - Number of supstep bins
C       NINIT - number of initial deflection pairs
C       IXDEF(5) - initial x-deflections
C       XDEF(7) - x-deflections for each bin
C       YDEF(7) - y-deflections for each bin
C       RCODES(7) - repeat codes for each bin
C       BINIDS(7) - substep bins ids for each bin
C       SAMPLE(7) - starting sample position for each spectra
C       LINE(7) - starting line position for each spectra
C       DELTAS(7) - sample position increment for each spectra
C       HOFF(7),VOFF(7) - horizontal and vertical offsets
C       DOPMAG, DOPZER - dopler magnitude and zero time
C       XDCAL, XDCALP - x-deflection calibration parameters
C       STPTIM - integration time at each step pattern position
C
        INTEGER NBINS,NINIT,IXDEF(5),XDEF(7),YDEF(7),RCODES(7)
        INTEGER BINIDS(7),HOFF(7),VOFF(7)
        REAL SAMPLE(7),LINE(7),DELTAS(7),DOPMAG,XDCAL,XDCALP
        DOUBLE PRECISION DOPZER,STPTIM
        COMMON /HRSDEF/ DOPZER,STPTIM,NBINS,NINIT,IXDEF,XDEF,YDEF,
     *                  RCODES,BINIDS,SAMPLE,LINE,DELTAS,HOFF,VOFF,
     *                  DOPMAG,XDCAL,XDCALP
C
C fill epsilon
C
        INTEGER EPSFIL
        PARAMETER (EPSFIL=800)
	REAL NOBCK
	PARAMETER (NOBCK=200.)
C
C Local variables
C
        INTEGER   FIRST
        PARAMETER (FIRST = 1)
C
        CHARACTER*80 CONTXT,STRBCK
        LOGICAL TFLAG(4)
C                         --->flag set to true if background type found
        INTEGER BINID,I,J,POS,I1,BTYPE
        INTEGER BLOC(2,8),LOC4(4)
        CHARACTER*3 SKYAP
C                                    --->sky aperture LSA or SSA
        INTEGER MEAN,MEDIAN,ORDER
C                                    --->filter widths,polynomial order
        LOGICAL FILLED
C                                    --->fill data in background?
        REAL RPOS,FRAC
C                                    --->Position of interpolation
        INTEGER IN1,IN2
C                                    --->Interpolation points
        REAL XSTEPS,FACTOR
        REAL NETAVE
        REAL UPRITE,LORITE,UPLEFT,LOLEFT
        REAL BINTUP,BINTLO
C
        DOUBLE PRECISION COEF(4)
C                                    --->scattered light coefficients
        INTEGER BACK, NET, NADDS, FBACK
        INTEGER GROSS, RAWSUM
        INTEGER NUPRIT,NLORIT,NUPLEF,NLOLEF
        INTEGER ISTATS(10)
C
C Background diode junk
C
        REAL DPOS
        INTEGER NRIGHT,NLEFT,NLO,NUP
C
C Locations of background diodes to use for binids=8 to 15.
C
        DATA BLOC/1,11,  2,12,  1,0,  2,0, 11,0, 12,0, 1,2, 11,12/
C
C Locations of background diodes to use for background type 4
C
        DATA LOC4/1,2,11,12/
C-----------------------------------------------------------------------------
C
C Determine background type
C
        DO 10 I=1,3
10              TFLAG(I)=.FALSE.
        TFLAG(4)=.TRUE.
C                                    --->Last type always available
        BCKID=0
        DO 20 I=1,NBINS
                BINID=BINIDS(I)
                IF((BINID.EQ.3).OR.(BINID.EQ.4))THEN
                   TFLAG(2)=.TRUE.
                   BCKID=I
                ENDIF
                IF(BINID.EQ.5)THEN
                   TFLAG(1)=.TRUE.
                   SKYAP='SSA'
                   BCKID=I
                ENDIF
                IF(BINID.EQ.6)THEN
                   TFLAG(1)=.TRUE.
                   SKYAP='LSA'
                   BCKID=I
                ENDIF
                IF((BINID.GT.7).AND.(BINID.LT.16))THEN
                   BCKID=I
                   TFLAG(3)=.TRUE.
                ENDIF
20      CONTINUE
        DO 30 I=1,4
                IF(TFLAG(I))THEN
                        BTYPE=I
                        GO TO 40
                ENDIF
30      CONTINUE
40      CONTINUE
C
C For the first pass, get scattered light coefficients and print messages
C
        IF((IREAD.EQ.1).AND.(BTYPE.GT.1))THEN
            CALL ZRCCRB (CCRB,GRAT,APER,SPORD,COEF,ISTAT)
            IF (ISTAT.NE.0) THEN
                CONTXT='ERROR getting scattered light coefficients'
                GO TO 999
            ENDIF
C
            IF(BTYPE.EQ.1)THEN
                STRBCK='sky spectrum.' 
            ELSE IF(BTYPE.EQ.2)THEN
                STRBCK='interorder spectrum.'
            ELSE IF(BTYPE.EQ.3)THEN
                STRBCK='background diodes from separate bins.'
            ELSE
                STRBCK='bkg diodes from same bins as gross '
     *                 // 'spectrum.'
            ENDIF
            CONTXT = 'Background computed with ' // STRBCK
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
C
C allocate memory and initialize
C
        IF (BTYPE.LE.2) THEN
            CALL UDMGET (500, TYREAL, BACK, ISTATS(1))
            CALL UDMGET (500, TYREAL, FBACK, ISTATS(2))
            CALL UDMGET (500, TYINT, NADDS, ISTATS(3))
            DO 115 I=1,3
                IF (ISTATS(I).NE.0) THEN
                    CONTXT='ERROR allocating memory'
                    GO TO 999 
                ENDIF
115         CONTINUE
            DO 50 I=1,500
                MEMR(BACK+I-1)=0.0
                MEMI(NADDS+I-1)=0
50          CONTINUE
        ENDIF
        IF (BTYPE.GE.2) THEN
            IF (BTYPE.EQ.2) THEN
                NN = 500
            ELSE
                NN = NS
            ENDIF
            CALL UDMGET (NN, TYREAL, GROSS, ISTATS(1))
            CALL UDMGET (NN, TYREAL, RAWSUM, ISTATS(1))
            CALL UDMGET (NN, TYREAL, NET, ISTATS(1))
            DO 125 I=1, 3
                    IF (ISTATS(I).NE.0) THEN
                        CONTXT='ERROR allocating memory'
                        GO TO 999 
                    ENDIF
125         CONTINUE
            DO 255 I=1,NN
                    MEMR(GROSS+I-1)=0.0
                    MEMR(RAWSUM+I-1)=0.0
                    MEMR(NET+I-1)=0.0
255         CONTINUE
        ENDIF
        IF (BTYPE.GE.3) THEN
            UPRITE=0.
            LORITE=0.
            UPLEFT=0.
            LOLEFT=0.
            NUPRIT=0
            NLORIT=0
            NUPLEF=0
            NLOLEF=0
        ENDIF
C
C set fill flag
C
        FILLED=.FALSE.
C
C number of x-steps
C
        XSTEPS=NS/500
        IXSTEP=INT(XSTEPS)
C
C -----------------------------------------------------------------
C BTYPE=1   sky background (Note: Currently, this option is unavailble)
C
        IF(BTYPE.EQ.1)THEN
C
C normalization factor between apertures
C
            IF(SKYAP.EQ.'LSA')THEN
                    FACTOR=1.0/RATIO
                ELSE
                    FACTOR=RATIO
            ENDIF
C
C search for valid bins
C
            DO 120 I=1,NBINS
                BINID=BINIDS(I)
                IF((BINID.EQ.5).OR.(BINID.EQ.6))THEN
                    DO 110 J=1,500
                        IF(EPS(J,I).NE.EPSFIL)THEN
                                MEMR(BACK+J-1)=MEMR(BACK+J-1)+DATA(J,I)
                                MEMI(NADDS+J-1)=MEMI(NADDS+J-1)+1
                            ELSE
                                FILLED=.TRUE.
                        ENDIF
110                 CONTINUE
                ENDIF
120         CONTINUE
            DO 130 J=1,500
                IF(MEMI(NADDS+J-1).EQ.0) THEN
                    WRITE(CONTXT,799)IREAD
799                 FORMAT('Warning: NO background computed',
     *                   ' for readout',I7,' due to fill data')
                    GO TO 800
                ENDIF
                MEMR(BACK+J-1)=MEMR(BACK+J-1)/MEMI(NADDS+J-1)*FACTOR
130         CONTINUE
C
            MEAN=SKYMNF
            MEDIAN=SKYMDF
            ORDER=SKYORD
        ENDIF
C
C----------------------------------------------------------------------------
C BType 2  Interorder spectra from main diode array
C
        IF(BTYPE.EQ.2)THEN
C
C search for valid bins
C
            DO 220 I=1,NBINS
                BINID=BINIDS(I)
                IF((BINID.EQ.3).OR.(BINID.EQ.4))THEN
C
C get the upper/lower coefficient index: 1=upper; 2=lower
C
                    K=BINID-2
C 
C sum all the backgrounds at a given point
C
                    DO 210 J=1,500
                        IF(EPS(J,I).NE.EPSFIL)THEN
                            MEMR(BACK+J-1)=MEMR(BACK+J-1)+
     $                                  COEF(K)*DATA(J,I)
                            MEMR(RAWSUM+J-1)=MEMR(RAWSUM+J-1)+DATA(J,I)
                            MEMI(NADDS+J-1)=MEMI(NADDS+J-1)+1
                        ELSE
                            FILLED=.TRUE.
                        ENDIF
210                 CONTINUE
                ENDIF
220         CONTINUE
            NETAVE=0.
            DO 230 J=1,500
                IF(MEMI(NADDS+J-1).EQ.0) THEN
                    WRITE(CONTXT,799)IREAD
                    GO TO 800
                ENDIF
                MEMR(BACK+J-1)=MEMR(BACK+J-1)/MEMI(NADDS+J-1)
C
                IF (IXSTEP.GT.1) THEN
                    DO 215 L=1,IXSTEP
                        JJ=(J-1)*IXSTEP+L
                        MEMR(GROSS+J-1)=MEMR(GROSS+J-1)+FLUX(JJ)
215                 CONTINUE
                    MEMR(GROSS+J-1)=MEMR(GROSS+J-1)/IXSTEP   
                ELSE
                    MEMR(GROSS+J-1)=FLUX(J)
                ENDIF
C
                MEMR(NET+J-1)=MEMR(GROSS+J-1)-
     $                  MEMR(RAWSUM+J-1)/MEMI(NADDS+J-1)
C
                NETAVE=NETAVE + MEMR(NET+J-1)
230         CONTINUE
            NETAVE=NETAVE/500
            DO 235 J=1,500
                MEMR(BACK+J-1)=MEMR(BACK+J-1)-COEF(3)*MEMR(NET+J-1)+
     $                  COEF(4)*NETAVE
        
235         CONTINUE
C
            MEAN=INTMNF
            MEDIAN=INTMDF
            ORDER=INTORD
        ENDIF
C
C----------------------------------------------------------------------------
C
C BTYPE=1 or 2  Smooth and subtract from FLUX (use interpolation if
C               length of flux is 1000 or 2000
C
        IF((BTYPE.EQ.1).OR.(BTYPE.EQ.2))THEN
C
C median filter, make the filter odd
C
                IF (MEDIAN.GT.0) THEN
                    MEDIAN=(MEDIAN/2)*2+1
                    CALL ZCLMDF(MEMR(BACK),500,MEDIAN,
     *                   MEMR(FBACK),ISTAT)
                    IF(ISTAT.NE.0)GO TO 1000
                    IF(IREAD.EQ.1)THEN
                       WRITE(CONTXT,99)MEDIAN
 99                    FORMAT('Background median filter width=',I4)
                       CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
                    ENDIF
                    DO 88 I = 1, 500
                       MEMR(BACK+I-1)=MEMR(FBACK+I-1)
 88                 CONTINUE
                ENDIF
C
C mean filter, make the filter odd
C
                IF (MEAN.GT.0) THEN
                    MEAN=(MEAN/2)*2+1
                    CALL ZCLMNF(MEMR(BACK),500,MEAN,
     *                 MEMR(FBACK),ISTAT)
		    IF(ISTAT.NE.0)GO TO 1000
                    IF(IREAD.EQ.1)THEN
                        WRITE(CONTXT,199)MEAN
199                      FORMAT('Background mean filter width=',I4)
                        CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
                    ENDIF
		    DO 87 I = 1, 500
		       MEMR(BACK+I-1)=MEMR(FBACK+I-1)
 87		    CONTINUE
		ENDIF
C
C polynomial smoothing
C
		IF (ORDER.GE.0) THEN
		    CALL ZCLPLY(MEMR(BACK),500,ORDER,ISTAT)
                    IF(ISTAT.NE.0)GO TO 1000
                    IF(IREAD.EQ.1)THEN
                        WRITE(CONTXT,299)ORDER 
299                      FORMAT('Background polynomial fit order=',I4)
                        CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
                    ENDIF
		ENDIF
C
C
C interpolate in the background array and subtract
C
                DO 250 I=1,NS
                   IF(EPSF(I).NE.EPSFIL)THEN
                        RPOS=(I-1)/XSTEPS+1
                        IN1=RPOS
                        IN2=IN1+1
                        IF(IN2.GT.500)IN2=500
                        FRAC=RPOS-IN1
                        BCK(I)=MEMR(BACK+IN1-1)+FRAC*
     $				(MEMR(BACK+IN2-1)-MEMR(BACK+IN1-1))
			FLUX(I) = FLUX(I)-BCK(I) 
                   ENDIF
250             CONTINUE
        ENDIF
C------------------------------------------------------------------------------
C BTYPE=3  Use special diodes from binids=8 to 15
C
        IF(BTYPE.EQ.3)THEN
            DO 320 I=1,NBINS
                BINID=BINIDS(I)
                IF((BINID.GT.7).AND.(BINID.LT.16))THEN
                    I1=BINID-7
C                                    --->position in BLOC array
                    DO 310 J=1,2
                        POS=BLOC(J,I1)
C                                    --->background diode position
                        IF(POS.GT.0)THEN
		            IF (MOD(POS,2).EQ.0) THEN
				K=2
C				     --->lower diode
			    ELSE
				K=1
C				     --->upper diode
			    ENDIF
                            IF(EPSET(POS,I).NE.EPSFIL)THEN
                                IF(POS.GT.6)THEN
				    IF(K.EQ.1)THEN
                                        UPRITE=UPRITE+ET(POS,I)
                                        NUPRIT=NUPRIT+1
				    ELSE
                                        LORITE=LORITE+ET(POS,I)
                                        NLORIT=NLORIT+1
				    ENDIF
                                ELSE
				    IF(K.EQ.1)THEN
                                        UPLEFT=UPLEFT+ET(POS,I)
                                        NUPLEF=NUPLEF+1
				    ELSE
                                        LOLEFT=LOLEFT+ET(POS,I)
                                        NLOLEF=NLOLEF+1
				    ENDIF
                                ENDIF
                            ELSE
                                FILLED=.TRUE.
                            ENDIF
                        ENDIF
310                 CONTINUE
                ENDIF
320         CONTINUE
        ENDIF
C
C-----------------------------------------------------------------------------
C BTYPE=4 Use main array background diodes
C
        IF(BTYPE.EQ.4)THEN
            DO 420 I=1,NBINS
                IF((BINIDS(I).EQ.1).OR.(BINIDS(I).EQ.2).OR.
     *                                 (BINIDS(I).EQ.7))THEN
                   DO 410 J=1,4
                        POS=LOC4(J)
C                                    --->Loc. of the back. diode
                        IF(EPSET(POS,I).NE.EPSFIL)THEN
			    IF (MOD(POS,2) .EQ. 0) THEN
				K=2
C				     --->lower diode
			    ELSE
				K=1
C				     --->upper diode
			    ENDIF
                            IF(POS.GT.6)THEN
				IF(K.EQ.1)THEN
                                    UPRITE=UPRITE+ET(POS,I)
                                    NUPRIT=NUPRIT+1
				ELSE
                                    LORITE=LORITE+ET(POS,I)
                                    NLORIT=NLORIT+1
				ENDIF
                            ELSE
				IF(K.EQ.1)THEN
                                    UPLEFT=UPLEFT+ET(POS,I)
                                    NUPLEF=NUPLEF+1
				ELSE
                                    LOLEFT=LOLEFT+ET(POS,I)
                                    NLOLEF=NLOLEF+1
				ENDIF
                            ENDIF
                        ELSE
                            FILLED=.TRUE.
                        ENDIF
410                 CONTINUE
                ENDIF
420         CONTINUE
        ENDIF
C
C--------------------------------------------------------------------------
C
C TYPES 3 or 4 - use left and right background value
C
        IF((BTYPE.EQ.3).OR.(BTYPE.EQ.4))THEN
C
C check to see what we got.
C
	    NLEFT=NLOLEF+NUPLEF
	    NRIGHT=NLORIT+NUPRIT
	    NUP=NUPLEF+NUPRIT
	    NLO=NLOLEF+NLORIT
            IF((NUP.EQ.0).AND.(NLO.EQ.0))THEN
	       WRITE(CONTXT,470)IREAD
 470	       FORMAT('No background diodes.  Background not ',
     1 	    'removed for readout ',I7)
	       GO TO 800
	    ENDIF
            IF(NLOLEF.GT.1) LOLEFT=LOLEFT/NLOLEF
            IF(NUPLEF.GT.1) UPLEFT=UPLEFT/NUPLEF
            IF(NLORIT.GT.1) LORITE=LORITE/NLORIT
            IF(NUPRIT.GT.1) UPRITE=UPRITE/NUPRIT
C
C print some messages
C
            IF(FILLED.AND.(NLEFT.EQ.0))THEN
                WRITE(CONTXT,499)IREAD
499             FORMAT('WARNING: only right background diodes ',
     *                 'available for readout',I7)
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            ENDIF
            IF(FILLED.AND.(NRIGHT.EQ.0))THEN
                WRITE(CONTXT,599)IREAD
599             FORMAT('WARNING: only left background diodes ',
     *                 'available for readout',I7)
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            ENDIF
            IF(FILLED.AND.(NUP.EQ.0))THEN
                WRITE(CONTXT,695)IREAD
695             FORMAT('WARNING: only upper background diodes ',
     *                 'available for readout',I7)
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            ENDIF
            IF(FILLED.AND.(NLO.EQ.0))THEN
                WRITE(CONTXT,795)IREAD
795             FORMAT('WARNING: only lower background diodes ',
     *                 'available for readout',I7)
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            ENDIF
            IF(NLOLEF.EQ.0)THEN
		IF(NUPLEF.GT.0)THEN
		    LOLEFT=UPLEFT
c		ELSE IF(NLORIG.GT.0)THEN
		ELSE IF(NLORIT.GT.0)THEN
		    LOLEFT=LORITE
c		ELSE IF(NUPRIG.GT.0)THEN
		ELSE IF(NUPRIT.GT.0)THEN
		    LOLEFT=UPRITE
		ENDIF
	    ENDIF
            IF(NUPLEF.EQ.0)THEN
		IF(NLOLEF.GT.0)THEN
		    UPLEFT=LOLEFT
c		ELSE IF(NUPRIG.GT.0)THEN
		ELSE IF(NUPRIT.GT.0)THEN
		    UPLEFT=UPRITE
c		ELSE IF(NLORIG.GT.0)THEN
		ELSE IF(NLORIT.GT.0)THEN
		    UPLEFT=LORITE
		ENDIF
	    ENDIF
            IF(NLORIT.EQ.0)THEN
		IF(NUPRIT.GT.0)THEN
		    LORITE=UPRITE
		ELSE IF(NLOLEF.GT.0)THEN
		    LORITE=LOLEFT
		ELSE IF(NUPLEF.GT.0)THEN
		    LORITE=UPLEFT
		ENDIF
	    ENDIF
            IF(NUPRIT.EQ.0)THEN
		IF(NLORIT.GT.0)THEN
		    UPRITE=LORITE
		ELSE IF(NUPLEF.GT.0)THEN
		    UPRITE=UPLEFT
		ELSE IF(NLOLEF.GT.0)THEN
		    UPRITE=LOLEFT
		ENDIF
	    ENDIF
C
C interpolate then subtract
C left background is at effective diode position -17 if the
C first diode on the main array is 0. right background diode position
C is at 516.
C
	    NETAVE=0.
            DO 600 I=1,NS
                IF(EPSF(I).NE.EPSFIL)THEN
                    DPOS=(I-1)/XSTEPS
		    FRAC=(DPOS+17)/533.0
                    BINTUP=UPLEFT+(UPRITE-UPLEFT)*FRAC
                    BINTLO=LOLEFT+(LORITE-LOLEFT)*FRAC
		    MEMR(RAWSUM+I-1)=(BINTUP+BINTLO)/2
                    BCK(I)=(COEF(1)*BINTUP+COEF(2)*BINTLO)/2
		    MEMR(GROSS+I-1)=FLUX(I)
                    MEMR(NET+I-1)=MEMR(GROSS+I-1)-MEMR(RAWSUM+I-1)
		    NETAVE=NETAVE+MEMR(NET+I-1)
                ENDIF
600         CONTINUE
	    NETAVE=NETAVE/NS
            DO 295 I=1,NS
		BCK(I)=BCK(I)-COEF(3)*MEMR(NET+I-1)+
     $			COEF(4)*NETAVE	
                FLUX(I)=FLUX(I)-BCK(I) 
295         CONTINUE
        ENDIF
C
C--------------------------------------------------------------------------
C
C DONE clean up
C
        IF(FILLED)THEN
            WRITE(CONTXT,699)IREAD
699         FORMAT('WARNING: readout',I7,' contained fill data in the',
     *          ' background')
	    GO TO 800
        ENDIF
        ISTAT=0
        GO TO 1000
C
C Background could not be computed.  Flag the data quality
C
 800	CONTINUE
	CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	WRITE(CONTXT,811)NOBCK
 811	FORMAT ('Calibrated science data quality value set to ', 
     1    f5.0)
	CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	DO 801 I = 1, NS
	   EPSF(I) = MAX(EPSF(I), NOBCK)
 801	CONTINUE
	ISTAT = 0
	GO TO 1000
	
C ERROR 
C
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
C 
C free memory
C
 1000	CONTINUE
	IF (NET.NE.0) CALL UDMFRE (NET, TYREAL, ISTATS(2))
	IF (BACK.NE.0) CALL UDMFRE (BACK, TYREAL, ISTATS(3))
	IF (FBACK.NE.0) CALL UDMFRE (FBACK, TYREAL, ISTATS(4))
	IF (NADDS.NE.0) CALL UDMFRE (NADDS, TYINT, ISTATS(5))
	IF (GROSS.NE.0) CALL UDMFRE (GROSS, TYREAL, ISTATS(6))
	IF (RAWSUM.NE.0) CALL UDMFRE (RAWSUM, TYREAL, ISTATS(7))
	DO 288 I=1,7
	    IF (ISTATS(I).NE.0) THEN
 	        CONTXT='ERROR deallocating memory'
	        CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	        ISTAT=1
            ENDIF
288	CONTINUE
        RETURN
        END
