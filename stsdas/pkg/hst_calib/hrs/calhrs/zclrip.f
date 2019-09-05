        SUBROUTINE ZCLRIP(PASS,CCR9,CCRA,GRAT,CARPOS,ORDER,MINRIP,
     *                  NS,NSPEC,FLUX,ERR,EPS,ISTAT)
*
*  Module number:
*
*  Module name: zclrip
*
*  Keyphrase:
*  ----------
*       GHRS echelle ripple routine
*
*  Description:
*  ------------
*       This routine performs the echelle ripple removal by dividing
*       the flux by the echelle ripple function given by:
*
*               ripple = gnorm * sinc(a*x+b)**2
*
*       where:
*                        cos(theta+beta+delta)
*               gnorm = -----------------------
*                       cos(theta+beta-delta+e)
*
*                            cos(theta+beta+delta)*sin(theta+e/2)
*               x = pi * m * ------------------------------------
*                                   sin(theta+beta+e/2)
*
*               e = arctan ( (samp-280.0)/f )
*               samp = the photocathode sample position
*               theta = (r0 - carpos)*2*pi/65536.0
*               r0,beta,delta,f are grating parameters in table CCRA
*               a and b are coefficients from table CCR9
*
*	The ripple function is normalized to 1. at carrousel position
*	27492 for ECH-A and 39144 for ECH-B at the center of the
*	photocathode (e=0)
*
*  FORTRAN name: zclrip
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zrccra, zrccr9
*  SDAS:
*       ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*      1.1      Sep 91  S. Hulbert      Normalize ripple function, implemented
*					PASS flag
*      1.2      Nov 96  M. De La Pena   Added STPTIM to HRSDEF
*-------------------------------------------------------------------------------
* INPUTS:
*       pass - integer flag set to 1 on first call, -1 on last
*       ccr9 - interpolated echelle ripple coef. table
*       ccra - echelle ripple grating parameter table
*       grat - grating mode
*       carpos - carrousel position
*       order - spectral order
*       minrip - minimum ripple value to apply
*       ns - number of points in each spectra
*       nspec - number of spectra
*
* INPUT/OUTPUTS:
*       flux - flux array (nsxnspec)
*       err - propagated statistical error array
*       eps - epsilon array
*
* OUTPUT:
*       istat - error status
*
*----------------------------------------------------------------------------
        INTEGER PASS
        CHARACTER*64 CCRA,CCR9
        CHARACTER*5 GRAT
        INTEGER CARPOS,ORDER,NS,NSPEC,ISTAT
        REAL FLUX(NS,NSPEC),ERR(NS,NSPEC),EPS(NS,NSPEC),MINRIP
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
        INTEGER EPSFIL
        PARAMETER (EPSFIL = 800)
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
C local variables
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
        INTEGER I,ISPEC,CLAST
        REAL X,E,PI,F,BETA,THETA,DELTA,R0,COEF(2),TBD,TB,SINC2
        REAL COSTBD,E2,TBMD,R,S,DS
	REAL CPNORM, RNORM
        CHARACTER*80 CONTXT
        DATA PI/3.14159/
C
C--------------------------------------------------------------------------
C
C If first call read table ccra
C
        IF((GRAT.NE.'ECH-A').AND.(GRAT.NE.'ECH-B'))GO TO 900
        IF(PASS.EQ.FIRST)THEN
           CALL ZRCCRA(CCRA,GRAT,F,BETA,DELTA,R0,CPNORM,ISTAT)
           IF(ISTAT.NE.0)GO TO 999
           WRITE(CONTXT,99)GRAT,R0
 99        FORMAT('Echelle Ripple grating parameters for ',A5,
     *          '   R0=',F10.1)
           CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
           WRITE(CONTXT,199)BETA,DELTA,F
 199       FORMAT(' beta=',F8.3,'   delta=',F8.3,'   F=',G14.6)
           CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
C     
C convert angles to radians, focal length to sample units
C     
           BETA = BETA * PI/180.0
           DELTA = DELTA * PI/180.0
           F = F/0.05
           CLAST=-1
        ENDIF
C     
C If new carrousel position, get new A,B coefficients
C
        IF(CARPOS.NE.CLAST)THEN
           CALL ZRCCR9(PASS,CCR9,GRAT,ORDER,CARPOS,COEF,ISTAT)
           IF(ISTAT.NE.0)GO TO 999
           WRITE(CONTXT,299)CARPOS,COEF
 299       FORMAT('Echelle ripple coef. for carpos=',I7,
     *          '   A=',F10.6,'   B=',F10.6)
           CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
C     
C calculate normalization factor for ripple fuction at the CPCENT carrousel
C position with E=0 (i.e. at the center of the photocathode)
C
           THETA = (R0-CPNORM)*2*PI/65536.0 - BETA
           TB = THETA+BETA
           TBD = TB + DELTA
           TBMD = TB - DELTA
           COSTBD = COS(TBD)
           X = PI*ORDER*COSTBD*SIN(THETA)/SIN(TB)
           X = COEF(1)*X + COEF(2)
           IF(ABS(X).LT.1.0E-15) THEN
              SINC2=1.0
           ELSE
              SINC2=(SIN(X)/X)**2
           ENDIF
           RNORM = COSTBD/COS(TBMD)*SINC2
C
C compute new grating angle
C
           THETA = (R0-CARPOS)*2*PI/65536.0 - BETA
           CLAST=CARPOS
        ENDIF
C
C Compute some things that don't change from point to point
C
        TB = THETA+BETA
        TBD = TB + DELTA
        TBMD = TB - DELTA
        COSTBD = COS(TBD)
C     
C perform echelle ripple removal for each spectra
C
        DO 500 ISPEC=1,NSPEC
           S = SAMPLE(ISPEC)-280.0
           DS = DELTAS(ISPEC)
           DO 300 I=1,NS
C
C calculate ripple function 
C
              E = ATAN(S/F)
              E2 = E/2.0
              X = PI*ORDER*COSTBD*SIN(THETA+E2)/SIN(TB+E2)
              X = COEF(1)*X + COEF(2)
              IF(ABS(X).LT.1.0E-15) THEN
                 SINC2=1.0
              ELSE
                 SINC2=(SIN(X)/X)**2
              ENDIF
              R = COSTBD/COS(TBMD+E)*SINC2
              R = R/RNORM
C
              IF(EPS(I,ISPEC).NE.EPSFIL) THEN
                 IF(R.GT.MINRIP)THEN
                    FLUX(I,ISPEC)=FLUX(I,ISPEC)/R
                    ERR(I,ISPEC)=ERR(I,ISPEC)/R
                 ELSE
                    FLUX(I,ISPEC)=0.0
                    ERR(I,ISPEC)=0.0
                 ENDIF
              ENDIF
              S=S+DS
 300       CONTINUE
 500    CONTINUE
 900    ISTAT=0
        GO TO 1000
 999    ISTAT=1
 1000   RETURN
        END
