        SUBROUTINE ZCLWAV(GRAT,ORDER,CARPOS,CAP,COEF,SAMPLE,
     $			DELTAS,NS,WAVE,ISTAT)

*
*  Module number:
*
*  Module name: ZCLWAV
*
*  Keyphrase:
*  ----------
*       Compute HRS wavelength array
*  Description:
*  ------------
*       This routine computes the wavelengths by solving the
*       dispersion relation for wavelength using Newtons iterative
*       method.
*
*       The dispersion relation is:
*
*         s = a0 + a1*m*w + a2*m*m*w*w + a3*m + a4*w + 
*		a5*m*m*w + a6*m*w*w + a7*m*m*m*w*w*w
*
*       where:
*               m - spectral order
*               w - wavelength
*               a0,a1,... - dispersion coefficients
*               s - sample position computed by SAMPLE+(i-1)*DELTAS
*                       where i is the data point number.
*
*  FORTRAN name: zclwav.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       None
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*	1.1	May 91	S. Hulbert	Added cubic dispersion term
*					Change starting wavelength guess
*					Modify calling sequence to include
*					carpos
*	1.2	Sep 91	S. Hulbert	Bug fix for problem calculating "d"
*	1.2.1	Feb 92	S. Hulbert	Bug Fix--change datatype of "d" to
*					double precision
*       1.3     Apr 94  J. Eisenhamer   Removed hardwired constants related
*                                       wavelength guessing.
*-------------------------------------------------------------------------------
*
* INPUTS:
*
*       grat - grating mode
*       order - spectral order
*       coef - dispersion coefficients (8 of them, real*8)
*       sample - starting sample position
*       deltas - delta sample position
*       ns - number of data points
*
* OUTPUTS:
*       wave - wavelength vector (real*8)
*       istat - error status
*
*-----------------------------------------------------------------------------
        CHARACTER*5 GRAT
        INTEGER ORDER,CARPOS,NS,ISTAT
        DOUBLE PRECISION COEF(8),WAVE(1),CAP(2)
        REAL SAMPLE,DELTAS
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C LOCAL VARIABLES
C
        DOUBLE PRECISION S,W,DSDW,W2,MAXERR,SCOMP,A,B,C,D
        CHARACTER*80 CONTXT
        INTEGER M2,NITER,I,ITER
C
        DATA NITER/20/
C                                    --->maximum allowed number of iterations
        DATA MAXERR/0.001D0/
C                                    --->maximum allowed error in sample units
C
C-----------------------------------------------------------------------------
C
C Check for valid spectral order
C
        IF (((GRAT .EQ. 'ECH-A') .AND. ((ORDER .LT. 32) .OR. 
     $		(ORDER .GT. 52))).OR.
     $      	((GRAT .EQ. 'ECH-B') .AND. ((ORDER .LT. 17) .OR. 
     $		(ORDER .GT. 34))))THEN
            CONTXT='Invalid order number computed'
            GO TO 999
        ENDIF
C
C Initialize starting guess
C
        W = CAP(1)/ORDER*SIN((CAP(2)-CARPOS)/10430.378D0)
C
C convert disp. coef. to coefficients of w only
C
        M2=ORDER*ORDER
        A = COEF(1) + COEF(4)*ORDER
        B = COEF(2)*ORDER + COEF(5) + COEF(6)*M2
        C = COEF(3)*M2 + COEF(7)*ORDER
	D = COEF(8)*M2*ORDER
C
C Loop on data points
C
        DO 1000 I=1,NS
                S=SAMPLE+(I-1)*DELTAS
C
C LOOP ON ITERATIONS
C
                DO 100 ITER=1,NITER
                        W2=W*W
                        SCOMP = A + B*W + C*W2 + D*W2*W
                        DSDW = B + 2.0*C*W + 3.0*D*W2
                        IF(DABS(DSDW).LT.1.0E-30) GO TO 120
                        W = W + (S-SCOMP)/DSDW
                        IF(DABS(SCOMP-S).LT.MAXERR) GO TO 200
100             CONTINUE
C
C If we made it here, we did not converge
C
120             CONTXT='Convergence Failure in the wavelength '//
     *                  'computation'
                GO TO 999
C
C converged.
C
200             WAVE(I) = W
1000    CONTINUE
        ISTAT=0
        GO TO 2000
C
C error section
C
999     DO 1500 I=1,NS
                WAVE(I)=0.0
1500    CONTINUE
        CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
2000    RETURN
        END
