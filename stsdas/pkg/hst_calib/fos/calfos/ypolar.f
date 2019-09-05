        SUBROUTINE YPOLAR(NFRAME,NX,IPASS,FLUX,ERR,EPS,FILL,
     *			MAXLEN,RET,ALPHA,W1,POLANG,RESULT)
*
*  Module number:
*
*  Module name: YPOLAR
*
*  Keyphrase:
*  ----------
*       Polarimetry processing
*
*  Description:
*  ------------
*       This routine computes the Stokes parameters, linear and circular
*       polarization, and the polarization angle for FOS
*       polarimetry data.
*
*  FORTRAN name: ypolar
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput, ysolve
*  SDAS:
*
*  Others:
*
*
*  History:
*  --------
*  Version      Date    Author		Description
*       1       Aug 89  D. Lindler      Coded per SE-06
*       2       Jun 90  D. Lindler      Scaling added
*		Sep 90  S. Hulbert	Added POLANG
*	3	Mar 92  S. Hulbert	Pass in EPS and FILL. Modify
*					weighting scheme.
*	3.1	Mar 94	H. Bushouse	Bypass check for D3 < 1.e-8 if D3
*					previously set to 0 because NOCIRC=T.
*	4	Jun 94	H. Bushouse	Allow negative I values; set PL, PC,
*					and Theta=0 when I < 0.
*	4.1	Sep 94	H. Bushouse	Initialize XSC to 1.
*       4.2     Mar 97  M. De La Pena   Removed PGAPER parameter - not used.
*-------------------------------------------------------------------------------
*
* INPUTS:
*       NFRAME - number of waveplate angles
*       NX - Number of data points in the spectra
*       IPASS - pass direction
*       FLUX - flux array
*       ERR - propagated statistical error array 
*			(zeros DON'T flag bad data, anymore)
*       EPS - data quality array
*       FILL - fill value above which we should reject the data
*	MAXLEN - first dimension of RET
*       RET - retardation array
*		Ret(*,1) = 0.5*(1+cos(delta))
*		RET(*,2) = 0.5*(1-cos(delta))
*		RET(*,3) = sin(delta)
*       ALPHA - angle of the pass direction of the Wollaston with respect to
*               the Q=1 coordinate axis.
*       W1 - initial position of the waveplate
*	POLANG - initial position of the polarizer
*
* OUTPUTS:
*       RESULT - result array
*                 RESULT(i,1) = Stokes parameter I for data point i
*                       (i,2) =    "      "      Q
*                       (i,3) =    "      "      U
*                       (i,4) =    "      "      V
*                       (i,5) = error estimate for I
*                       (i,6) =   "      "      "  Q
*                       (i,7) =   "      "      "  U
*                       (i,8) =   "      "      "  V
*                       (i,9) = linear polarization PL
*                       (i,10)= circular polarization PC
*                       (i,11)= polarization angle THETA
*                       (i,12)= error estimate for PL
*                       (i,13)=   "      "      "  PC
*                       (i,14)=   "      "      "  THETA
*----------------------------------------------------------------------------
	IMPLICIT NONE
C
        INTEGER NFRAME,NX,IPASS,MAXLEN
        REAL FLUX(16,NX),ERR(16,NX),EPS(16,NX),RET(MAXLEN,3),ALPHA,W1
        REAL RESULT(MAXLEN,14)
	REAL POLANG,FILL
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C local variables
C
        CHARACTER*80 CONTXT
        DOUBLE PRECISION WK,PI,COS4WK,SIN4WK,COS2WK,SIN2WK
        DOUBLE PRECISION I,Q,U,V,VARI,VARQ,VARU,VARV,VARL,VARC,VART
        DOUBLE PRECISION Q2,I2,V2,U2,PL,PC,THETA,Q2U2
        INTEGER NGOOD,ERRNUM,K,CHAN,L,M,ROW,COL,ERRCNT,ISTAT
        DOUBLE PRECISION SUM,AA(4,4),G(4),AINV(4,5),F(4,16),W(16)
        DOUBLE PRECISION A,B,R(16),S(16),T(16),D1,D2,D3
        CHARACTER*30 MESS(8)
        LOGICAL NOCIRC
        REAL NULL, SCALE, XSC
        DATA MESS/'Not enough data - no V',
     *            'ABS(SIN(DELTA)) < 1e-8 - no V',
     *            'Not enough data - no I,Q,U',
     *            'Singular Matrix',
     *            'Scaled I < 1e-8',
     *            'Scaled Q or U < 1e-8',
     *            'Variance less than zero',
     *            'Scaled V < 1e-8'/
        DATA PI/3.1415927/,NULL/0.0/
C------------------------------------------------------------------------------
        ERRCNT = 0
        WRITE(CONTXT,9)IPASS
9       FORMAT('Polarimetric processing for pass direction',I2)
        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
        A = COS(2.0*ALPHA)
        B = SIN(2.0*ALPHA)
        DO 100 K=1,NFRAME
               WK = PI/8.0*(K-1) + W1 + POLANG
               COS4WK = DCOS(4.0*WK)
               COS2WK = DCOS(2.0*WK)
               SIN4WK = DSIN(4.0*WK)
               SIN2WK = DSIN(2.0*WK)
               R(K) = A*COS4WK - B*SIN4WK
               S(K) = A*SIN4WK + B*COS4WK
               T(K) = A*SIN2WK + B*COS2WK
100     CONTINUE
C
C loop on channels
C
        DO 900 CHAN = 1,NX
               ERRNUM = 0
C
C initialize output array
C
               DO 110 K=1,14
                  RESULT(CHAN,K)=NULL
110            CONTINUE
C
               D1 = RET(CHAN,1)
               D2 = RET(CHAN,2)
               D3 = RET(CHAN,3)
C
C Circular polarization tests
C
               NOCIRC=.FALSE.
               IF(NFRAME.LT.5) THEN
                  NOCIRC=.TRUE.
                  D3 = 0.0
                  ERRNUM = 1
               ENDIF
C
C Only do next check if NOCIRC=FALSE (modified Mar-94 by HAB)
C
c              IF(DABS(D3).LT.1.0E-8) THEN
               IF((.NOT.NOCIRC).AND.(DABS(D3).LT.1.0E-08)) THEN
                   NOCIRC=.TRUE.
                   D3 = 0.0
                   ERRNUM = 2
               ENDIF
C
C Fill coefficient array
C
               DO 150 K=1,NFRAME
                   F(1,K) = 0.5
                   F(2,K) = 0.5*(D1*A + D2 * R(K))
                   F(3,K) = 0.5*(D1*B - D2 * S(K))
                   F(4,K) = 0.5*(D3*T(K))
150            CONTINUE
C
C Determine scale factor for this channel
C
               SCALE=1.0
               XSC = 1.0
               DO 160 K=1,NFRAME
                  IF(EPS(K,CHAN).LT.FILL.AND.ERR(K,CHAN).GT.0.)THEN
                     IF(ERR(K,CHAN).LT.1.0E-08) XSC=1.0E+12
                     IF(ERR(K,CHAN).LT.1.0E-16) XSC=1.0E+20
                     IF(ERR(K,CHAN).LT.1.0E-24) XSC=1.0E+28
                     IF(XSC.GT.SCALE) SCALE=XSC
                  ENDIF
160            CONTINUE
C
C Scale data
C
               DO 170 K=1,NFRAME
                    ERR(K,CHAN)=ERR(K,CHAN)*SCALE
                    FLUX(K,CHAN)=FLUX(K,CHAN)*SCALE
170            CONTINUE
C
C Compute weights
C NOTE: Kludged weight for zero error to be the same as (what we think)
C can be the smallest non-zero values after scaling.
C This is only a problem, now that a zero count rate is assigned
C a zero statistical error.
C
               NGOOD = 0
               DO 180 K=1,NFRAME
                  IF(EPS(K,CHAN).LT.FILL)THEN
			IF (ERR(K,CHAN).GT.0.)THEN
                            W(K) = 1./ERR(K,CHAN)**2
			ELSE
			    IF(SCALE.EQ.1.)THEN
				W(K)=1.0E16
			    ELSE 
				W(K)=1.0E8
			    ENDIF
			ENDIF
                        NGOOD = NGOOD + 1
                  ELSE
                        W(K) = 0.0
                  ENDIF
180            CONTINUE
               IF(NGOOD.LT.4)THEN
                  ERRNUM=3
                  GO TO 800
               ENDIF
C
C Compute matrix AA
C
               DO 300 L=1,4
                    DO 250 M=1,4
                        SUM = 0.0D0
                        DO 200 K=1,NFRAME
                            SUM = SUM + F(L,K) * F(M,K) * W(K)
200                     CONTINUE
                        AA(L,M) = SUM
250                 CONTINUE
                    SUM = 0.0
                    DO 280 K=1,NFRAME
                        SUM = SUM + FLUX(K,CHAN)*F(L,K)*W(K)
280                 CONTINUE
                    G(L) = SUM
300            CONTINUE
               IF(NOCIRC)THEN
                    AA(4,4)=1.0
                    G(4)=0.0
               ENDIF
C
C Set up special 4 x 5 matrix to solve for (I,Q,U,V)
C
               DO 400 ROW=1,4
                    DO 350 COL=1,4
                        AINV(ROW,COL)=0.0
350                 CONTINUE
                    AINV(ROW,ROW) = 1.0
                    AINV(ROW,5) = G(ROW)
400            CONTINUE
               CALL YSOLVE(AA,4,4,AINV,5,5,ISTAT)
               IF(ISTAT.NE.0)THEN
                        ERRNUM = 4
                        GO TO 800
               ENDIF
C
C Extract I, Q, and U
C
               I = AINV(1,5)
               Q = AINV(2,5)
               U = AINV(3,5)
C
C Extract variances of I, Q, and U
C
               VARI = AINV(1,1)
               VARQ = AINV(2,2)
               VARU = AINV(3,3)
C
C Check for near-zero values in I, Q, and U
C (Allow negative values of I; HAB June-94)
C
c              IF(I.LT.1.0E-08)THEN
               IF(ABS(I).LT.1.0E-08)THEN
                     ERRNUM = 5
                     GO TO 800
               ENDIF
               IF((ABS(Q).LT.1.0E-08).OR.(ABS(U).LT.1.0E-08))THEN
                     ERRNUM = 6
                     GO TO 800
               ENDIF
C
C Compute linear polarization and theta
C
               Q2 = Q**2
               U2 = U**2
               I2 = I**2
               PL = DSQRT(Q2+U2)/I
               THETA = 0.5 * DATAN2(U,Q)
C
C Compute variances for the linear polarization and theta
C
               Q2U2 = Q2+U2
               VARL = VARQ/I2*Q2/Q2U2 + VARU/I2*U2/Q2U2
     *                        + VARI/I2*Q2U2/I2
               VART = (U2/Q2U2*VARQ/Q2U2 + Q2/Q2U2*VARU/Q2U2)/4.0D0
C
C Store I, Q, and U
C
               RESULT(CHAN,1) = I/SCALE
               RESULT(CHAN,2) = Q/SCALE
               RESULT(CHAN,3) = U/SCALE
C
C Store standard errors for I, Q, and U
C
               IF(VARI.LT.0.0) THEN
                      ERRNUM = 7
                      GO TO 800
               ENDIF
               RESULT(CHAN,5) = SQRT(VARI)/SCALE
C
               IF(VARQ.LT.0.0) THEN
                      ERRNUM = 7
                      GO TO 800
               ENDIF
               RESULT(CHAN,6) = SQRT(VARQ)/SCALE
C
               IF(VARU.LT.0.0) THEN
                      ERRNUM = 7
                      GO TO 800
               ENDIF
               RESULT(CHAN,7) = SQRT(VARU)/SCALE
C
C Store the linear polarization and theta
C
               RESULT(CHAN,9)  = PL
               RESULT(CHAN,11) = THETA
C
C Store standard errors for the linear polarization and theta
C
               IF(VARL.LT.0.0) THEN
                      ERRNUM = 7
                      GO TO 800
               ENDIF
               RESULT(CHAN,12) = SQRT(VARL)
C
               IF(VART.LT.0.0) THEN
                      ERRNUM = 7
                      GO TO 800
               ENDIF
               RESULT(CHAN,14) = SQRT(VART)
C
C Compute circular polarization parameters, if possible
C
               IF(.NOT.NOCIRC)THEN
C
C Extract V
C
                   V = AINV(4,5)
C
C Extract variance of V
C
                   VARV = AINV(4,4)
C
C Compute circular polarization
C
                   IF(ABS(V).LT.1.0E-08)THEN
                        ERRNUM = 8
                        GO TO 800
                   ENDIF
                   V2 = V**2
                   PC = V/I
C
C Compute variance of circular polarization
C
                   VARC = VARV/I2 + VARI/I2*V2/I2
C
C Store V
C
                   RESULT(CHAN,4) = V/SCALE
C
C Store standard error for V
C
                   IF(VARV.LT.0.0) THEN
                        ERRNUM = 7
                        GO TO 800
                   ENDIF
                   RESULT(CHAN,8) = SQRT(VARV)/SCALE
C
C Store circular polarization
C
                   RESULT(CHAN,10) = PC
C
C Store standard error for the circular polarization
C
                   IF(VARC.LT.0.0) THEN
                        ERRNUM = 7
                        GO TO 800
                   ENDIF
                   RESULT(CHAN,13) = SQRT(VARC)
               ENDIF
C
C Check for negative values of I; set PL,PC,Theta=0 (HAB June-94)
C
	       IF (I .LT. 0.0) THEN
		   DO 700 K = 9, 14
		      RESULT(CHAN,K) = NULL
700		   CONTINUE
	       END IF
C
C ERROR OCCURED
C
800            IF(ERRNUM.GT.0)THEN
                    ERRCNT = ERRCNT + 1
                    IF(ERRCNT.LT.20)THEN
                       WRITE(CONTXT,899)CHAN,MESS(ERRNUM)
899                    FORMAT('Error in polarimetry, data point',I5,
     *                                  ':',A30)
                       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                    ENDIF
                    IF(ERRCNT.EQ.20)THEN
                       CONTXT='Additional unreported polarimetry errors'
     *                         //' occurred'
                       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                    ENDIF
               ENDIF
C
C Next channel
900     CONTINUE
C
        IF(ERRCNT.GT.0)THEN
           WRITE(CONTXT,1999)NULL
1999       FORMAT('Uncomputed results set to NULL=',G10.4)
           CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF
C
        RETURN
        END
