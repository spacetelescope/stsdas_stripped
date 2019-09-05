        SUBROUTINE YPOL3(NX,MAXLEN,WAVE,COEF,PGAPER,RES)
*
*  Module number:
*
*  Module name: ypol3
*
*  Keyphrase:
*  ----------
*       Correct polarimetry data for interference
*
*  Description:
*  ------------
*       This routine corrects polarimetry data for interference
*	and changes theta to sky coordinates by adding PA_APER.
*       On input it requires the results after combination of
*       the two pass directions.  These results are corrected
*       using the coefficients in CCS4 describing the interference.
*
*  FORTRAN name: ypol3
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  SDAS:
*
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Sept 89 D. Lindler      Designed and coded
*       2       Jun 90  D. Lindler      Scaling added
*     2.1	Apr 93	H. Bushouse	Declare local variables
*	3	Jun 94	H. Bushouse	Retain negative IQUV values; set PL,
*					PC, and Theta=0 when I < 0.
*     3.1	Sep 94	H. Bushouse	Initialize XSC to 1.
*-------------------------------------------------------------------------------
*
* Inputs:
*       nx - number of data points
*       maxlen - max number of data points
*       wave - wavelength vector
*       coef - coefficients describing the interference
*               coef(1) = a - amplitude coef. 1
*                    2  = b - amplitude coef. 2
*                    3  = c1 - wavelength coef 1
*                    4  = c2 -     "       "   2
*                    5  = c3 -     "       "   3
*                    6  = c4 -     "       "   4
*                    7  = c5 -     "       "   5
*	pgaper - PA_APER From science data file (in radians)
*
* Input/Outputs:
*       res - polarimetry results
*-------------------------------------------------------------------------------
	IMPLICIT NONE
C
        INTEGER NX,MAXLEN
        DOUBLE PRECISION COEF(7),PGAPER
        REAL RES(MAXLEN,14),WAVE(*)
C
C local variables
C
        DOUBLE PRECISION A,B,C1,C2,C3,C4,C5,W,I,Q,U,VARI,VARQ,VARU,VARP,
     *          PL,THETA,COS2T,SIN2T,COS2,SIN2,P2,I2,W2,W3,VART,
     *          AMP,PHASE,THCORR,PI,Q2U2
	REAL SCALE, XSC
        INTEGER IPIX,J,SIGJ
        DATA PI/3.1415926536/
C-------------------------------------------------------------------------------
        A = COEF(1)
        B = COEF(2)
        C1 = COEF(3)
        C2 = COEF(4)
        C3 = COEF(5)
        C4 = COEF(6)
        C5 = COEF(7)
C
C loop on channels
C
        DO 500 IPIX = 1,NX
               W = WAVE(IPIX)
               IF (W.LE.0.0) GO TO 400
C
C Determine scale factor
C
               SCALE=1.0
               XSC = 1.0
               DO 40 J=1,4
                  SIGJ=J+4
                  IF(RES(IPIX,SIGJ).GT.0.0) THEN
                     IF(RES(IPIX,SIGJ).LT.1.0E-08) XSC=1.0E+12
                     IF(RES(IPIX,SIGJ).LT.1.0E-16) XSC=1.0E+20
                     IF(RES(IPIX,SIGJ).LT.1.0E-24) XSC=1.0E+28
                     IF(XSC.GT.SCALE) SCALE=XSC
                  ENDIF
40             CONTINUE
C
C Retrieve necessary values from input array
C (Allow negative values of I; HAB June-94)
C
               I = SCALE*RES(IPIX,1)
               Q = SCALE*RES(IPIX,2)
               U = SCALE*RES(IPIX,3)
               Q2U2 = Q*Q+U*U
c              IF(I.LT.1.0E-08) GO TO 400
               IF(ABS(I).LT.1.0E-8) GO TO 400
c              VARI  = SCALE*SCALE*RES(IPIX,5)**2
               VARI  = (SCALE*RES(IPIX,5))**2
               PL    = RES(IPIX,9)
c              IF(PL.LT.1.0E-08) GO TO 400
               VARP  = RES(IPIX,12)**2
               THETA = RES(IPIX,11)
               VART  = RES(IPIX,14)**2
C
C Correct Theta for wavelength-dependent oscillation and to refer
C to position angle on the sky
C
               AMP = A*W + B
               W2 = W*W
               W3 = W*W2
               PHASE = (C1/W3 + C2/W2 + C3/W + C4 + C5*W)
               THCORR = AMP*DCOS(PHASE)
               THETA = THETA - THCORR + PGAPER
C
C Calculate new Q and U consistent with corrected Theta
C (modified Jun-94 HAB to use old Q & U values instead of PL)
C
               COS2T = DCOS(2.0D0*THETA)
               SIN2T = DSIN(2.0D0*THETA)
c              Q = PL*I*COS2T
               Q = SQRT(Q2U2) * COS2T
c              U = PL*I*SIN2T
               U = SQRT(Q2U2) * SIN2T
C
C Compute new variances for Q and U
C (modified Jun-94 HAB to use old Q & U values instead of PL)
C
               I2 = I*I
               P2 = PL*PL
               COS2 = COS2T*COS2T
               SIN2 = SIN2T*SIN2T
c              VARU = 4.0*I2*P2*COS2*VART + I2*SIN2*VARP + P2*SIN2*VARI
               VARU = 4.0*Q2U2 *COS2*VART + I2*SIN2*VARP + 
     *                Q2U2*SIN2*VARI/I2
c              VARQ = 4.0*I2*P2*SIN2*VART + I2*COS2*VARP + P2*COS2*VARI
               VARQ = 4.0*Q2U2* SIN2*VART + I2*COS2*VARP + 
     *                Q2U2*COS2*VARI/I2
C
C Rescale and store I, Q, and U
C
               RES(IPIX,1) = I/SCALE
               RES(IPIX,2) = Q/SCALE
               RES(IPIX,3) = U/SCALE
C
C Rescale and store standard errors for I, Q, and U
C
               RES(IPIX,5) = SQRT(VARI)/SCALE
               RES(IPIX,6) = SQRT(VARQ)/SCALE
               RES(IPIX,7) = SQRT(VARU)/SCALE
C
C Store theta
C
               RES(IPIX,11) = THETA
C
C Check for negative values of I; set PL, PC, and Theta=0 (HAB June-94)
C
               IF (I .LT. 0.0) THEN
		   DO 300 J = 9, 14
		      RES(IPIX,J) = 0.0
300		   CONTINUE
	       END IF
C
	       GO TO 500
C
C Come here when skipping a channel; set results to zero
C
400            DO 450 J=1,14
                      RES(IPIX,J)=0.0
450            CONTINUE
C
C Next channel
500     CONTINUE
C
        RETURN
        END
