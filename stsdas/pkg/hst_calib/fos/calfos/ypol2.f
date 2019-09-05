        SUBROUTINE YPOL2(NX,MAXLEN,NSHIFT,RES1,RES2)
*
*  Module number:
*
*  Module name: YPOL2
*
*  Keyphrase:
*  ----------
*       Combine results of 2 pass directions
*
*  Description:
*  ------------
*       This routine combines the two pass directions by computing
*       a weighted average of the stokes parameters and then computing
*       new values for the circular and linear polarization and
*       polarization angle.
*
*  FORTRAN name: YPOL2
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
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
*       1       Sept 89  D. Lindler     Designed and coded
*	2	Jun 90	 D. Lindler	Scaling added
*       2.1     Sep 92   D. Bazell      Fix indexing to sig array
*       2.2     Feb 93   J. Eisenhamer  Fix bug that forced the V parameter
*                                       to be positive due to missing 'abs()'.
*	2.3	Mar 94	 H. Bushouse	Fix bug that allowed scaled IQUV values
*					to remain set in output RES1 array when
*					skipping a pixel due to low flux.
*	3	Jun 94	 H. Bushouse	Allow negative I values; set PL, PC,
*					Theta=0 when I < 0.
*	3.1	Sep 94	 H. Bushouse	Initialize XSC to 1.
*       3.2     Mar 97   M. De La Pena  Removed PGAPER parameter - not used.
*-------------------------------------------------------------------------------
*
* Inputs:
*       nx - number of spectral data points
*       maxlen - max number of data points allowed
*       nshift - wavelength shift in pixels of second pass dir. w.r.t.
*               first pass dir.
*       res2 - polarization results for second pass direction
*
* Input/output:
*       res1 - polarization results for first pass direction. On
*               output, it contains the combined results
*
*------------------------------------------------------------------------------
	IMPLICIT NONE
C
        INTEGER NX,MAXLEN,NSHIFT
        REAL RES1(MAXLEN,14),RES2(MAXLEN,14)
C
C Local variables
C
        DOUBLE PRECISION I,Q,U,V,VARI,VARQ,VARU,VARV,Q2,I2,U2,V2,
     *          PL,THETA,PC,Q2U2,VARL,VART,VARC,
     *          WSUM,WGHT1,WGHT2,SIG1,SIG2
	REAL SCALE, XSC
        INTEGER IPIX,IPIX2,J,JSIG
C-----------------------------------------------------------------------------
C
C loop on channels
C
        DO 500 IPIX = 1,NX
               IPIX2 = IPIX + NSHIFT
C                                    --->position in second pass dir
               IF((IPIX2.LT.1).OR.(IPIX2.GT.NX))GO TO 400
C
C Determine scale factor
C
               SCALE=1.0
               XSC = 1.0
               DO 30 J=1,4
                  JSIG = J + 4
                  SIG1 = RES1(IPIX, JSIG)
                  SIG2 = RES2(IPIX2,JSIG)
                  IF((SIG1.GT.0.0).AND.(SIG2.GT.0.0)) THEN
                     IF(SIG1.LT.1.0E-8) XSC=1.0E+12
                     IF(SIG2.LT.1.0E-8) XSC=1.0E+12
                     IF(SIG1.LT.1.0E-16) XSC=1.0E+20
                     IF(SIG2.LT.1.0E-16) XSC=1.0E+20
                     IF(SIG1.LT.1.0E-24) XSC=1.0E+28
                     IF(SIG2.LT.1.0E-24) XSC=1.0E+28
                     IF(XSC.GT.SCALE) SCALE=XSC
                  ENDIF
30             CONTINUE
C
C Scale data and compute weighted Stoke's parameters
C
               DO 50 J=1,4
                    JSIG = J+4
                    SIG1 = SCALE*RES1(IPIX, JSIG)
                    SIG2 = SCALE*RES2(IPIX2,JSIG)
                    RES1(IPIX,J)  = SCALE*RES1(IPIX, J)
                    RES2(IPIX2,J) = SCALE*RES2(IPIX2,J)
                    IF((SIG1.EQ.0.0).OR.(SIG2.EQ.0.0))THEN
                          RES1(IPIX,JSIG)=0.0
                          RES1(IPIX,J)=0.0
                    ELSE
                          WGHT1 = 1.0/(SIG1*SIG1)
                          WGHT2 = 1.0/(SIG2*SIG2)
                          WSUM = WGHT1 + WGHT2
                          RES1(IPIX,J) = (RES1(IPIX,J)*WGHT1 +
     *                                  RES2(IPIX2,J)*WGHT2)/WSUM
                          RES1(IPIX,JSIG) = SQRT(1.0/WSUM)
                    ENDIF
50             CONTINUE
C
C Extract I, Q, U and V
C
               I = RES1(IPIX,1)
               Q = RES1(IPIX,2)
               U = RES1(IPIX,3)
               V = RES1(IPIX,4)
C
C Extract variances of I, Q, U and V
C
               VARI = RES1(IPIX,5)**2
               VARQ = RES1(IPIX,6)**2
               VARU = RES1(IPIX,7)**2
               VARV = RES1(IPIX,8)**2
C
C Zero storage array (Bug fix Mar-94: Zero ALL output locations, not just 9-14)
C
c              DO 60 J=9,14
               DO 60 J=1,14
                  RES1(IPIX,J)=0.0
60             CONTINUE
C
C Skip processing if variance of I is zero
C
               IF(VARI.EQ.0.0)GO TO 500
C
C Check for near-zero values in I, Q, and U
C (Allow negative values for I; HAB June-94)
C
c              IF((I.LT.1.0E-8).OR.(ABS(Q).LT.1.0E-8).OR.
               IF((ABS(I).LT.1.0E-8).OR.(ABS(Q).LT.1.0E-8).OR.
     *            (ABS(U).LT.1.0E-8)) GO TO 500
C
C Store I, Q, and U
C
               RES1(IPIX,1) = I/SCALE
               RES1(IPIX,2) = Q/SCALE
               RES1(IPIX,3) = U/SCALE
C
C Store standard errors for I, Q, and U
C
               RES1(IPIX,5) = SQRT(VARI)/SCALE
               RES1(IPIX,6) = SQRT(VARQ)/SCALE
               RES1(IPIX,7) = SQRT(VARU)/SCALE
C
C Compute linear polarization and theta
C
               Q2 = Q**2
               U2 = U**2
               I2 = I**2
               PL    = DSQRT(Q2+U2)/I
               THETA = 0.5 * DATAN2(U,Q)
C
C Compute variances for the linear polarization and theta
C
               Q2U2 = Q2+U2
               VARL = VARQ/I2*Q2/Q2U2 + VARU/I2*U2/Q2U2
     *              + VARI/I2*Q2U2/I2
               VART = (U2/Q2U2*VARQ/Q2U2 + Q2/Q2U2*VARU/Q2U2)/4.0D0
C
C Store the linear polarization and theta
C
               RES1(IPIX,9)  = PL
               RES1(IPIX,11) = THETA
C      
C Store standard errors for the linear polarization and theta
C
               RES1(IPIX,12) = SQRT(VARL)
               RES1(IPIX,14) = SQRT(VART)
C
C Compute the circular polarization
C (Allow negative values of V; JE Feb-93)
C
c              IF(V.LT.1.0E-08) GO TO 500
               IF(ABS(V).LT.1.0E-8) GO TO 200
               V2 = V**2
               PC = V/I
C
C Compute variance of the circular polarization
C
               VARC = VARV/I2 + VARI/I2*V2/I2
C
C Store V
C
               RES1(IPIX,4) = V/SCALE
C
C Store standard error in V
C
               RES1(IPIX,8) = SQRT(VARV)/SCALE
C
C Store the circular polarization
C
               RES1(IPIX,10) = PC
C
C Store standard error of circular polarization
C
               RES1(IPIX,13) = SQRT(VARC)
C
C Check for negative values of I; set PL, PC, Theta=0 (HAB June-94)
C
200            IF (I .LT. 0.0) THEN
		   DO 300 J = 9, 14
		      RES1(IPIX,J) = 0.0
300		   CONTINUE
	       END IF
C
               GO TO 500
C
C Come here when skipping a channel (set all results to zero)
C
400            CONTINUE
               DO 450 J=1,14
                  RES1(IPIX,J) = 0.0
450            CONTINUE
C
C Next channel
500     CONTINUE
C
        RETURN
        END
