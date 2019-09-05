        SUBROUTINE ZDOPSM(RESP,DOPLER,SRESP)
*
*  Module number:
*
*  Module name: ZDOPSM
*
*  Keyphrase:
*  ----------
*       Smooth response by doppler function
*
*  Description:
*  ------------
*       This routine smooths a response curve by convolution with
*       the dopler function containing the percentage of time spent
*       at each deflection offset.
*
*  FORTRAN name: zdopsm.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*       none
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*-------------------------------------------------------------------------------
*
* INPUTS:
*       resp - raw response vector. 4800 points on 1 deflection unit spacing
*       dopler - 400 point vector giving percentage of time spent at each
*               deflection.  Position i is for a deflection offset of
*               i-200.
*
* OUTPUTS:
*       sresp - smoothed response vector
*
*------------------------------------------------------------------------------
        REAL RESP(4800),DOPLER(400),SRESP(4800)
C
C local variables
C
        INTEGER J1,J2,IOFF,I,J
        REAL W
C------------------------------------------------------------------------------
C
C Initialize output to all zeros
C
        DO 10 J=1,4800
10              SRESP(J)=0.0
C
C Loop on wieghts in DOPLER
C
        DO 100 I=1,400
                W = DOPLER(I)
C                                    --->Weight
                IF ( W .EQ. 0) GO TO 100
                IOFF = I-200
C                                    --->offset
                J1 = 1+IOFF
                IF(J1.LT.1) J1 = 1
                J2 = 4800+IOFF
                IF(J2.GT.4800) J2 = 4800
                DO 50 J=J1,J2
                        SRESP(J) = SRESP(J) + RESP(J-IOFF)*W
50              CONTINUE
100     CONTINUE
        RETURN
        END
