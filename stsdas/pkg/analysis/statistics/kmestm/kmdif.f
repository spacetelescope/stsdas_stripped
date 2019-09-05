C
C      **********************************************************************
C      ******************** SUBROUTINE KMDIF ********************************
C      **********************************************************************
C
       SUBROUTINE KMDIF(S, ZU, IU, NTOT, START, BINSIZ, NBIN,
     +                  F, BS, BL, DIFF)

C
C      *       THIS SUBROUTINE COMPUTES AND PRINTS THE DIFFERENTIAL KM      *
C      *       ESTIMATOR BASED ON WARDLE AND KNAPP, 'THE STATISTICAL        *
C      *       DISTRIBUTION OF THE NEUTRAL-HYDROGEN CONTENT OF S0 GALAXIES',*
C      *       ASTRN. J., 91:23 1986.                                       *
C      *                                                                    *
C      *       INPUT     S(L)  :  PL ESTIMATOR                              *
C      *                ZU(I)  :  UNCENSORED DATA POINTS                    *
C      *                 IU    :  NUMBER OF UNCENSORED DATA POINTS          *
C      *                NTOT   :  TOTAL NUMBER OF DATA POINTS,=IU+IC.       *
C      *                START  :  STARTING VALUE OF THE FIRST BIN           *
C      *                BINSIZ :  WIDTH OF THE BIN                          *
C      *                NBIN   :  NUMBER OF BINS                            *
C      *                                                                    *
C      *      OUTPUT                                                        *
C      *                F(I)   :  MASS OF THE I TH DATA POINT               *
C      *               BS(J)   :  STARTING VALUE FOR THE BIN J              *
C      *               BL(J)   :  ENDING VALUE FOR THE BIN J                *
C      *               DIFF(J) :  DIFFERENTIAL KM ESTIMATOR AT BIN J        *
C      *                                                                    *
C      *                                                                    *
C
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)
C
       DIMENSION ZU(NTOT),S(NTOT),F(NTOT)
       DIMENSION BS(NBIN),BL(NBIN),DIFF(NBIN)
C
C       *    FIRST, COMPUTE THE MASS OF EACH POINT                          *
C
        F(1) = 1.0 -S(1)
       
        DO 610 I = 2, IU
           F(I) = DABS(S(I) - S(I-1))
  610   CONTINUE
C
C       *  SET THE BIN BOUNDARIES.                                          *
C
        DO 620 J = 1, NBIN
           BS(J) = START + BINSIZ*(J-1)
           BL(J) = START + BINSIZ*J
  620   CONTINUE

        I = 1
        J = 0

  630   J = J + 1
        DIFF(J) = 0.0

C
C      *       CHECK WHETHER THE I-TH POINT IS IN THE BIN                  *
C
  640  IF(J .LE. NBIN) THEN
         IF(ZU(I) .LT. BS(J)) THEN
            IF(I .GE. IU) THEN
               GOTO 630
            ENDIF
            I = I + 1
            GOTO 640
         ENDIF

C      *       COMPUTE THE DIFFERENTIAL KM                                 *
C
         IF(ZU(I) .GE. BL(J)) GOTO 630
         DIFF(J) = DIFF(J) + F(I)
   
         IF(I .LT. IU) THEN
            I = I + 1
            GOTO 640
         ENDIF
         GOTO 630
       ENDIF
C
       RETURN
       END
