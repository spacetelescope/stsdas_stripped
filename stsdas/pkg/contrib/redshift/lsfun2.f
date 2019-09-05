      SUBROUTINE LSFUN2(M,N,XC,FVECC,FJACC,LJC)

      PARAMETER ( MMAX = 1000 )
      REAL X(MMAX),Y(MMAX)
      COMMON / COMXY / X,Y

      INTEGER M,N,LJC
      DOUBLE PRECISION XC(N),FVECC(M),FJACC(LJC,N)

      INTEGER I

      DO I = 1,M
          FVECC(I) = (XC(1)*DEXP(-((X(I)-XC(2))**2D0)/2D0/XC(3)/XC(3)))
     *               - Y(I)
          FJACC(I,1) = DEXP(-((X(I)-XC(2))**2D0)/2D0/XC(3)/XC(3))
          FJACC(I,2) = XC(1)*DEXP(-((X(I)-XC(2))**2D0)/2D0/XC(3)/XC(3))
     *                 * (X(I)-XC(2)) / XC(3) / XC(3)
          FJACC(I,3) = XC(1)*DEXP(-((X(I)-XC(2))**2D0)/2D0/XC(3)/XC(3))
     *                 * ((X(I)-XC(2))**2D0) / (XC(3)**3D0)
      ENDDO
      RETURN
      END

