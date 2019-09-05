        SUBROUTINE BFIELD(MJDTIM,ALT,LNG,LAT,NSPEC,B,ISTAT)
*
*  Module number:
*
*  Module name: BFIELD
*
*  Keyphrase: 
*  ----------
*		Compute earth's magnetic field at HST position
*
*  Description: 
*  ------------
*		Compute the three component (north, east, down) earth's
*		magnetic field strength (in gauss) at a series of
*		HST positions
*		Use the formalization given by Wertz
*
*  FORTRAN name: BFIELD.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments

*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 91	    S. Hulbert      Designed and coded
*	1.1	Aug 93	    H. Bushouse	    Added ISTAT to argument list
*-------------------------------------------------------------------------------
*
* INPUTS:
*	mjdtime - array of modified julian dates
*	alt - array of altitudes of HST (kilometers)
*	long - array of longitudes of HST (degrees)
*	lat - array of latitudes of HST (degrees)
*	nspec - number of array elements
*
* INPUT/OUTPUT:
*	b - magnetic field (3 components, gauss)
*
* OUTPUT:
*       istat - error status
*
*----------------------------------------------------------------------------
        INTEGER NSPEC, ISTAT
        DOUBLE PRECISION MJDTIM(NSPEC), ALT(NSPEC)
        DOUBLE PRECISION LNG(NSPEC), LAT(NSPEC), B(3,NSPEC)
C----------------------------------------------------------------------------
        REAL REARTH
        PARAMETER (REARTH = 6371.2)
        DOUBLE PRECISION PI
        PARAMETER (PI = 3.1415926535898)
C----------------------------------------------------------------------------
C local variables
C
        DOUBLE PRECISION G0(9,9), GDOT(9,9)
        DOUBLE PRECISION H0(9,9), HDOT(9,9)
        DOUBLE PRECISION S(9,9), K(9,9)
C
        DOUBLE PRECISION CSMPHI(9), SNMPHI(9), P(9,9), DPDTHE(9,9)
        DOUBLE PRECISION G(9,9), H(9,9)
        DOUBLE PRECISION RADIUS, DELYR, LAT0, LNG0, COELV
        DOUBLE PRECISION COSTHE, SINTHE, COSPHI, SINPHI
        DOUBLE PRECISION BR, BTHE, BPHI
        DOUBLE PRECISION SIGR, SIGTHE, SIGPHI
        DOUBLE PRECISION CONST, RATIO
        INTEGER I, N, M, NM1, MM1
C
C igrf coefficients 
C
        DATA G0/0., -30186., -1898., 1299., 951., -204., 46., 66., 11., 
     $        0., -2036., 2997., -2144., 807., 368., 57., -57., 13., 
     $        0., 0., 1551., 1296., 462., 275., 15., -7., 3., 
     $        0., 0., 0., 805., -393., -20., -210., 7., -12., 
     $        0., 0., 0., 0., 235., -161., -1., -22., -4., 
     $        0., 0., 0., 0., 0., -38., -8., -9., 6., 
     $        0., 0., 0., 0., 0., 0., -114., 11., -2., 
     $        0., 0., 0., 0., 0., 0., 0., -8., 9., 
     $        0., 0., 0., 0., 0., 0., 0., 0., 1./
        DATA GDOT/0.0, 25.6, -24.9, -3.8, -0.2, 0.3, 0.2, 0.0, 0.2, 
     $        0.0, 10.0, 0.7, -10.4, -2.0, -0.7, 0.5, 0.0, 0.3, 
     $        0.0, 0.0, 4.3, -4.1, -3.9, 1.1, 2.0, 0.0, 0.0, 
     $        0.0, 0.0, 0.0, -4.2, -2.1, -1.6, 2.8, 0.6, 0.2, 
     $        0.0, 0.0, 0.0, 0.0, -3.1, -0.5, 0.0, 0.9, -0.4, 
     $        0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.9, 0.3, -0.3, 
     $        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.1, 0.3, 0.6, 
     $        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.5, -0.3, 
     $        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.1/
        DATA H0/0., 0., 0., 0., 0., 0., 0., 0., 0., 
     $        0., 5735., -2124., -361., 148., 39., -23., -68., 4., 
     $        0., 0., -37., 249., -264., 142., 102., -24., -15., 
     $        0., 0., 0., -253., 37., -147., 88., -4., 2., 
     $        0., 0., 0., 0., -307., -99., -43., 11., -19., 
     $        0., 0., 0., 0., 0., 74., -9., 27., 1., 
     $        0., 0., 0., 0., 0., 0., -4., -17., 18., 
     $        0., 0., 0., 0., 0., 0., 0., -14., -6., 
     $        0., 0., 0., 0., 0., 0., 0., 0., -19./
        DATA HDOT/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
     $        0.0, -10.2, -3.0, 6.9, 5.0, 1.2, -0.5, -1.4, -0.2, 
     $        0.0, 0.0, -18.9, 2.5, 0.8, 2.3, -0.1, -0.1, -0.4, 
     $        0.0, 0.0, 0.0, -5.0, 1.7, -2.0, -0.2, 0.3, -0.2,
     $        0.0, 0.0, 0.0, 0.0, -1.0, 1.3, -1.3, 0.3, -0.3, 
     $        0.0, 0.0, 0.0, 0.0, 0.0, 1.1, 0.7, -0.7, 0.4, 
     $        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.7, 0.1, -0.3, 
     $        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.8, -0.6, 
     $        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3/
C
C normalization coefficients 
C
        DATA S/ 
     $   1.0000000,  1.0000000,  1.5000000,  2.5000000,  4.3750000,
     $   7.8750000, 14.4375000, 26.8125000, 50.2734375,  0.0000000,
     $   1.0000000,  1.7320508,  3.0618622,  5.5339861, 10.1665812,
     $  18.9031239, 35.4696007, 67.0312500,  0.0000000,  0.0000000,
     $   0.8660254,  1.9364917,  3.9131191,  7.6852126, 14.9442320,
     $  28.9608078, 56.0823669,  0.0000000,  0.0000000,  0.0000000,
     $   0.7905695,  2.0916500,  4.7062125,  9.9628220, 20.4783840,
     $  41.4195709,  0.0000000,  0.0000000,  0.0000000,  0.0000000,
     $   0.7395099,  2.2185299,  5.4568624, 12.3489304, 26.7362175,
     $   0.0000000,  0.0000000,  0.0000000,  0.0000000,  0.0000000,
     $   0.7015607,  2.3268139,  6.1744652, 14.8305855,  0.0000000,
     $   0.0000000,  0.0000000,  0.0000000,  0.0000000,  0.0000000,
     $   0.6716933,  2.4218245,  6.8652272,  0.0000000,  0.0000000,
     $   0.0000000,  0.0000000,  0.0000000,  0.0000000,  0.0000000,
     $   0.6472598,  2.5068266,  0.0000000,  0.0000000,  0.0000000,
     $   0.0000000,  0.0000000,  0.0000000,  0.0000000,  0.0000000,
     $   0.6267067/
        DATA K/
     $   0.0000000,  0.0000000,  0.3333333,  0.2666667,  0.2571429,
     $   0.2539683,  0.2525252,  0.2517483,  0.2512821,  0.0000000,
     $   0.0000000,  0.0000000,  0.2000000,  0.2285714,  0.2380952,
     $   0.2424242,  0.2447552,  0.2461538,  0.0000000,  0.0000000,
     $  -1.0000000,  0.0000000,  0.1428571,  0.1904762,  0.2121212,
     $   0.2237762,  0.2307692,  0.0000000,  0.0000000, -2.6666667,
     $  -0.3333333,  0.0000000,  0.1111111,  0.1616162,  0.1888112,
     $   0.2051282,  0.0000000,  0.0000000, -5.0000000, -0.8000000,
     $  -0.2000000,  0.0000000,  0.0909091,  0.1398601,  0.1692308,
     $   0.0000000,  0.0000000, -8.0000000, -1.4000000, -0.4571429,
     $  -0.1428571,  0.0000000,  0.0769231,  0.1230769,  0.0000000,
     $   0.0000000,-11.6666670, -2.1333334, -0.7714286, -0.3174603,
     $  -0.1111111,  0.0000000,  0.0666667,  0.0000000,  0.0000000,
     $ -16.0000000, -3.0000000, -1.1428572, -0.5238096, -0.2424242,
     $  -0.0909091,  0.0000000,  0.0000000,  0.0000000,-21.0000000,
     $  -4.0000000, -1.5714285, -0.7619048, -0.3939394, -0.1958042,
     $  -0.0769231/
C
C----------------------------------------------------------------------------
        DO 100 I = 1, NSPEC
C
C distance from center of earth    
C
            RADIUS = ALT(I) + REARTH
C
C years since 1975
C
            DELYR = (MJDTIM(I) - 46066.) / 365.25
C
C convert to radians
C
            LAT0 = LAT(I) * PI / 180.
            LNG0 = LNG(I) * PI / 180.
C
C calculate sine and cosine of coelevation and east longitude
C
            COELV = PI/2. - LAT0
            COSTHE = COS(COELV)
            SINTHE = SIN(COELV)
            COSPHI = COS(LNG0)
            SINPHI = SIN(LNG0)
C
C calculate g and h coefficients for current epoch
C
            DO 110 N = 1, 9
                DO 120 M = 1, 9
                    G(N,M) = (G0(N,M) + DELYR * GDOT(N,M)) * S(N,M)
                    H(N,M) = (H0(N,M) + DELYR * HDOT(N,M)) * S(N,M)
120                CONTINUE
110            CONTINUE
C
C calculate cos(m*phi)'s and sin(m*phi)'s 
C
            CSMPHI(1) = 1.0        
            SNMPHI(1) = 0.0
            DO 140 M = 2, 9
                CSMPHI(M) = CSMPHI(M-1)*COSPHI - SNMPHI(M-1)*SINPHI 
                SNMPHI(M) = SNMPHI(M-1)*COSPHI + CSMPHI(M-1)*SINPHI 
140            CONTINUE
C
C calculate legendre functions
C For P(n-2) when n==2 use P(n-2)==0.
C
            P(1,1) = 1.0
            DPDTHE(1,1) = 0.0
            DO 150 N = 2, 9
                P(N,N) =  SINTHE * P(N-1,N-1)
                DPDTHE(N,N) = SINTHE * DPDTHE(N-1,N-1) +
     $                        COSTHE * P(N-1,N-1)
                DO 160 M = 1, N 
                    IF (M .NE. N) THEN
                       IF(N.GT.2) THEN
                          P(N,M) = COSTHE * P(N-1,M) - K(N,M) * P(N-2,M)
                          DPDTHE(N,M) = COSTHE * DPDTHE(N-1,M) -
     $                         SINTHE * P(N-1,M) -
     $                         K(N,M) * DPDTHE(N-2,M)
                       ELSE
                          P(N,M) = COSTHE * P(N-1,M)
                          DPDTHE(N,M) = COSTHE * DPDTHE(N-1,M) -
     $                         SINTHE * P(N-1,M)
                       ENDIF
                    ENDIF
160                CONTINUE
150             CONTINUE
C
C calculate magnetic field components in (r,theta,phi) coordinates
C
            BR = 0.0
            BTHE = 0.0
            BPHI = 0.0
            RATIO = REARTH/RADIUS
            DO 180 N = 2, 9 
                NM1 = N - 1
                SIGR = 0.0
                SIGTHE = 0.0
                SIGPHI = 0.0
                DO 190 M = 1, N 
                    MM1 = M - 1
                    SIGR = SIGR + (G(N,M)*CSMPHI(M) + 
     $                        H(N,M)*SNMPHI(M)) * P(N,M)
                    SIGTHE = SIGTHE + (G(N,M)*CSMPHI(M) + 
     $                        H(N,M)*SNMPHI(M)) * DPDTHE(N,M)
                    SIGPHI = SIGPHI + MM1 * (-G(N,M)*SNMPHI(M) + 
     $                        H(N,M)*CSMPHI(M)) * P(N,M)
190                CONTINUE
                CONST = RATIO**(NM1+2)
                BR = BR + CONST * (NM1+1) * SIGR
                BTHE = BTHE + CONST * SIGTHE
                BPHI = BPHI + CONST * SIGPHI
180            CONTINUE
            BTHE = -BTHE
            BPHI = -BPHI / SINTHE
C
C convert to N,E,down (assume latitude = geodetic latitude)
C in units of gauss
C
C north
            B(1,I) = -1.0D-5 * BTHE
C east
            B(2,I) = 1.0D-5 * BPHI
C down
            B(3,I) = -1.0D-5 * BR
C
100     CONTINUE
C
	ISTAT = 0
        RETURN
        END
