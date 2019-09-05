        SUBROUTINE VELSUN (
*
*  inputs
*
     :                  MJD,
*
*  outputs
*
     :                  VEL)
*
*  Module number:
*
*  Module name:
*
*  Keyphrase:
*  ----------
*  calculate the heliocentric velocities of the Earth in the J2000 coordinate
*  system
*
*  Description:
*  ------------
*  Use the low-precision formulae for the Sun's coordinates described in
*  The Astronomical Almanac (1984), page C24.  The velocities are obtained
*  by taking derivatives of the coordinates.  The velocity vector is in the
*  equatorial coordinate system of the epoch J2000.
*  This algorithm does not include Earth-Moon motion, Sun-barycenter motion,
*  light time correction from the Earth to the Sun, but should be accurate
*  to ~0.025 km/sec within the epoch range of 1900 to 2100 AD.
*
*  Double precision calculations are used throughout.
*
*  Page numbers refered in this code are from the 1984 Almanac.
*
*  FORTRAN name: VELSUN.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       None
*  SDAS:
*       None
*  Others:
*       None
*
*  History:
*  --------
*  Version      Date        Author          Description
*     1       05-05-89     J.-C. HSU        coding
*
*-------------------------------------------------------------------------------
*
*== input:
*                                       --modified Julian date
        DOUBLE PRECISION        MJD
*
*== output:
*                                       --heliocentric velocity vector of the
*                                       --Earth in J2000 equatorial coordinates
        REAL                    VEL(3)
*
*== local:
*
        DOUBLE PRECISION                TDAYS,
*                                       --mean ecliptic longitudte of the Earth
     :                                  L0, L1, L2, L3, LAMBDA, DLAM,
*                                       --mean anomaly
     :                                  G0, G1, G, DG,
*                                       --Earth's distance from the Sun
     :                                  R0, R1, R2, R, DR,
*                                       --obliquity
     :                                  OBL,
     :                                  SING, COSG, SIN2G, COS2G,
     :                                  SINLAM, COSLAM,
*                                       --constants
     :                                  AU, RADN,
*                                       --modified Julian date for J2000
     :                                  MJ2000
*                                       --page S7
        PARAMETER (AU = 1.4959787D8)
*
        PARAMETER (RADN = 57.29577951308232088 D0)
        PARAMETER (MJ2000 = 51544.5 D0)
*----------------------------------------------------------------------------
*
*  Coefficients for mean ecliptic longitude of the Earth, corrected for
*  abberation;  time should be days past J2000. page C24
*
        L0 = (280.460 - 180.) / RADN
*
*  second term of L1 is the precession, page S19
*
        L1 = (0.9856474D0 - 50.29 / (3600. * 365.25)) / RADN
        L2 = 1.915 / RADN
        L3 = 0.02 / RADN
*
*  Coefficients for mean anomaly, page C24
*
        G0 = 357.528D0 / RADN
        G1 = 0.9856003D0 / RADN
*
*  Coefficients for distance from the Sun in km. page C24
*
        R0 =  1.00014D0 * AU
        R1 = -0.01671D0 * AU
        R2 = -0.00014D0 * AU
*
*  days since J2000
*
        TDAYS = MJD - MJ2000
*
*  obliquity of ecliptic at J2000
*
        OBL = 23.439291 / RADN
*
*  mean anomaly (radians) and its derivative (radians / day).
*
        G = G0 + G1 * TDAYS
        DG = G1
*
        SING = SIN (G)
        COSG = COS (G)
        SIN2G = 2. * SING * COSG
        COS2G = COSG*COSG - SING*SING
*
*  ecliptic longitude of the Earth (radians); distance from the Sun (km).
*
        LAMBDA = L0 + L1 * TDAYS + L2 * SING + L3 * SIN2G
        R = R0 + R1 * COSG + R2 * COS2G
*
*       Derivatives in radians / sec; km / sec.
*
        DLAM = (L1 + L2 * DG * COSG + 2. * L3 * DG * COS2G) / 86400.
        DR = (-R1 * DG * SING - 2. * R2 * DG * SIN2G) / 86400.
*
        SINLAM = SIN (LAMBDA)
        COSLAM = COS (LAMBDA)
*
        VEL(1) =  DR * COSLAM - R * SINLAM * DLAM
        VEL(2) = (DR * SINLAM + R * COSLAM * DLAM) * COS (OBL)
        VEL(3) = (DR * SINLAM + R * COSLAM * DLAM) * SIN (OBL)
*
        RETURN
        END
