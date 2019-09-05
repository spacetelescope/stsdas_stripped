        SUBROUTINE ZHELIO (
*
*  inputs
*
     :                  MJD, RA, DEC,
*
*  input/output
*
     :                  V,
*
*  outputs
*
     :                  STATUS)
*
*  Module number:
*
*  Module name:
*
*  Keyphrase:
*  ----------
*  compute velocity vector of the earth along the line of sight
*
*  Description:
*  ------------
*  The input right ascension and declination must be of the epoch J2000.
*  If not, must be precessed to J2000 first.
*
*  FORTRAN name: ZHELIO.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       VELSUN
*  SDAS:
*       None
*  Others:
*       None
*
*  History:
*  --------
*  Version      Date        Author          Description
*     1       05-05-89     J.-C. HSU        coding
*   1.1       May91        S.Hulbert        input time is now mjd	
*   1.2       Apr93        J. Eisenhamer    Removed unused variable OK.
*
*-------------------------------------------------------------------------------
*
*== input:
*                                       --input time (MJD)
        DOUBLE PRECISION        MJD   
*                                       --right ascension and declination in
*                                       --decimal degrees, must be J2000 position
        REAL                    RA, DEC
*
*== input/output:
*                                       --veclocity along the line of sight
        DOUBLE PRECISION        V
*
*== output:
*                                       --error status
        INTEGER                 STATUS
*
*== local:
*                                       --degrees per radian
        DOUBLE PRECISION        RADN
        PARAMETER               (RADN = 57.29577951308232088 D0)
*                                       --Earth's velocity vector
        REAL                    VEL(3)
*----------------------------------------------------------------------------
*
*  calculate the velocity vector of Earth's motion around the Sun, in equatorial
*  system of the epoch J2000 (in km/sec)
*
        CALL VELSUN (MJD,   VEL)
*
*  calculate the line of sight velocity
*
        V = VEL(1) * COS(DEC/RADN) * COS(RA/RADN) +
     :      VEL(2) * COS(DEC/RADN) * SIN(RA/RADN) +
     :      VEL(3) * SIN(DEC/RADN)
*  Clean end.
        STATUS = 0
        GO TO 1000
*
  999   CONTINUE
*
 1000   RETURN
        END
