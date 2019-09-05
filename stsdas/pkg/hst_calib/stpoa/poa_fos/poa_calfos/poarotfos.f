      SUBROUTINE POAROTFOS(ID,PLNG,PLAT,PRHST,PDHST,RAV1,DECV1,PAV3,
     1   PBN,PBE,PBD,PBV1,PBV2,PBV3,PBDX,PBDY,PBDZ,
     2   PVDX,PVDY,PVDZ)
  
c 
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  ------------------------------------------------------------------------
*
*  Module number:
*
*  Module name: poarotfos
*
*  Keyphrase:
*  ----------
*       POA/FOS  - B-field projections
*
*  Description:
*  ------------
*  Perform the POA projection of B-Field from N,E,D to x,y,z in Digicon
*  and the velocity components from V123
*
*  History:
*  --------
*  Version   Date        Author          Description
*      1.0       May 00  M. Rosa         POA calibration code
*      1.1       Dec 00  A. Alexov       PVX,Y,Z passed to POAROTFOS() via
*                                        addition of POA global params
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IMPLICIT NONE

C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
c      INTEGER STDOUT
c      PARAMETER (STDOUT = 1)
c      INTEGER STDERR
c      PARAMETER (STDERR = 2)

c      INTEGER ISTAT

c the POA orbital computation yields new group parameters 
      REAL*8 MIDTIMP,POAXP,POAYP,POAZP,VXP,VYP,VZP,BNP,BEP,BDP,
     a       BV1P,BV2P,BV3P,BDXP,BDYP,BDZP,YGMPXSCL,YGMPYSCL,
     b       YOFFXP,YOFFYP,YYBASE0,YYBSXSCL,YMEANTMP,YTMPXSCL,YAPGRTX0,
     c       YXCEN,GMSTP,GLNGP,GLATP,MLNGP,MLATP,ALNGP,ALATP,LSHP,
     d       PVX,PVY,PVZ
      COMMON /POA_P/ MIDTIMP,POAXP,POAYP,POAZP,VXP,VYP,VZP,BNP,BEP,BDP,
     *       BV1P,BV2P,BV3P,BDXP,BDYP,BDZP,YGMPXSCL,YGMPYSCL,
     *       YOFFXP,YOFFYP,YYBASE0,YYBSXSCL,YMEANTMP,YTMPXSCL,YAPGRTX0,
     *       YXCEN,GMSTP,GLNGP,GLATP,MLNGP,MLATP,ALNGP,ALATP,LSHP,
     *       PVX,PVY,PVZ

C LOCAL VARIABLES ------------------------------------------------------
c      CHARACTER*80 CONTXT

      INTEGER*4 ID 
      REAL*8 RAV1,DECV1,PAV3 
      REAL*8 PLNG,PLAT,PRHST,PDHST
      REAL*8 PBN,PBE,PBD,PBV1,PBV2,PBV3,PBDX,PBDY,PBDZ
      REAL*8 PVDX,PVDY,PVDZ
      REAL*8 BLOC(3), BDUM(3), TILT, PSCORR
      TILT = 0.0D0      ! tilt 
      PSCORR = 0.0D0    ! POSANGL correction 

      BLOC(1) = PBN
      BLOC(2) = PBE
      BLOC(3) = PBD
c rotate BN.BE.BD into equatorial plane
      call cartroty (-PLAT-90.D0,bloc,bdum) !x(N)y(E) equator, z(D)towards N
c rotate to HST position
      call cartrotz (-RAV1+PRHST,bdum,bloc)  !proj x(N), v1 on equator
      call cartroty (DECV1,bloc,bdum)  !x(N) now parallel to V1 in DEC also
      call cartrotx (PAV3-PSCORR,bdum,bloc)  !xyz now parallel to v1,v2,v3

c For check with AEDP, magnetometers are -v1,-v2,+v3, map into -b1,-b2,b3 !!!
      PBV1 = BLOC(1) 
      PBV2 = BLOC(2) 
      PBV3 = BLOC(3) 

c x points in optical axis HST outward (=v1)          
      call cartrotx (45.0D0,bloc,bdum)  ! 
      call cartrotz (23.0D0,bdum,bloc)  ! z,x in FOS optical plane 
      IF(ID.eq.1) call cartroty (-7.9180D0,bloc,bdum) !red  x digicon axis 
      IF(ID.eq.2) call cartroty (7.9180D0,bloc,bdum)  !blue
      call cartrotx (-180.0D0,bdum,bloc)  ! flip dir of y and z
      call cartroty (90.0D0,bloc,bdum)    ! z to diodes
      call cartrotz (TILT,bdum,bloc)  ! 

      PBDX = BLOC(1)
      PBDY = BLOC(2)
      PBDZ = BLOC(3)

C Now velocity components in detector coordinates
C  HST is already at RHST,DECHST or GLAT, which is the point where VX,VY,VZ
C  is evaluated, and this is already in RA,DEC,z system. 
      BDUM(1) = PVX
      BDUM(2) = PVY
      BDUM(3) = PVZ
      call cartrotz (-RAV1,bdum,bloc) !ORG!proj of x(N) and v1 on equator
      call cartroty (DECV1,bloc,bdum)  !x(N) now parallel to V1 in DEC also
      call cartrotx (PAV3-pscorr,bdum,bloc)  !xyz now parallel to v1,v2,v3


C adding temporary STDOUT section, for velocities in m/sec
C
c      WRITE(CONTXT,90) (BDUM(1))/1000, (BDUM(2)/1000), (BDUM(3))/1000
c90    FORMAT(' POAROTFOS:  BDUM(1)=', F8.3, ', BDUM(2)=', F8.3,
c     * ', BDUM(3)=', F8.3)
c      CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
      PVX = BDUM(1)/1000
      PVY = BDUM(2)/1000
      PVZ = BDUM(3)/1000

C
C end of temporary STDOUT section
 

      call cartrotx (45.0D0,bloc,bdum)  !
      call cartrotz (23.0D0,bdum,bloc)  !
      IF(ID.eq.1) call cartroty (-7.9180D0,bloc,bdum) !red  x digicon axis 
      IF(ID.eq.2) call cartroty (7.9180D0,bloc,bdum)  !blue side
      call cartrotx (-180.0D0,bdum,bloc)  !
      call cartroty (90.0D0,bloc,bdum)  !
      call cartrotz (TILT,bdum,bloc)  !

      PVDX = BLOC(1)
      PVDY = BLOC(2)
      PVDZ = BLOC(3)

      RETURN
      END
c-- end of POAROTFOS

