      SUBROUTINE POAGIMP(PMIDTIM,RAV1,DECV1,PAV3,ID,ISTAT)
c 
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  ------------------------------------------------------------------------
*
*  Module number:
*
*  Module name: poagimp
*
*  Keyphrase:
*  ----------
*       POA - improved GIMP offsets calculations
*
*  Description:
*  ------------
c  The POA upgrade to determine better GIMP offsets
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       May 00  M. Rosa         POA calibration code
c      1.1       Dec 00  A. Alexov       PVX,Y,Z passed to POAROTFOS(),
c                                        VX,VY,VZ separated from COMMON 
c                Dec 00  A. Alexov       Added PVX, PVY, PVZ to POA global
c      1.2       Jun 01  A. Alexov       Added IDS's common block for file
c                                        header access
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IMPLICIT NONE 
c        CHARACTER*80 CONTXT
      INTEGER ID,ISTAT
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C MR
      REAL*8 PMIDTIM,RAV1,DECV1,PAV3 
      REAL*8 PR,PX,PY,PZ
      REAL*8 PGMST,PLNG,PLAT,PRHST,PDHST
      REAL*8 PBV1,PBV2,PBV3,PVDX,PVDY,PVDZ
      REAL*8 PBG,PBN,PBE,PBD,PBDX,PBDY,PBDZ
      REAL*8  digx,digy,digz,etime
      REAL*8 ALAT,ALNG, SHELL, Y75, MLNG,MLAT, VX, VY, VZ
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

C
C Common block containing input/output file descriptions
C
C       IDS - file id numbers
C       GCOUNT - group counts
C       NAXIS - naxis
C       NAXIS1 - first dimensions
C       NAXIS2 - second dimensions
C       FILL - Fill values
C
        INTEGER IDS(30),NAXIS(30),NAXIS1(30),NAXIS2(30),GCOUNT(30)
        REAL FILL(30)
        COMMON /IOCOM/IDS,GCOUNT,NAXIS,NAXIS1,NAXIS2,FILL

c--------------------------------------------------------------------------
c compute spacecraft position at midpoint using NORAD epehemeris code
      CALL NORAD(PMIDTIM,PX,PY,PZ,PR,VX,VY,VZ,
     1             PLNG,PLAT,PRHST,PDHST,PGMST,ISTAT)
      IF(ISTAT.NE.0)THEN
              GO TO 999
      ENDIF

      PVX=VX
      PVY=VY
      PVZ=VZ
      Y75 = (PMIDTIM - 42413.0D0)/365.25D0+1975.D0 ! Time in years from 1975
c hand over some of the parameters to the POA keys 
      MIDTIMP = PMIDTIM
      GMSTP = PGMST
      POAXP = PX
      POAYP = PY
      POAZP = PZ
      VXP = PVX
      VYP = PVY
      VZP = PVZ
      GLNGP = PLNG
      GLATP = PLAT
c get the "dipole" magnetic coordinates (relevant for background scaling) 
      CALL GEOPOS (PLAT,PLNG,MLAT,MLNG,ISTAT)
      IF(ISTAT.NE.0)THEN
              GO TO 999
      ENDIF

      MLNGP = MLNG
      MLATP = MLAT
c model the B-field accurately using the GSFC IGRF code
      CALL SNEWBILCAL (PLNG,PLAT,PR,Y75,
     1           ALNG,ALAT,PBG,PBN,PBE,PBD,SHELL,ISTAT)
      IF(ISTAT.NE.0)THEN
              GO TO 999
      ENDIF
c 
c now here is the place to scale with a factor from actual magnetometer
c readings as can be obtained in the engineering data stream (AEDP)
C (NOT YET)
c
      ALNGP = ALNG 
      ALATP = ALAT
      LSHP = SHELL 
      BNP = PBN
      BEP = PBE
      BDP = PBD

c now we need to project into the spacecraft and FOS coordinate frames
c      CALL POAROTFOS (ID,PLNG,PLAT,PRHST,PDHST,RAV1,DECV1,PAV3,
c     1     PBN,PBE,PBD,PBV1,PBV2,PBV3,PBDX,PBDY,PBDZ,PVDX,PVDY,PVDZ)
      CALL POAROTFOS (ID,PLNG,PLAT,PRHST,PDHST,RAV1,DECV1,PAV3,
     1     PBN,PBE,PBD,PBV1,PBV2,PBV3,PBDX,PBDY,PBDZ,
     2     PVDX,PVDY,PVDZ)

c  The magnetometer prediction v123
      BV1P = PBV1  
      BV2P = PBV2
      BV3P = PBV3
c  The B-field in detector coordinates 
      BDXP = PBDX
      BDYP = PBDY
      BDZP = PBDZ

C the predicted drift digx,y as pixels
      CALL DIGICON (PBDX,PBDY,PBDZ,PVDX,PVDY,PVDZ,
     1                    digx,digy,digz,etime)
      YOFFXP=digx
      YOFFYP=digy
        
 9999   CONTINUE
      ISTAT=0
      GOTO 1000

 999    ISTAT=1
 1000   RETURN
        END
c----end of POAGIMP

