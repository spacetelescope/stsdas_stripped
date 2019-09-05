C
      SUBROUTINE SNEWBILCAL (DGLNG,DGLAT,RADI,DYEAR,DALNG,
     $                       DALAT,DBABS,DBN,DBE,DBD,DXL,ISTAT)
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NEWBILCAL" program to produce IGRF geomagnetic model (GSFC)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c      1.2       Jun 01  A. Alexov       Added IDS's common block for file
c                                        header access, ISTAT
c  ------------------------------------------------------------------------
C
C *************************************************************
C ** IGRF MAGNETIC FIELD MODEL +  SHELLG L-VALUE CALCULATION **
C *************************************************************
C adapted by Michael Rosa STECF 22 DEC 1996 from BILCAL, VERSION 3.0, 
C AUGUST 1995, includes also AACGM code
C
C  UMR     =   ATAN(1.0)*4./180.   <DEGREE>*UMR=<RADIANT>
C  ERA         EARTH RADIUS FOR NORMALIZATION OF CARTESIAN COORDINATES 
C  EREQU       MAJOR HALF AXIS FOR EARTH ELLIPSOID (6378.160 KM)
C  ERPOL       MINOR HALF AXIS FOR EARTH ELLIPSOID (6356.775 KM)
C  AQUAD       SQUARE OF MAJOR HALF AXIS FOR EARTH ELLIPSOID
C  BQUAD       SQUARE OF MINOR HALF AXIS FOR EARTH ELLIPSOID
C  ERA, EREQU and ERPOL as recommended by the IAU
C---------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER*4   ICODE
      REAL*8 DGLNG,DGLAT,DALNG,DALAT,DBABS,DXL,RADI
      REAL*8 DBN,DBE,DBD,DIMO,DIP,DEC,DYEAR
c      REAL*8 DGLNG,DGLAT,RADI,DIMO,DIP,DEC

      REAL*4      GLNG,GLAT,ALAT,ALNG,BABS,BAB1,XL
      REAL*4      UMR,ERAD,AQUAD,BQUAD
      REAL*4      HEIGHT,YEAR,BN,BE,BD

      COMMON/GENER/       UMR,ERAD,AQUAD,BQUAD
C
C local variables
C
      INTEGER ISTAT
      CHARACTER*80 CONTXT
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
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

C     initialize common /GENER/
      if (ERAD.LT.6300.0) CALL INITIZE
C
      HEIGHT = SNGL(RADI/1000.D0)-ERAD
      GLNG = SNGL(DGLNG)
      GLAT = SNGL(DGLAT) 
      YEAR = SNGL(DYEAR)

C Get field coefficients for actual decimal year
      CALL FELDCOF (YEAR,DIMO,ISTAT)
      IF (ISTAT .NE. 0) THEN
             GO TO 999
      END IF
C Calculate field
      CALL FELDG (GLAT,GLNG,HEIGHT,BN,BE,BD,BABS)
C Calculate shell parameters
      CALL SHELLG (GLAT,GLNG,HEIGHT,DIMO,XL,ICODE,BAB1)
 
      DIP = ASIN(BD/BABS)/UMR
      DEC = ASIN(BE/SQRT(BE*BE+BN*BN))/UMR

      DBABS = DBLE(BABS)
      DBN = BN
      DBE = BE
      DBD = BD 
      DXL = DBLE(XL) 

      CALL CONV_GEO_CRD (GLAT,GLNG,HEIGHT,ALAT,ALNG,1,ISTAT)
C check the output error      
      IF (ISTAT .NE. 0) THEN
             GO TO 999
      END IF

      DALNG = DBLE(ALNG)
      DALAT = DBLE(ALAT) 

      IF (ISTAT .NE. 0) THEN
             GO TO 999
      END IF

      ISTAT=0
      GO TO 1000
999   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT=1
1000  RETURN
      END

