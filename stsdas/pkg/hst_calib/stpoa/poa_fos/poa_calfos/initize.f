C
       SUBROUTINE INITIZE
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NEWBILCAL" program to produce IGRF geomagnetic model (GSFC)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c  ------------------------------------------------------------------------
C----------------------------------------------------------------
C Initializes the parameters in COMMON/GENER/
C       UMR     = ATAN(1.0)*4./180.   <DEGREE>*UMR=<RADIANT>
C       ERA       EARTH RADIUS FOR NORMALIZATION OF CARTESIAN 
C                     COORDINATES (6371.2 KM) 
C       EREQU       MAJOR HALF AXIS FOR EARTH ELLIPSOID (6378.160 KM)
C       ERPOL       MINOR HALF AXIS FOR EARTH ELLIPSOID (6356.775 KM)
C       AQUAD       SQUARE OF MAJOR HALF AXIS FOR EARTH ELLIPSOID
C       BQUAD   SQUARE OF MINOR HALF AXIS FOR EARTH ELLIPSOID
C
C ERA, EREQU and ERPOL as recommended by the INTERNATIONAL 
C ASTRONOMICAL UNION . (IAU1964)
C-----------------------------------------------------------------
       REAL*4 UMR,ERA,EREQU,ERPOL,AQUAD,BQUAD
       COMMON/GENER/       UMR,ERA,AQUAD,BQUAD

       ERA=6371.2
       EREQU=6378.16
       ERPOL=6356.775
       AQUAD=EREQU*EREQU
       BQUAD=ERPOL*ERPOL
       UMR=ATAN(1.0)*4./180.

       RETURN
       END
