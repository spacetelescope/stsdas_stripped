C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE GEOPOS(LATI,LONGI,MLAT,MLONG,ISTAT)
*
*  Module number:
*
*  Module name: GEOPOS
*
*  Keyphrase:
*  ----------
*       Calculate geomagnetic position from geographic position
*
*  Description:
*  ------------
*       This routine calculates the geomagnetic position given the
*	geographic latitude and longitude.
*	This routine is adapted from an algorith supplied by the FOS
*	IDT whose ultimate source is somewhere in the NSSDCA
*
*  FORTRAN name: GEOPOS.for
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
*       1       Sep 91	S. Hulbert      Designed and coded
*       1.1     Sep 98  M.D. De La Pena Removed dead statement.
*-------------------------------------------------------------------------------
*
* INPUTS:
*	lati - geographic latitude
*	lngi - geographic longitude
*
* OUTPUT:
*	mlat - geomagnetic latitude
*	mlong - geomagnetic longitude
*       istat - error status
*
*----------------------------------------------------------------------------
        INTEGER ISTAT
        DOUBLE PRECISION LATI,LONGI,MLAT,MLONG
C------------------------------------------------------------------------------
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.1415926535898D0)
      DOUBLE PRECISION RADDEG
      PARAMETER (RADDEG = 57.295779513082D0)
C
C LOCAL VARIABLES ------------------------------------------------------
	DOUBLE PRECISION DEGRAD,TWOPI,CBG,CI,SI,YLG,SBG,CLG,SLG
	DOUBLE PRECISION SBM,CBM,SLM,CLM
C
C-----------------------------------------------------------------------
C
	DEGRAD=1./RADDEG
	TWOPI=2.D0*PI
	CBG=11.4D0*DEGRAD                              
	CI=COS(CBG)     
	SI=SIN(CBG)
C
C convert geographic latitude and longitude to geomagnetic 
C 
        YLG=LONGI+69.8D0   
        CBG=COS(LATI*DEGRAD)                           
        SBG=SIN(LATI*DEGRAD)                           
        CLG=COS(YLG*DEGRAD)                          
        SLG=SIN(YLG*DEGRAD)                          
        SBM=SBG*CI+CBG*CLG*SI                        
        MLAT=ASIN(SBM)   
        CBM=COS(MLAT)     
        SLM=(CBG*SLG)/CBM                            
        CLM=(-SBG*SI+CBG*CLG*CI)/CBM
   	IF(CLM.GT.1.D0)CLM=1.D0                 
        MLONG=ACOS(CLM)
        IF(SLM.LT.0.D0)MLONG=TWOPI-ACOS(CLM)             
        MLAT=MLAT*RADDEG    
        MLONG=MLONG*RADDEG
C
        ISTAT=0
1000    RETURN
        END
