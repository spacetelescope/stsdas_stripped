C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE VARINIT(ISTAT)
*
*  Module number:
*
*  Module name: varinit
*
*  Keyphrase:
*  ----------
*       initialize the common variables for fitting routines
*
*  Description:
*  ------------
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*
*  Subroutines Called:
*  -------------------
*
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*     /////////////////////////////////////////////////////////////////////
*            Start of Post Operational Archive versions (poa_calfos)
*     /////////////////////////////////////////////////////////////////////
*     1.0       Nov 01  A. Alexov         mostly a copy of fttabl.for (midas)
*-------------------------------------------------------------------------------
*
* INPUTS:
*
* OUTPUTS:
*       istat - error status
*
        IMPLICIT NONE

        INTEGER ISTAT
C------------------------------------------------------------------------------
C
	INTEGER I

        INCLUDE 'fiti.inc'
        INCLUDE 'fitc.inc'
C
C-----------------------------------------------------------------------------

C no weighting to be done in POA version
      FZWEIGHT = 0                                                        
C                                                                               
C INITIALIZE VARIABLES IN THE COMMON AREA                                       
C                                                                               
      FZNAME = 'bla'                                                          
      FZTYPE = 'TBL '                                                         
      FZNFUN = 0                                                              
      FZNDAT = 0                                                              
      FZNPTOT = 0                                                             
      FZNITER = 0                                                             
      FZRELAX = 0.                                                            
      FZCHISQ = 0.                                                            
      FZCCHIS = 0.                                                            
      DO 30 I = 1,FZFUNMAX       
          FZFCODE(I) = 0                                                      
          FZACTPAR(I) = 0                                                     
          FZSPEC(I) = ' '                                                     
   30 CONTINUE                                                                  
      DO 40 I = 1,FZPARMAX                                                    
          FZERROR(I) = 0.D0                                                   
          FZVALUE(I) = 0.D0                                                   
          FZGUESS(I) = 0.D0                                                   
          FZUNCER(I) = 0.                                                     
          FZFIXED(I) = -1                                                     
   40 CONTINUE                                                                  
      FZMAPPED = 1  


        GOTO 1000

 999    ISTAT=1
1000    RETURN
        END
