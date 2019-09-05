C @(#)fitc.inc	15.1.1.3 (ESO-DMD) 02/01/01 13:41:24
*
*  Module number:
*
*  Module name: fitc.inc
*
*  Keyphrase:
*  ----------
*       
*
*  Description:
*  ------------
*
*  FORTRAN name: fitc.inc
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  
*
*  Subroutines Called:
*  -------------------
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Nov 01  A. Alexov       Copy of fitc.inc from midas
*------------------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C.IDENTIFICATION
C       FITC.INC                     version 1.1 840328
C        J.D.Ponz              ESO - Garching
C
C.PURPOSE
C
C       Control Data Structure for Fitting
C
C       User Interface Control Variables : Common /FZCOMMON0/,
C                                            /FZCOMMON1/
C.KEYWORDS
C
C       FIT, DATA STRUCTURE
C
C.VERSIONS
C       2.0 -- Modify common FITCM1 to include first double precision
C 
C 010131		last modif
C 
C------------------------------------------------------------------------------
C 
C
C       Common areas
C
       COMMON/FITCM0/       FZNAME,FZTYPE,FZMASK,
     .                      FZRIMA,FZFNAM,FZSPEC,FZPTOKEN
C 
       COMMON/FITCM1/       FZERROR,FZVALUE,FZGUESS,
     .       FZNFUN,FZNITER,FZNDAT,
     .       FZNPTOT,FZFLAG,FZDVAR,FZWEIGHT,FZNIND,FZIVAR,FZNPIX,
     .       FZFIXED,FZFCODE,FZACTPAR,FZPLEN,FZRELAX,FZCCHIS,
     .       FZCHISQ,FZCHI,FZSTART,FZSTEP,
     .       FZUNCER,FZPFAC,FZDUMI1,FZDUMI2,FZMAPPED,FZFPAR,
     .       FZSELE
C 
        COMMON/FITCM2/ FZMETPAR,FZLOWB,FZUPPB,FZNRPFIX,
     .       FZPPRN,FZIWGT,FZIBND,FZIMET
C 
        COMMON/FITCM3/ FERPAR,FERFUN,FERTAB,FERDAT,FERPFU,
     .                 FERDUP,FERCON,FZIDEN
C 
        COMMON/FITCM4/ FZPTRI,FZPTRM
C 
       EQUIVALENCE       (FZNAXIS,FZNIND)
       EQUIVALENCE       (FZCHAR,FZNAME)
       EQUIVALENCE       (FZINTG,FZNFUN)
       EQUIVALENCE       (FZREAL,FZRELAX)
       EQUIVALENCE       (FZDUMM,FZGUESS)

