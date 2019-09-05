C @(#)fiti.inc	15.1.1.3 (ESO-DMD) 02/01/01 13:41:24
*
*  Module number:
*
*  Module name: fiti.inc
*
*  Keyphrase:
*  ----------
*       
*
*  Description:
*  ------------
*
*  FORTRAN name: fiti.inc
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
*       1       Nov 01  A. Alexov       Copy of fiti.inc from midas
*------------------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C.IDENTIFICATION
C       FIT.INC                     version 1.1 840328
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
C------------------------------------------------------------------------------
C
C       parameters
C
       INTEGER       FZFUNMAX       ! max.no. of allowed functions
       INTEGER       FZINDMAX       ! max.no. of independent variables
       INTEGER       FZPARMAX       ! max.no. of used parameters
       INTEGER       FZDEFMAX       ! no. of system defined functions
       INTEGER       FZNCHAR       ! no. of characters saved
       INTEGER       FZNINTG       ! no. of integers saved
       INTEGER       FZNREAL       ! no. of reals saved
CC       INTEGER       FZNDBLE       ! no. of double precision saved
       INTEGER       FZCINTG       ! optional number of copied integers
       INTEGER       FZCREAL       ! optional number of copied reals
       PARAMETER       (FZFUNMAX=16)
       PARAMETER       (FZINDMAX=3)
       PARAMETER       (FZPARMAX=128)
       PARAMETER       (FZDEFMAX=50) ! no. of system functions implemented
       PARAMETER       (FZNCHAR=28)       ! 12+4+12
       PARAMETER       (FZNINTG=14)       ! long load
       PARAMETER       (FZNREAL=109)       ! 3+PARMAX*FUNMAX+2*INDMAX
       PARAMETER       (FZCREAL=103)       ! 
       PARAMETER       (FZCINTG=5)       ! short load
C
C       Character       (FITCHAR)
C
       CHARACTER*12      FZNAME       ! name of i/o file
       CHARACTER*4       FZTYPE       ! type of i/o file ('BDF ','TBL ')
       CHARACTER*12      FZMASK       ! optional i/o weight mask
       CHARACTER*12      FZRIMA       ! optional reference image
C
       CHARACTER*8       FZFNAM(FZDEFMAX)       ! system defined function names
       CHARACTER*80      FZSPEC(FZFUNMAX)       ! function spec
       CHARACTER*8       FZPTOKEN(FZPARMAX)       ! parameter names
C
C       Integers saved (FITINTG)
C
       INTEGER       FZNFUN       ! no. of defined functions
       INTEGER       FZNITER       ! iteration counter
       INTEGER       FZNDAT       ! no. of data entering in the fit
       INTEGER       FZNPTOT       ! total number of parameters
       INTEGER       FZFLAG       ! if 1, poissonian weights
C
C       Integers optionally loaded
C
       INTEGER       FZDVAR       ! dependent variable (0 -BDF, col#-TBL)
       INTEGER       FZWEIGHT       ! optional i/o weight column
       INTEGER       FZNIND       ! no. of independent variables
       INTEGER       FZIVAR(FZINDMAX)       ! independent variables
       INTEGER       FZNPIX(FZINDMAX)       ! number of pixels per axis
C
C       Integers used as work space
C
       INTEGER       FZFIXED(FZPARMAX)       ! fixed param. flag
       INTEGER       FZFCODE(FZFUNMAX)       ! function code
       INTEGER       FZACTPAR(FZFUNMAX)       ! actual n. of params
       INTEGER       FZPLEN(FZPARMAX)       ! length of the param names
C
       INTEGER       FZNAXIS       ! number of axis for BDF(=FZNIND)
       INTEGER       FZPPRN       ! offset to print flag
       INTEGER       FZIWGT       ! offset to weight
       INTEGER       FZIBND       ! offset to bounds
       INTEGER       FZIMET       ! offset to method
C
C       Reals saved (FITREAL)
C
       REAL              FZRELAX       ! relaxation factor
       REAL              FZCCHIS       ! Final chi sq. value
       REAL              FZCHISQ       ! defined chi sq. value
       REAL              FZCHI(100)       ! chisq conv
       REAL              FZSTART(FZINDMAX)       ! starting coords. for BDF
       REAL              FZSTEP(FZINDMAX)       ! sampling step for BDF
C       (FITERROR)
       DOUBLE PRECISION              FZERROR(FZPARMAX)       ! parameter error 
C       (FITPARAM)
       DOUBLE PRECISION              FZVALUE(FZPARMAX)       ! par. values
C
C       Reals used as work space
C
       DOUBLE PRECISION              FZGUESS(FZPARMAX)       ! initial guess
       DOUBLE PRECISION              FZDUMM
       REAL              FZUNCER(FZPARMAX)       ! uncertain.
       REAL              FZPFAC(FZPARMAX)       ! linear terms in the const.
C
C       auxiliary data
C
       INTEGER       FZPTRI       ! pointer to bdf
       INTEGER       FZPTRM       ! pointer to weights
       INTEGER       FZDUMI1,FZDUMI2
C 
       INTEGER       FZMAPPED       ! mapped flag
       INTEGER       FZFPAR(FZDEFMAX)       ! number of parameters per
                                            ! defined function
       INTEGER       FZSELE(20)       ! function selection flag
C
       CHARACTER*28  FZCHAR              ! char buffer
       INTEGER       FZINTG(FZNINTG)       ! intg buffer
       REAL          FZREAL(FZNREAL)       ! real buffer
C
C       Modifs for NAG and bound constraints
C
       REAL              FZMETPAR(10)       ! defined chi sq. value
       DOUBLE PRECISION  FZLOWB(FZPARMAX)       ! par. lower bnd
       DOUBLE PRECISION  FZUPPB(FZPARMAX)       ! par. upper bnd
       INTEGER           FZNRPFIX              ! Number of fix par.
C
C       Errors
C
       INTEGER       FERPAR       ! error in input parameters
       INTEGER       FERFUN       ! non defined function name
       INTEGER       FERTAB       ! inp/out on different tables
       INTEGER       FERDAT       ! data not available
       INTEGER       FERPFU       ! error in function params.
       INTEGER       FERDUP       ! duplicated param name
       INTEGER       FERCON       ! error in cons. definition
C
C ...  added in new vesion
C
       INTEGER       FZIDEN      ! file identification
