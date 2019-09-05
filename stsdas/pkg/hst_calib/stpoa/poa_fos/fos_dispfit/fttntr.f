C @(#)fttntr.for	15.1.1.1 (ES0-DMD) 01/12/01 16:57:19
C===========================================================================
C Copyright (C) 1995 European Southern Observatory (ESO)
C
C This program is free software; you can redistribute it and/or 
C modify it under the terms of the GNU General Public License as 
C published by the Free Software Foundation; either version 2 of 
C the License, or (at your option) any later version.
C
C This program is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C
C You should have received a copy of the GNU General Public 
C License along with this program; if not, write to the Free 
C Software Foundation, Inc., 675 Massachusetss Ave, Cambridge, 
C MA 02139, USA.
C
C Corresponding concerning ESO-MIDAS should be addressed as follows:
C	Internet e-mail: midas@eso.org
C	Postal address: European Southern Observatory
C			Data Management Division 
C			Karl-Schwarzschild-Strasse 2
C			D 85748 Garching bei Muenchen 
C			GERMANY
C===========================================================================
C
      SUBROUTINE FTTNTR(V1,V2,NACT,ACTCH,ISTAT)  
*
*  Module number:
*
*  Module name: FTTNTR
*
*  Keyphrase:
*  ----------
*       perform the fitting
*
*  Description:
*  ------------
*       This routine uses the MIDAS converted NR method of fitting to
*       create coeffs for the POA FOS dispersion.
*
*  FORTRAN name: fttntr.for
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
*       1       Nov 01  A. Alexov       Copy of yclopn.f w/some changes
*------------------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                   
C.COPYRIGHT: Copyright (c) 1987 European Southern Observatory,                  
C                                         all rights reserved                   
C                                                                               
C.VERSION: 1.0  ESO-FORTRAN Conversion, AA  17:25 - 13 JAN 1988                 
C                                                                               
C.LANGUAGE: F77+ESOext                                                          
C                                                                               
C.AUTHOR: J.D.PONZ                                                              
C                                                                               
C.IDENTIFICATION                                                                
C                                                                               
C  FITLIB.FOR   VERSION 1.0  27 MAR 1984                                        
C                                                                               
C.PURPOSE                                                                       
C                                                                               
C  INTERFACE ROUTINES FOR THE FITTING STRUCTURES                                
C ITERATE ON THE FITTING PROCESS                                                
C THE ITERATION IS FINISHED IF                                                  
C - NITER IS REACHED OR                                                         
C - CHISQ IS REACHED                                                            
C - CONVERGENCE IS ACHIEVED                                                     
C                                                                               
C                                                                               
C.ALGORITHM                                                                     
C                                                                               
C  USE MIDAS I/O INTERFACES TO FRAMES AND TABLES                                
C                                                                               
C.KEYWORDS                                                                      
C                                                                               
C  NON LINEAR FITTING                                                           
C                                                                               
C                                                                               
C----------------------------------------------------------------               
C                                                                               
C INPUT PARAMETERS                                                              
C NITER INTG NO. OF ITERATIONS TO BE DONE                                       
C   <0 USE PREVIOUS COMPUTED VALUES                                             
C   >0 USE GUESS                                                                
C RELAX REAL RELAXATION FACTOR                                                  
C CHISQ REAL CHI SQUARE                                                         
C V1,V2 REAL OPTIONAL FITTING INTERVAL (IF V1.NE.V2)                            
C                                                                               
C OUTPUT PARAMETERS                                                             
C NACT INTG ACTUAL NUMBER OF ITERATIONS                                         
C ACTCH REAL ACTUAL CHI SQ.                                                     
C ISTAT INTG STATUS RETURN                                                      
C                                                                               
      INTEGER NITER,NACT,ISTAT,IPRINT,I,J,NITER1,ICOUNT,NN,NP,NNP
ccc      INTEGER ISTAR,NCOL,NS,KZ6941,N,IDAT,NOFFSET,IFUN,K,IP,NILAST
      INTEGER KZ6941,N,IDAT,NOFFSET,IFUN,K,IP,NILAST
      REAL PREC,V1,V2,ACTCH,VS,VE,CHIOLD,CHIPERC,AC1,AC2,AC3
      REAL DELTA,C1,C2
ccc      CHARACTER*80 LINE                                                         
      CHARACTER*6 TYPE                                                          
      DOUBLE PRECISION CHI,PP,Q,Y,Y1,Y2,YOUT,CORRNRM                            
      DOUBLE PRECISION AL,B(50),A(50,50)                                        
ccc      INTEGER ICOL(10),ACOL,AROW,NC,NROW
ccc      REAL XVAL(10),X(8),W,FSUMSQ                                           
      REAL X(8),W,FSUMSQ                                           
ccc      LOGICAL NULL(10),VALID,NEXT,AUTO,ISEL,VCMATPR                             
      LOGICAL NULL(10),VALID,NEXT,AUTO,VCMATPR                             
      INCLUDE 'fiti.inc'                        
      DOUBLE PRECISION FZDERIV(FZPARMAX)
      INCLUDE 'fitc.inc'                                     
ccc      EQUIVALENCE (XVAL(1),W)                                                   
C      EQUIVALENCE (XVAL(2),Y)                                                   
ccc      EQUIVALENCE (XVAL(3),X(1))                                                

C Common block containing confiquration parameters
C

        CHARACTER*64 INPUT,FITTAB,OUTPUT
        INTEGER      ITERATION, PREV_ITERATION, PRINT
        REAL*4       RELAX, CHISQ
        LOGICAL      CONT_FIT,USE_PARAMS
        CHARACTER*4  DEP_VAR
        CHARACTER*18 DEP_COL,IDEP_COL,SEL_COL,NEW_COL
        REAL*4  X_ARRAY(4096)
        REAL*8  WAVE_ARRAY(4096)
        REAL*4  CALCX_ARRAY(4096)
        REAL*8  CALCW_ARRAY(4096)
        LOGICAL MOD_VAL(10)
        INTEGER NUM_IN_PNTS

        COMMON /CONFG1/RELAX,INPUT,FITTAB,OUTPUT,ITERATION,PRINT,
     *          PREV_ITERATION, CHISQ, CONT_FIT,
     *          USE_PARAMS,DEP_VAR,DEP_COL,IDEP_COL,SEL_COL,NEW_COL

        REAL*8  INIT_GUESS(10), ACTUAL_VAL(10), ERROR_VAL(10)
        COMMON /FIT_DATA/INIT_GUESS, ACTUAL_VAL, ERROR_VAL, MOD_VAL
        COMMON /XWAVE_DATA/X_ARRAY, WAVE_ARRAY, NUM_IN_PNTS
        COMMON /CALC_DATA/CALCX_ARRAY, CALCW_ARRAY

C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
      CHARACTER*80 CONTXT
      CHARACTER  PAR(10)

C    local vars
ccc      DOUBLE PRECISION PARAM(10)

      DATA TYPE/'R*4'/                                                          
      DATA PAR/'a','b','c','d','e','o','p','q','r','s'/                     
        
C FZMETPAR(10)       ! defined chi sq. value
C from fitimag:  CALL STKRDR('INPUTR',1,9,IAV,FZMETPAR(2),KUN,KNUL,ISTAT)
c        FZMETPAR(2) = 0.0
C from fitimag:  CALL GENCNV(PPRINT,2,1,IVAL,FZMETPAR(1),DVAL,MM)
c        FZMETPAR(1) = 0.0
        PREC =0.
        CALL VARINIT(ISTAT)
        IF(ISTAT.NE.0) GO TO 999
             
      DO 200 I=1,10
         NULL(I) = .FALSE.
200   CONTINUE

      CONTXT=' '                                                
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      CONTXT='Non-linear Least Square Fitting : '//                        
     +            'Newton-Raphson Method (defaulted)'                  
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      CONTXT='_______'//                                                   
     +     '___________________________________________________________'        
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      CONTXT=' '                                                  
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      CONTXT=' '                                                  
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      CONTXT=                                                              
     +         ' Print   Max. Fct. eval.  Prec. on Param.  Relax. Fact.'      
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)

C FZMETPAR(1) is how often to print; negative value also prints the corr matrix
C FZMETPAR(2) is ??
C FZMETPAR(3) is ??
C FZMETPAR(4) is the relaxation factor

c      FZMETPAR(1)=PRINT                              
c      FZMETPAR(2)=ITERATION                          
c      FZMETPAR(3)=0.
c      FZMETPAR(4)=RELAX

      WRITE (CONTXT,9000) PRINT,ITERATION,                
     +  0.,RELAX                                            
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)

      CONTXT=' '                                                  
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                    
      IF (RELAX.LT.0) THEN
         VCMATPR = .TRUE. 
      ELSE
         VCMATPR = .FALSE.
      ENDIF 
                       
      IPRINT = ABS(PRINT)
      NITER=ITERATION                                      
      IF (IPRINT.EQ.0) IPRINT = ABS(NITER)                           
ccc      AUTO   = RELAX .LT. 0.                
C POA - never want the code to run in AUTO mode
      AUTO=.FALSE.                                               
      RELAX  = ABS(RELAX)                                                       
      DO 10 I = 1,8                                                             
          NULL(I) = .FALSE.                                                     
   10 CONTINUE                                                                  
      NEXT   = .TRUE.        
C POA - total number of params is 10
      FZNPTOT=10                                                   
      IF (NITER.GT.0) THEN                                                      
          DO 20 J = 1,FZNPTOT   
              FZGUESS(J) = INIT_GUESS(J)                                     
              FZVALUE(J) = FZGUESS(J)    
ccc              PARAM(J)   = FZVALUE(J)                                 
   20     CONTINUE                                                              
          FZNITER = 0                                                         
          NITER1 = NITER                                                        

      ELSE                                                                      
          NITER1 = -NITER 
          DO 21 J = 1,FZNPTOT   
              FZGUESS(J) = INIT_GUESS(J)   
              FZVALUE(J) = FZGUESS(J)                                       
ccc              PARAM(J)   = FZVALUE(J)                                 
   21     CONTINUE                                                              
          FZNITER = 0                                                           
      END IF                                                                    
                                                                                
      VS     = MIN(V1,V2)                                                       
      VE     = MAX(V1,V2)                                                       
      FZCCHIS = 9999.                                                         
      FZCHISQ = 1.D-6                                                         
      FZRELAX = RELAX                                                         
      CORRNRM = 1.D6                                                            
      CHI    = 0.D0                                                             
      ICOUNT = 0   
C POA - no weight related parameters available
ccc      FZWEIGHT=0   
      FZNIND=1         ! no. of independent variables                                               
ccc      IF (FZWEIGHT.EQ.0) THEN                                                 
ccc          ISTAR  = 2                                                           
      W      = 1.                                                           
ccc          NCOL   = FZNIND + 1                                                 
ccc          ICOL(1) = 0                                                           
ccc                                                                                
ccc      ELSE                                                                      
ccc          ISTAR  = 1                                                            
ccc          NCOL   = FZNIND + 2                                                 
ccc          ICOL(1) = FZWEIGHT                                                  
ccc      END IF                                                                    
      
C POA - (in)dependent variable col numbers (irrelevant for us)  
ccc      FZDVAR=1  
ccc      FZIVAR(1)=2                                                               
ccc      FZIVAR(2)=2                                                               
ccc      ICOL(2) = FZDVAR                                                        
ccc      NULL(1) = .FALSE.                                                         
ccc      DO 30 I = 1,FZNIND                                                      
ccc          ICOL(I+2) = FZIVAR(I)                                               
ccc   30 CONTINUE    
C POA - removing the table get related call, since we have the data                           
ccc      CALL TBIGET(FZIDEN,NC,NROW,NS,ACOL,AROW,ISTAT) 

C POA - note that KZ6941 is the variable for the major interation loop               
      KZ6941 = 0
99    KZ6941 = KZ6941 + 1
          IF ( .NOT. (NEXT)) GO TO 180                                          
          DO 50 I = 1,50                                                        
              DO 40 J = 1,50                                                    
                  A(J,I) = 0.D0                                                 
   40         CONTINUE                                                          
              B(I)   = 0.D0                                                     
   50     CONTINUE                                                              
          CHIOLD = CHI                                                          
          CHI    = 0.D0                                                         
C                                                                               
C ITERATION FOR TABLES                                                          
C                                                                               
          N      = 0                                                            
          DO 100 IDAT = 1,NUM_IN_PNTS
C POA - deleting table i/o midas enties, our data is already loaded
ccc              CALL TBSGET(FZIDEN,IDAT,ISEL,ISTAT)    ! Reads row selection flag (ISEL)          
cccC              XVAL(2) = Y     ! to avoid problems with the equivalence
ccc              CALL TBRRDR(FZIDEN,IDAT,NCOL,ICOL(ISTAR),  ! Reads table row as a real variable
ccc     +                    XVAL(ISTAR),NULL(ISTAR),ISTAT) ! XVAL(ISTAR) is output arrays
ccc              Y=XVAL(2)


ccc For fit_wave2x function, wave is input, and pos is output; XVAL(2)=wave=X(1), XVAL(3)=pos=Y
ccc For fit_x2wave function, pos is input, and wave is output; XVAL(3)=pos=X(1), XVAL(2)=wave=Y

ccc              XVAL(2) = WAVE_ARRAY(IDAT)  
ccc              XVAL(3) = X_ARRAY(IDAT)  
              IF (DEP_VAR.EQ.'X') THEN    ! fit_wave2x function
                 Y=DBLE(X_ARRAY(IDAT))
                 X(1)=REAL(WAVE_ARRAY(IDAT))
              ELSE
                 X(1)=X_ARRAY(IDAT)          ! fit_x2wave function
                 Y=WAVE_ARRAY(IDAT)
              ENDIF
ccc              VALID  = ISEL .AND. ( .NOT. NULL(1)) .AND.                        
ccc     +                 ( .NOT. NULL(2)) .AND. ( .NOT. NULL(3))                  
              VALID  = .TRUE.  ! data has already been validated
C                                                                               
C ...       COMPUTE FUNCTION VALUES AND DERIVATIVES (0 IF FIXED PARAM)          
C                                                                               
ccc              IF (V1.NE.V2) VALID  = VALID .AND. XVAL(1) .GE. VS .AND.          
ccc     +                               XVAL(1) .LE. VE                            
              IF (VALID) THEN                                                   
                  N      = N + 1                                                
                  Y1     = 0.                                                   
                  NOFFSET = 1  
C POA - assuming that there will be only one function - FZNFUN   
                  FZNFUN = 1     ! no. of defined functions                                  
                  DO 60 IFUN = 1,FZNFUN                                       
ccc                      CALL FTFUNC(FZFCODE(IFUN),FZNIND,X,                   
ccc     +                            FZACTPAR(IFUN),FZVALUE(NOFFSET),          
ccc     +                            YOUT,FZDERIV(NOFFSET))   
C
C FTFUNC information - SUBROUTINE FTFUNC(NF,IND,X,NP,PARAM,Y1,DERIV)
C INPUT PARAMS:                                                              
C [NF] INTG FUNCTION CODE                                                         
C [IND] INTG NO. OF IND. VARS    (should be = 1)                                                 
C [X] REAL IND.VARS                   - array, with size=1               
C [NP] INTG NO. OF PARAMS                     
C [PARAM] DBLE FUNCTION PARAMETERS    - array     
C
C OUTPUT PARAMS:                                                                 
C [Y1] DBLE COMPUTED FUNCTION VALUE                                               
C [DERIV] DBLE COMPUTED DERIVATIVE VALUE    - array                                      
C
C       INTEGER NP,IND,NF
C       DOUBLE PRECISION Y1,PARAM(NP),DERIV(NP) 
C       REAL X(IND)

c    IND=num ind vars=1, real x(ind), NP= num of params = 10, func params
c    yout = computed output vals, computed derivative vals
                      NP=10
                      IF (DEP_VAR.EQ.'X') THEN
                         CALL FIT_WAVE2X(FZNIND,X,NP,FZVALUE(NOFFSET),
     *                               YOUT,FZDERIV(NOFFSET))
                         CALCX_ARRAY(IDAT)=REAL(YOUT)
                      ELSE
                         CALL FIT_X2WAVE(FZNIND,X,NP,FZVALUE(NOFFSET),
     *                               YOUT,FZDERIV(NOFFSET))
                         CALCW_ARRAY(IDAT)=YOUT
                      ENDIF
cc  call one or the other depending on which is the dependant variable
cc                      CALL FIT_X2WAVE(FZNIND,X,FZACTPAR(IFUN),FZVALUE(NOFFSET),YOUT,
cc                                  FZDERIV(NOFFSET))
                      Y1     = Y1 + YOUT                                        
                      NOFFSET = NOFFSET + FZACTPAR(IFUN) ! FZACTPAR(FZFUNMAX) actual n. of pars
   60             CONTINUE                                                      
                  DO 70 J = 1,FZNPTOT   
                      IF (MOD_VAL(J)) THEN
                         FZFIXED(J) = -1
                      ELSE
                         FZFIXED(J) = 0
                      ENDIF                      
                      IP     = FZFIXED(J)                                     
                      IF (IP.EQ.0) FZDERIV(J) = 0.D0                          
                      IF (IP.GT.0) THEN    ! FZPFAC(FZPARMAX) linear terms in the const.        
                          FZDERIV(IP) = FZDERIV(IP) +   
     +                                    FZDERIV(J)/FZPFAC(J)         
                          FZDERIV(J) = 0.D0                                   
                      END IF                                                    
                                                                                
   70             CONTINUE                                                      
C                                                                               
C ...         COMPUTE RESIDUALS, WEIGHTED RESIDUALS AND CHI**2                  
C                                                                               
                  W      = 1./ (W*W)                                            
                  Y2     = Y - Y1                                               
                  PP     = DBLE(W)                                              
                  IF (FZFLAG.EQ.1) PP     = 1.D0/AMAX1(1.0,SNGL(Y1))          
                  Q      = Y2                                                   
                  CHI    = CHI + Q*Q*PP                                         
C                                                                               
C ...         BUILD NORMAL EQUATION MATRIX                                      
C                                                                               
                  DO 90 J = 1,FZNPTOT                                         
                      AL     = FZDERIV(J)                                     
                      DO 80 K = 1,FZNPTOT                                     
                          A(J,K) = A(J,K) + AL*PP*FZDERIV(K)                  
   80                 CONTINUE                                                  
                      B(J)   = B(J) + PP*Q*AL                                   
   90             CONTINUE                                                      
              END IF                                                            
                                                                                
  100     CONTINUE                                                              
C                                                                               
C ...     ADD (PROBABILISTIC) PARAMETER CONSTRAINTS                             
C                                                                               
          NN     = N                                                            
          NP     = 0                                                            
          DO 110 I = 1,FZNPTOT                                                
              IF (FZFIXED(I).GE.0) THEN                                       
                  A(I,I) = 1.0D0                                                
                                                                                
              ELSE                                                              
                  NP     = NP + 1                                               
                  IF (FZUNCER(I).GT.0.) THEN                                  
                      NN     = NN + 1                                           
                      Q      = 1.0D0/DBLE(FZUNCER(I)*FZUNCER(I))            
                      A(I,I) = A(I,I) + Q                                       
                      B(I)   = B(I) + Q* (FZGUESS(I)-FZVALUE(I))            
C     ^                                                                         
C   To be modified for soft constraints                                         
                  END IF                                                        
                                                                                
              END IF                                                            
                                                                                
  110     CONTINUE                                                              
C                                                                               
C ...     INVERT NORMAL EQUATION MATRIX TO GET THE COVARIANCE MATRIX            
C                                                                               
          CALL DMATIN(A,FZNPTOT,50,FZFIXED,ISTAT)                           
          NNP    = FZNPTOT                                                    
          IF (ISTAT.NE.0) THEN                                                  
              CONTXT='*** ERR-NR  : Problems inverting matrix ***'
              CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              RETURN                                                            
                                                                                
          END IF                                                                
C                                                                               
C ... COMPUTE REDUCED CHI**2 AND CORRECTIONS FOR THE UNKNOWNS                   
C      
          FSUMSQ = CHI                                                         
          CHI    = DMAX1(CHI/DBLE(NN-NP),0.D0)                                
          CORRNRM = 0.D0                                                        
          DO 130 I = 1,FZNPTOT                                                
              Q      = 0.0D0                                                    
              DO 120 J = 1,FZNPTOT                                            
                  Q      = Q + A(I,J)*B(J)                                      
  120         CONTINUE                                                          
C                                                                               
C ... COMPUTE ERRORS OF THE CORRECTIONS (I.E. THE UNKNOWNS)                     
C ... AND APPLY CORRECTIONS (DO THE ADJUSTMENT!)                                
C                                                                               
              IF (FZFIXED(I).EQ.0) THEN                                       
                  FZERROR(I) = 0.D0                                           
                                                                                
              ELSE                                                              
                  Q      = RELAX*Q                                              
                  CORRNRM = CORRNRM + Q*Q                                       
                  FZERROR(I) = DSQRT(A(I,I)*CHI)                              
                  FZVALUE(I) = FZVALUE(I) + Q                               
              END IF                                                            
                                                                                
  130     CONTINUE                                                              
          CORRNRM = DSQRT(CORRNRM)                                              
C                                                                               
C ... LINEAR CONSTR. BETWEEN TWO PARAMS                                         
C                                                                               
          DO 140 I = 1,FZNPTOT                                                
              IP     = FZFIXED(I)                                             
              IF (IP.GT.0) THEN                                                 
                  FZERROR(I) = FZPFAC(I)*FZERROR(IP)                      
                  FZVALUE(I) = FZPFAC(I)*FZVALUE(IP)                      
              END IF                                                            
                                                                                
  140     CONTINUE                                                              
          ICOUNT = ICOUNT + 1                                                   
          FZNITER = FZNITER + 1                                             
          FZCCHIS = SNGL(CHI)                                                 
          NEXT   = ICOUNT .LT. NITER1 .AND. CORRNRM .GT. PREC                   
          IF ( .NOT. NEXT .OR. MOD(ICOUNT,IPRINT).EQ.0) THEN                    
              CONTXT=' '
              CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)                                          
              CONTXT=                                                      
     +        ' Iter  Fct. Eval.  Sum of Squares  Red. Chisq.   % Decr.'
              CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)                                          

              IF (ICOUNT.GT.1) THEN                                             
                  CHIPERC = 100.* (CHIOLD-CHI)/CHIOLD                           
                  WRITE (CONTXT,9010) FZNITER,FZNITER+PREV_ITERATION,                 
     +              FSUMSQ,FZCCHIS,CHIPERC                                           
                                                                                
              ELSE                                                              
                  WRITE (CONTXT,9010) FZNITER,FZNITER+PREV_ITERATION,                 
     +              FSUMSQ,FZCCHIS    

              END IF                                                            
                    
              CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)                            
              CONTXT=' '                                          
              CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)                            
              CONTXT='        Parameters   '
              CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)                            
                   
              DO 150 I = 1,FZNPTOT                                            
                  WRITE (CONTXT,9020) FZVALUE(I)                                
                  CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)                                     
  150         CONTINUE                                                          
              CONTXT=' '                                          
              CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)                                         
          END IF                                                                
C                                                                               
C ... STORE THE CHISQ                                                           
C                                                                               
          IF (FZNITER.LE.100) THEN                                            
              FZCHI(FZNITER) = CHI                                          
              NILAST = FZNITER                                                
                                                                                
          ELSE                                                                  
              DO 160 I = 1,99                                                   
                  FZCHI(I) = FZCHI(I+1)                                     
  160         CONTINUE                                                          
              FZCHI(100) = CHI                                                
              NILAST = 100                                                      
          END IF                                                                
C                                                                               
C ...                                                                           
C                                                                               
          IF (AUTO) THEN                                                        
              IF (NILAST.GT.10) THEN                                            
C                                                                               
C ... CHECK FOR CONVERGENCE                                                     
C                                                                               
                  IF (FZCHI(NILAST-1).EQ.FZCHI(NILAST)) GO TO 190           
                  AC1    = ALOG(AMAX1(FZCHI(NILAST-2),0.0001))                
                  AC2    = ALOG(AMAX1(FZCHI(NILAST-1),0.0001))                
                  AC3    = ALOG(AMAX1(FZCHI(NILAST),0.0001))                  
                  C1     = AC1 - AC2                                            
                  C2     = AC2 - AC3                                            
                  IF (ABS(C2).LE.1.E-5) GO TO 190                               
                  DELTA  = ABS((C1-C2)/C2)                                      
                  IF (C2.GT.0.0 .AND. DELTA.LE.0.25) RELAX  = (1.0-             
     +                DELTA)**4                                                 
              END IF                                                            
                                                                                
          END IF                                                                
                                                                                
      GOTO 99
  180 CONTINUE                                                                  
  190 CONTXT=' '                                                 
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)                                
      NACT   = FZNITER                                                        
      ACTCH  = FZCCHIS                                                        
      FZNDAT = N                                                              
ccc      IF (CORRNRM.LE.PREC) THEN                                                 
ccc          CONTXT='  --> NR  : Convergence achieved <--'             
ccc          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)  
ccc      ELSE                                                                      
ccc          CONTXT='*** WARN-NR  : No convergence reached ***'       
ccc          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)  
ccc      END IF 
                                                                   
ccc      CONTXT=' '                                                 
ccc      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)                                      
C                                                                               
C ... PRINT CORREL. MATRIX                                                      
C                                                                               
      IF (VCMATPR) CALL DCPRIN(A,NNP,50)  
           
C Print results
C Place results in common variables
      CONTXT=' '                 
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)   
      WRITE (CONTXT,9050) ' No. of data points', NUM_IN_PNTS
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)   
      CONTXT=' Dependent variable        '// DEP_COL            
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT) 
      CONTXT=' Independent variable        '// IDEP_COL            
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT) 

      CONTXT=' '                 
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)       
      CONTXT=' Param    Init Guess   OPEN/FIXED   Actual Value    Error'    
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)  
      DO 210 I = 1,10                                                   
         WRITE (CONTXT,9030) PAR(I),FZGUESS(I),FZFIXED(I),
     +                  FZVALUE(I),FZERROR(I)
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT) 
         ACTUAL_VAL(I)=FZVALUE(I)
         ERROR_VAL(I)=FZERROR(I)
 210  CONTINUE
      CONTXT=' '                 
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)  
      CONTXT='     Red. Chisq      Act. Nr. F. Eval.'                 
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)   
         WRITE (CONTXT,9040) FZCCHIS, NACT+PREV_ITERATION
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT) 

      CONTXT=' '                 
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)  

      IF (ISTAT.EQ.0) GOTO 1000

 999  ISTAT=1
 1000 ISTAT=0
      RETURN                                                                    
                                                                                
 9000 FORMAT (I6,7X,I7,7X,1PE9.1,8X,0PF4.2)                                     
 9010 FORMAT (I5,6X,I5,3X,1PE12.4,2X,1PE12.4,4X,0PF5.2)                         
 9020 FORMAT (2X,1PD15.7)                                                       
 9030 FORMAT (1X,1A,2X,1PD15.7,I,2X,1PD15.7,1PD15.7)
 9040 FORMAT (3X,1PE12.4,8X,I7)
 9050 FORMAT (A,I)                                                       

      END                                                                       
