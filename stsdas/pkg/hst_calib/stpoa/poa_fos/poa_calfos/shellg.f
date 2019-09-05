C
      SUBROUTINE SHELLG(GLAT,GLON,ALT,DIMO,FL,ICODE,B0)
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NEWBILCAL" program to produce IGRF geomagnetic model (GSFC)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c  ------------------------------------------------------------------------
C--------------------------------------------------------------------
C CALCULATES L-VALUE FOR SPECIFIED GEODAETIC COORDINATES, ALTITUDE
C AND GEMAGNETIC FIELD MODEL.
C REF: G. KLUGE, EUROPEAN SPACE OPERATIONS CENTER, INTERNAL NOTE 
C      NO. 67, 1970.
C      G. KLUGE, COMPUTER PHYSICS COMMUNICATIONS 3, 31-35, 1972
C--------------------------------------------------------------------
C CHANGES (D. BILITZA, NOV 87):
C   - USING CORRECT DIPOL MOMENT I.E.,DIFFERENT COMMON/MODEL/
C   - USING IGRF EARTH MAGNETIC FIELD MODELS FROM 1945 TO 1990
C--------------------------------------------------------------------
C  INPUT:  ENTRY POINT SHELLG
C               GLAT  GEODETIC LATITUDE IN DEGREES (NORTH)
C                GLON  GEODETIC LONGITUDE IN DEGREES (EAST)
C                ALT   ALTITUDE IN KM ABOVE SEA LEVEL
C
C          ENTRY POINT SHELLC
C              V(3)  CARTESIAN COORDINATES IN EARTH RADII (6371.2 KM)
C                     X-AXIS POINTING TO EQUATOR AT 0 LONGITUDE
C                     Y-AXIS POINTING TO EQUATOR AT 90 LONG.
C                     Z-AXIS POINTING TO NORTH POLE
C
C          DIMO             DIPOL MOMENT IN GAUSS (NORMALIZED TO EARTH RADIUS) 
C
C          COMMON 
C              X(3)       NOT USED
C              H(144)       FIELD MODEL COEFFICIENTS ADJUSTED FOR SHELLG
C-----------------------------------------------------------------------
C  OUTPUT: FL          L-VALUE
C          ICODE         =1 NORMAL COMPLETION
C                     =2 UNPHYSICAL CONJUGATE POINT (FL MEANINGLESS)
C                     =3 SHELL PARAMETER GREATER THAN LIMIT UP TO
C                        WHICH ACCURATE CALCULATION IS REQUIRED;
C                        APPROXIMATION IS USED.
C           B0          MAGNETIC FIELD STRENGTH IN GAUSS
C-----------------------------------------------------------------------
      REAL*4 V(3),U(3,3),P(8,100),SP(3)
      REAL*4 X(3),H(144) 
      REAL*4 UMR,ERAD,AQUAD,BQUAD


      INTEGER icode,i,n
      REAL*4 glat,glon,alt,steq,iequ
      REAL*4 fl,b0,rmin,rmax,step,bequ,rlat,ct,st,d,rlon,rq
      REAL*4 r3h,bq2,r2,bq3,r3,bq1,r1,zz,step12,step2,fi,oradik,oterm
      REAL*4 stp,z,c0,c1,c2 
      REAL*4 c3,d0,d1,d2,e0,e1,e2,t,hli,zq,r,ff,radik,term,dimob0,xx,gg 

      REAL*8 dimo 
  
      COMMON /SHCOMM/  X,H
      COMMON /FIDB0/ SP
      COMMON /GENER/ UMR,ERAD,AQUAD,BQUAD
C
C-- RMIN, RMAX ARE BOUNDARIES FOR IDENTIFICATION OF ICODE=2 AND 3
C-- STEP IS STEP SIZE FOR FIELD LINE TRACING
C-- STEQ IS STEP SIZE FOR INTEGRATION
      DATA RMIN,RMAX       /0.05,1.01/
      DATA STEP,STEQ       /0.20,0.03/
      DATA U/                 +0.3511737,-0.9148385,-0.1993679,  
     1                        +0.9335804,+0.3583680,+0.0000000,  
     2                        +0.0714471,-0.1861260,+0.9799247/  
      BEQU=1.E10

C*****ENTRY POINT  SHELLG  TO BE USED WITH GEODETIC CO-ORDINATES
      RLAT=GLAT*UMR
      CT=SIN(RLAT)                                              
      ST=COS(RLAT)                                              
      D=SQRT(AQUAD-(AQUAD-BQUAD)*CT*CT)
      X(1)=(ALT+AQUAD/D)*ST/ERAD
      X(3)=(ALT+BQUAD/D)*CT/ERAD
      RLON=GLON*UMR
      X(2)=X(1)*SIN(RLON)                                       
      X(1)=X(1)*COS(RLON)                                       
      GOTO9                                                     

C*****ENTRY POINT  SHELLC  TO BE USED WITH CARTESIAN CO-ORDINATES
      ENTRY SHELLC(V,FL,B0)                                     
      X(1)=V(1)                                                  
      X(2)=V(2)                                                  
      X(3)=V(3)                                                  
C*****CONVERT TO DIPOL-ORIENTED CO-ORDINATES                     
9     RQ=1./(X(1)*X(1)+X(2)*X(2)+X(3)*X(3))
      R3H=SQRT(RQ*SQRT(RQ))                                      
      P(1,2)=(X(1)*U(1,1)+X(2)*U(2,1)+X(3)*U(3,1))*R3H           
      P(2,2)=(X(1)*U(1,2)+X(2)*U(2,2)            )*R3H           
      P(3,2)=(X(1)*U(1,3)+X(2)*U(2,3)+X(3)*U(3,3))*RQ            
C*****FIRST THREE POINTS OF FIELD LINE                           
      STEP=-SIGN(STEP,P(3,2))                                    
      CALL STOER(P(1,2),BQ2,R2)                                  
      B0=SQRT(BQ2)                                               
      P(1,3)=P(1,2)+0.5*STEP*P(4,2)                              
      P(2,3)=P(2,2)+0.5*STEP*P(5,2)                              
      P(3,3)=P(3,2)+0.5*STEP                                     
      CALL STOER(P(1,3),BQ3,R3)                                  
      P(1,1)=P(1,2)-STEP*(2.*P(4,2)-P(4,3))                      
      P(2,1)=P(2,2)-STEP*(2.*P(5,2)-P(5,3))                      
      P(3,1)=P(3,2)-STEP                                         
      CALL STOER(P(1,1),BQ1,R1)                                  
      P(1,3)=P(1,2)+STEP*(20.*P(4,3)-3.*P(4,2)+P(4,1))/18.       
      P(2,3)=P(2,2)+STEP*(20.*P(5,3)-3.*P(5,2)+P(5,1))/18.       
      P(3,3)=P(3,2)+STEP                                         
      CALL STOER(P(1,3),BQ3,R3)                                  
C*****INVERT SENSE IF REQUIRED                                   
      IF(BQ3.LE.BQ1)GOTO2                                        
      STEP=-STEP                                                 
      R3=R1                                                      
      BQ3=BQ1                                                    
      DO 1 I=1,7                                                 
      ZZ=P(I,1)                                                  
      P(I,1)=P(I,3)                                              
1     P(I,3)=ZZ                                                  
C*****SEARCH FOR LOWEST MAGNETIC FIELD STRENGTH
2     IF(BQ1.LT.BEQU) THEN
       BEQU=BQ1
       IEQU=1
       ENDIF
      IF(BQ2.LT.BEQU) THEN
       BEQU=BQ2
       IEQU=2
       ENDIF
      IF(BQ3.LT.BEQU) THEN
       BEQU=BQ3
       IEQU=3
       ENDIF
C*****INITIALIZATION OF INTEGRATION LOOPS                        
      STEP12=STEP/12.
      STEP2=STEP+STEP                                            
      STEQ=SIGN(STEQ,STEP)                                       
      FI=0.                                                      
      ICODE=1                                                    
      ORADIK=0.                                                  
      OTERM=0.                                                   
      STP=R2*STEQ                                                
      Z=P(3,2)+STP                                               
      STP=STP/0.75
      P(8,1)=STEP2*(P(1,1)*P(4,1)+P(2,1)*P(5,1))                 
      P(8,2)=STEP2*(P(1,2)*P(4,2)+P(2,2)*P(5,2))                 
C*****MAIN LOOP (FIELD LINE TRACING)                             
      DO 3 N=3,3333                                              
C*****CORRECTOR (FIELD LINE TRACING)                             
      P(1,N)=P(1,N-1)+STEP12*(5.*P(4,N)+8.*P(4,N-1)-P(4,N-2))    
      P(2,N)=P(2,N-1)+STEP12*(5.*P(5,N)+8.*P(5,N-1)-P(5,N-2))    
C*****PREPARE EXPANSION COEFFICIENTS FOR INTERPOLATION           
C*****OF SLOWLY VARYING QUANTITIES                               
      P(8,N)=STEP2*(P(1,N)*P(4,N)+P(2,N)*P(5,N))                 
      C0=P(1,N-1)**2+P(2,N-1)**2                                 
      C1=P(8,N-1)                                                
      C2=(P(8,N)-P(8,N-2))*0.25                                  
      C3=(P(8,N)+P(8,N-2)-C1-C1)/6.0
      D0=P(6,N-1)                                                
      D1=(P(6,N)-P(6,N-2))*0.5                                   
      D2=(P(6,N)+P(6,N-2)-D0-D0)*0.5                             
      E0=P(7,N-1)
      E1=(P(7,N)-P(7,N-2))*0.5                                   
      E2=(P(7,N)+P(7,N-2)-E0-E0)*0.5                             
C*****INNER LOOP (FOR QUADRATURE)                                
4     T=(Z-P(3,N-1))/STEP                                        
      IF(T.GT.1.)GOTO5                                           
      HLI=0.5*(((C3*T+C2)*T+C1)*T+C0)                            
      ZQ=Z*Z
      R=HLI+SQRT(HLI*HLI+ZQ)
      IF(R.LE.RMIN)GOTO30                               
      RQ=R*R
      FF=SQRT(1.+3.*ZQ/RQ)                              
      RADIK=B0-((D2*T+D1)*T+D0)*R*RQ*FF                 
      IF(R-RMAX)44,44,45                                
45    ICODE=2                                           
      RADIK=RADIK-12.*(R-RMAX)**2                       
44    IF(RADIK+RADIK.LE.ORADIK) GOTO 10
      TERM=SQRT(RADIK)*FF*((E2*T+E1)*T+E0)/(RQ+ZQ)      
      FI=FI+STP*(OTERM+TERM)                            
      ORADIK=RADIK                                      
      OTERM=TERM                                        
      STP=R*STEQ                                        
      Z=Z+STP                                           
      GOTO4                                             
C*****PREDICTOR (FIELD LINE TRACING)                    
5     P(1,N+1)=P(1,N)+STEP12*(23.*P(4,N)-16.*P(4,N-1)+5.*P(4,N-2))  
      P(2,N+1)=P(2,N)+STEP12*(23.*P(5,N)-16.*P(5,N-1)+5.*P(5,N-2))  
      P(3,N+1)=P(3,N)+STEP                                          
      CALL STOER(P(1,N+1),BQ3,R3)                                   
C*****SEARCH FOR LOWEST MAGNETIC FIELD STRENGTH
      IF(BQ3.LT.BEQU) THEN
       IEQU=N+1
       BEQU=BQ3
       ENDIF
3     CONTINUE
10    IF(IEQU.lt.2) IEQU=2 
      SP(1)=P(1,IEQU-1)
      SP(2)=P(2,IEQU-1)
      SP(3)=P(3,IEQU-1)
      IF(ORADIK.LT.1E-15)GOTO11                                     
      FI=FI+STP/0.75*OTERM*ORADIK/(ORADIK-RADIK)              
C
C-- The minimal allowable value of FI was changed from 1E-15 to 1E-12,
C-- because 1E-38 is the minimal allowable arg. for ALOG in our envir.
C-- D. Bilitza, Nov 87.
C
11    FI=0.5*ABS(FI)/SQRT(B0)+1E-12                       
C*****COMPUTE L FROM B AND I.  SAME AS CARMEL IN INVAR.  
C-- Correct dipole moment is used here. D. Bilitza, Nov 87.
      DIMOB0=DIMO/B0
      XX=ALOG(FI*FI*FI/DIMOB0)
      IF(XX.GT.23.0) GOTO 776   
      IF(XX.GT.11.7) GOTO 775  
      IF(XX.GT.+3.0) GOTO 774    
      IF(XX.GT.-3.0) GOTO 773   
      IF(XX.GT.-22.) GOTO 772  
  771 GG=3.33338E-1*XX+3.0062102E-1                                 
      GOTO777                                                          
  772 GG=((((((((-8.1537735E-14*XX+8.3232531E-13)*XX+1.0066362E-9)*XX+  
     18.1048663E-8)*XX+3.2916354E-6)*XX+8.2711096E-5)*XX+1.3714667E-3)* 
     2XX+1.5017245E-2)*XX+4.3432642E-1)*XX+6.2337691E-1                 
      GOTO777                                                           
  773 GG=((((((((2.6047023E-10*XX+2.3028767E-9)*XX-2.1997983E-8)*XX-    
     15.3977642E-7)*XX-3.3408822E-6)*XX+3.8379917E-5)*XX+1.1784234E-3)* 
     2XX+1.4492441E-2)*XX+4.3352788E-1)*XX+6.228644E-1                  
      GOTO777                                                           
  774 GG=((((((((6.3271665E-10*XX-3.958306E-8)*XX+9.9766148E-07)*XX-    
     11.2531932E-5)*XX+7.9451313E-5)*XX-3.2077032E-4)*XX+2.1680398E-3)* 
     2XX+1.2817956E-2)*XX+4.3510529E-1)*XX+6.222355E-1                  
      GOTO777                                                           
  775 GG=(((((2.8212095E-8*XX-3.8049276E-6)*XX+2.170224E-4)*XX-6.7310339
     1E-3)*XX+1.2038224E-1)*XX-1.8461796E-1)*XX+2.0007187E0             
      GOTO777                                                           
  776 GG=XX-3.0460681E0                                                 
  777 FL=EXP(ALOG((1.+EXP(GG))*DIMOB0)/3.0)
      RETURN                                                            
C*****APPROXIMATION FOR HIGH VALUES OF L.                               
30    ICODE=3                                                           
      T=-P(3,N-1)/STEP                                                  
      FL=1./(ABS(((C3*T+C2)*T+C1)*T+C0)+1E-15)                          
      RETURN                                                            
      END                                                               
C
