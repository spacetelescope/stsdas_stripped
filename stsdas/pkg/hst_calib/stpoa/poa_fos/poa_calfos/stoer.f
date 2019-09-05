C
      SUBROUTINE STOER(P,BQ,R)                                          
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NEWBILCAL" program to produce IGRF geomagnetic model (GSFC)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c  ------------------------------------------------------------------------
C*******************************************************************
C* SUBROUTINE USED FOR FIELD LINE TRACING IN SHELLG                *
C* CALLS ENTRY POINT FELDI IN GEOMAGNETIC FIELD SUBROUTINE FELDG   *
C*******************************************************************
      REAL*4  BQ,R
      REAL*4  P(7),U(3,3)
      REAL*4  XI(3),H(144)

      REAL*8  ZM,FLI,RQ,WR,XM,YM,Q
      REAL*8  DX,DY,DZ,DXM,DYM,DZM,DR,DSQ

      COMMON /SHCOM/  XI,H
      DATA U/                 +0.3511737,-0.9148385,-0.1993679,         
     A                        +0.9335804,+0.3583680,+0.0000000,         
     B                        +0.0714471,-0.1861260,+0.9799247/         
C*****XM,YM,ZM  ARE GEOMAGNETIC CARTESIAN INVERSE CO-ORDINATES          
      ZM=P(3)                                                           
      FLI=P(1)*P(1)+P(2)*P(2)+1E-15
      R=0.5*(FLI+SQRT(FLI*FLI+(ZM+ZM)**2))
      RQ=R*R
      WR=SQRT(R)                                                        
      XM=P(1)*WR                                                        
      YM=P(2)*WR                                                        
C*****TRANSFORM TO GEOGRAPHIC CO-ORDINATE SYSTEM                        
      XI(1)=XM*U(1,1)+YM*U(1,2)+ZM*U(1,3)                               
      XI(2)=XM*U(2,1)+YM*U(2,2)+ZM*U(2,3)                               
      XI(3)=XM*U(3,1)          +ZM*U(3,3)                               
C*****COMPUTE DERIVATIVES                                               
      CALL FELDI(XI,H)                                                  
      Q=H(1)/RQ                                                         
      DX=H(3)+H(3)+Q*XI(1)                                              
      DY=H(4)+H(4)+Q*XI(2)                                              
      DZ=H(2)+H(2)+Q*XI(3)                                              
C*****TRANSFORM BACK TO GEOMAGNETIC CO-ORDINATE SYSTEM                  
      DXM=U(1,1)*DX+U(2,1)*DY+U(3,1)*DZ                                 
      DYM=U(1,2)*DX+U(2,2)*DY                                           
      DZM=U(1,3)*DX+U(2,3)*DY+U(3,3)*DZ                                 
      DR=(XM*DXM+YM*DYM+ZM*DZM)/R                                       
C*****FORM SLOWLY VARYING EXPRESSIONS                                   
      P(4)=(WR*DXM-0.5*P(1)*DR)/(R*DZM)                                 
      P(5)=(WR*DYM-0.5*P(2)*DR)/(R*DZM)                                 
      DSQ=RQ*(DXM*DXM+DYM*DYM+DZM*DZM)
      BQ=DSQ*RQ*RQ
      P(6)=SQRT(DSQ/(RQ+3.*ZM*ZM))                                      
      P(7)=P(6)*(RQ+ZM*ZM)/(RQ*DZM)                                     
      RETURN                                                            
      END                                                               
C
