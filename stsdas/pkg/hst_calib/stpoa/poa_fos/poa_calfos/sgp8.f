c * SGP8 14 NOV 80 
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NORAD TLES" based ephemreis program (NASA)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c  ------------------------------------------------------------------------
      SUBROUTINE SGP8(IFLAG,TSINCE)
c
      REAL*8 XMO,XNODEO,OMEGAO,EO,XINCL,XNO,XNDT2O,
     &           XNDD6O,BSTAR,X,Y,Z,XDOT,YDOT,ZDOT
      REAL*8 EPOCH,DS50
      COMMON /E1/EPOCH,DS50,XMO,XNODEO,OMEGAO,EO,XINCL,XNO,XNDT2O,
     &           XNDD6O,BSTAR,X,Y,Z,XDOT,YDOT,ZDOT
c      COMMON/E1/XMO,XNODEO,OMEGAO,EO,XINCL,XNO,XNDT2O,
c     1 XNDD6O,BSTAR,X,Y,Z,XDOT,YDOT,ZDOT,EPOCH,DS50

      INTEGER*4 iflag,isimp,i
 
      REAL*8 tsince,rho,a1,cosi,theta2,tthmun,eosq
      REAL*8 betao2,betao,del1,ao,delo,aodp,xnodp,b,po,pom2
      REAL*8 sini,sing,cosg,temp,sinio2,cosio2,theta4,unm5th
      REAL*8 unmth2,a3cof,pardt1,pardt2,pardt4,xmdt1,xgdt1,xhdt1
      REAL*8 xlldot,omgdt,xnodot,tsi,eta,eta2,psim2,alpha2,eeta
      REAL*8 cos2g,d5,d1,d2,d3,d4,b1,b2,b3,c0,c1,c4,c5,xndt,xndtn
      REAL*8 d6,d7,d8,c8,c9,edot,d20,aldtal,tsdtts,etdt,psdtps
      REAL*8 sin2g,c0dtc0,c1dtc1,d9,d10,d11,d12,d13,d14,d15
      REAL*8 d1dt,d2dt,d3dt,d4dt,d5dt,c4dt,c5dt,d16,xnddt,eddot
      REAL*8 d25,d17,tsddts,etddt,d18,d19,d23,d1ddt,xntrdt,tmnddt
      REAL*8 pp,gamma,xnd,qq,ed,ovgpp,xmam,omgasm,xnodes,temp1,xn
      REAL*8 em,z1,z7,zc2,sine,cose,zc5,cape,am,sinos,cosos
      REAL*8 axnm,aynm,pm,beta2m,g1,g2,g3,g4,g5,snf,beta,csf
      REAL*8 fm,snfg,csfg,sn2f2g,cs2f2g,ecosf,g10,rm,aovr,g13,g14,dr
      REAL*8 diwc,di,sni2du,xlamb,y4,y5,r,rdot,rvdot,snlamb,cslamb
      REAL*8 ux,vx,uy,vy,uz,vz

      REAL*8 fmod2p,actan

      REAL*8 CK2,CK4
      REAL*8 E6A,QOMS2T,S,TOTHRD,XJ3,XKE,XKMPER,XMNPDA,AE
      COMMON/C1/CK2,CK4,E6A,QOMS2T,S,TOTHRD,XJ3,XKE,XKMPER,XMNPDA,AE 

      DATA RHO/.15696615/ 
      IF (IFLAG .EQ. 0) GO TO 100 
c * RECOVER ORIGINAL MEAN MOTION (XNODP) AND SEMIMAJOR AXIS (AODP)
c * FROM INPUT ELEMENTS --------- CALCULATE BALLISTIC COEFFICIENT
c * (B TERM) FROM INPUT B* DRAG TERM
      A1=(XKE/XNO)**TOTHRD
      COSI=COS(XINCL)
      THETA2=COSI*COSI
      TTHMUN=3.*THETA2-1.
      EOSQ=EO*EO
      BETAO2=1.-EOSQ
      BETAO=SQRT(BETAO2)
      DEL1=1.5*CK2*TTHMUN/(A1*A1*BETAO*BETAO2)
      AO=A1*(1.-DEL1*(.5*TOTHRD+DEL1*(1.+134./81.*DEL1)))
      DELO=1.5*CK2*TTHMUN/(AO*AO*BETAO*BETAO2)
      AODP=AO/(1.-DELO)
      XNODP=XNO/(1.+DELO)
      B=2.*BSTAR/RHO

c * INITIALIZATION
      ISIMP=0
      PO=AODP*BETAO2
      POM2=1./(PO*PO)
      SINI=SIN(XINCL)
      SING=SIN(OMEGAO)
      COSG=COS(OMEGAO)
      TEMP=.5*XINCL
      SINIO2=SIN(TEMP)
      COSIO2=COS(TEMP)
      THETA4=THETA2**2
      UNM5TH=1.-5.*THETA2
      UNMTH2=1.-THETA2
      A3COF=-XJ3/CK2*AE**3
      PARDT1=3.*CK2*POM2*XNODP
      PARDT2=PARDT1*CK2*POM2
      PARDT4=1.25*CK4*POM2*POM2*XNODP
      XMDT1=.5*PARDT1*BETAO*TTHMUN
      XGDT1=-.5*PARDT1*UNM5TH
      XHDT1=-PARDT1*COSI
      XLLDOT=XNODP+XMDT1+
     2 .0625*PARDT2*BETAO*(13.-78.*THETA2+137.*THETA4)
      OMGDT=XGDT1+
     1 .0625*PARDT2*(7.-114.*THETA2+395.*THETA4)+PARDT4*(3.-36.*
     2 THETA2+49.*THETA4)
      XNODOT=XHDT1+
     1 (.5*PARDT2*(4.-19.*THETA2)+2.*PARDT4*(3.-7.*THETA2))*COSI
      TSI=1./(PO-S)
      ETA=EO*S*TSI
      ETA2=ETA**2
      PSIM2=ABS(1./(1.-ETA2))
      ALPHA2=1.+EOSQ
      EETA=EO*ETA
      COS2G=2.*COSG**2-1.
      D5=TSI*PSIM2
      D1=D5/PO
      D2=12.+ETA2*(36.+4.5*ETA2)
      D3=ETA2*(15.+2.5*ETA2)
      D4=ETA*(5.+3.75*ETA2)
      B1=CK2*TTHMUN
      B2=-CK2*UNMTH2
      B3=A3COF*SINI
      C0=.5*B*RHO*QOMS2T*XNODP*AODP*TSI**4*PSIM2**3.5/SQRT(ALPHA2)
      C1=1.5*XNODP*ALPHA2**2*C0
      C4=D1*D3*B2
      C5=D5*D4*B3
      XNDT=C1*(
     1 (2.+ETA2*(3.+34.*EOSQ)+5.*EETA*(4.+ETA2)+8.5*EOSQ)+
     1 D1*D2*B1+ C4*COS2G+C5*SING)
      XNDTN=XNDT/XNODP

c * IF DRAG IS VERY SMALL, THE ISIMP FLAG IS SET AND THE
c * EQUATIONS ARE TRUNCATED TO LINEAR VARIATION IN MEAN
c * MOTION AND QUADRATIC VARIATION IN MEAN ANOMALY
      IF(ABS(XNDTN*XMNPDA) .LT. 2.16E-3) GO TO 50
      D6=ETA*(30.+22.5*ETA2)
      D7=ETA*(5.+12.5*ETA2)
      D8=1.+ETA2*(6.75+ETA2)
      C8=D1*D7*B2
      C9=D5*D8*B3
      EDOT=-C0*(
     1 ETA*(4.+ETA2+EOSQ*(15.5+7.*ETA2))+EO*(5.+15.*ETA2)+
     1 D1*D6*B1 +
     1 C8*COS2G+C9*SING)
      D20=.5*TOTHRD*XNDTN
      ALDTAL=EO*EDOT/ALPHA2
      TSDTTS=2.*AODP*TSI*(D20*BETAO2+EO*EDOT)
      ETDT=(EDOT+EO*TSDTTS)*TSI*S
      PSDTPS=-ETA*ETDT*PSIM2
      SIN2G=2.*SING*COSG
      C0DTC0=D20+4.*TSDTTS-ALDTAL-7.*PSDTPS
      C1DTC1=XNDTN+4.*ALDTAL+C0DTC0
      D9=ETA*(6.+68.*EOSQ)+EO*(20.+15.*ETA2)
      D10=5.*ETA*(4.+ETA2)+EO*(17.+68.*ETA2)
      D11=ETA*(72.+18.*ETA2)
      D12=ETA*(30.+10.*ETA2)
      D13=5.+11.25*ETA2
      D14=TSDTTS-2.*PSDTPS
      D15=2.*(D20+EO*EDOT/BETAO2)
      D1DT=D1*(D14+D15)
      D2DT=ETDT*D11
      D3DT=ETDT*D12
      D4DT=ETDT*D13
      D5DT=D5*D14
      C4DT=B2*(D1DT*D3+D1*D3DT)
      C5DT=B3*(D5DT*D4+D5*D4DT)
      D16=
     1 D9*ETDT+D10*EDOT +
     1 B1*(D1DT*D2+D1*D2DT) +
     1 C4DT*COS2G+C5DT*SING+XGDT1*(C5*COSG-2.*C4*SIN2G)
      XNDDT=C1DTC1*XNDT+C1*D16
      EDDOT=C0DTC0*EDOT-C0*(
     1 (4.+3.*ETA2+30.*EETA+EOSQ*(15.5+21.*ETA2))*ETDT+(5.+15.*ETA2
     ' +EETA*(31.+14.*ETA2))*EDOT +
     1 B1*(D1DT*D6+D1*ETDT*(30.+67.5*ETA2)) +
     1 B2*(D1DT*D7+D1*ETDT*(5.+37.5*ETA2))*COS2G+
     1 B3*(D5DT*D8+D5*ETDT*ETA*(13.5+4.*ETA2))*SING+XGDT1*(C9*
     ' COSG-2.*C8*SIN2G))
      D25=EDOT**2
      D17=XNDDT/XNODP-XNDTN**2
      TSDDTS=2.*TSDTTS*(TSDTTS-D20)+AODP*TSI*(TOTHRD*BETAO2*D17-4.*D20*
     ' EO*EDOT+2.*(D25+EO*EDDOT))
      ETDDT =(EDDOT+2.*EDOT*TSDTTS)*TSI*S+TSDDTS*ETA
      D18=TSDDTS-TSDTTS**2
      D19=-PSDTPS**2/ETA2-ETA*ETDDT*PSIM2-PSDTPS**2
      D23=ETDT*ETDT
      D1DDT=D1DT*(D14+D15)+D1*(D18-2.*D19+TOTHRD*D17+2.*(ALPHA2*D25
     ' /BETAO2+EO*EDDOT)/BETAO2)
      XNTRDT=XNDT*(2.*TOTHRD*D17+3.*
     1 (D25+EO*EDDOT)/ALPHA2-6.*ALDTAL**2 +
     1 4.*D18-7.*D19 ) +
     1 C1DTC1*XNDDT+C1*(C1DTC1*D16+
     1 D9*ETDDT+D10*EDDOT+D23*(6.+30.*EETA+68.*EOSQ)+
     1 ETDT*EDOT*(40.+30.*
     ' ETA2+272.*EETA)+D25*(17.+68.*ETA2) +
     1 B1*(D1DDT*D2+2.*D1DT*D2DT+D1*(ETDDT*D11+D23*(72.+54.*ETA2))) +
     1 B2*(D1DDT*D3+2.*D1DT*D3DT+D1*(ETDDT*D12+D23*(30.+30.*ETA2))) *
     1 COS2G+
     1 B3*((D5DT*D14+D5*(D18-2.*D19)) *
     1 D4+2.*D4DT*D5DT+D5*(ETDDT*D13+22.5*ETA*D23)) *SING+XGDT1*
     1 ((7.*D20+4.*EO*EDOT/BETAO2)*
     ' (C5*COSG-2.*C4*SIN2G) 
     ' +((2.*C5DT*COSG-4.*C4DT*SIN2G)-XGDT1*(C5*SING+4.*
     ' C4*COS2G))))
      TMNDDT=XNDDT*1.E9
      TEMP=TMNDDT**2-XNDT*1.E18*XNTRDT
      PP=(TEMP+TMNDDT**2)/TEMP
      GAMMA=-XNTRDT/(XNDDT*(PP-2.))
      XND=XNDT/(PP*GAMMA)
      QQ=1.-EDDOT/(EDOT*GAMMA)
      ED=EDOT/(QQ*GAMMA)
      OVGPP=1./(GAMMA*(PP+1.))
      GO TO 70
50    ISIMP=1
      EDOT=-TOTHRD*XNDTN*(1.-EO)
70    IFLAG=0

c * UPDATE FOR SECULAR GRAVITY AND ATMOSPHERIC DRAG
100   XMAM=FMOD2P(XMO+XLLDOT*TSINCE)
      OMGASM=OMEGAO+OMGDT*TSINCE
      XNODES=XNODEO+XNODOT*TSINCE
      IF(ISIMP .EQ. 1) GO TO 105
      TEMP=1.-GAMMA*TSINCE
      TEMP1=TEMP**PP
      XN=XNODP+XND*(1.-TEMP1)
      EM=EO+ED*(1.-TEMP**QQ)
      Z1=XND*(TSINCE+OVGPP*(TEMP*TEMP1-1.))
      GO TO 108
105   XN=XNODP+XNDT*TSINCE
      EM=EO+EDOT*TSINCE
      Z1=.5*XNDT*TSINCE*TSINCE
108   Z7=3.5*TOTHRD*Z1/XNODP
      XMAM=FMOD2P(XMAM+Z1+Z7*XMDT1)
      OMGASM=OMGASM+Z7*XGDT1
      XNODES=XNODES+Z7*XHDT1

c * SOLVE KEPLERS EQUATION
      ZC2=XMAM+EM*SIN(XMAM)*(1.+EM*COS(XMAM))
      DO 130 I=1,10
      SINE=SIN(ZC2)
      COSE=COS(ZC2)
      ZC5=1./(1.-EM*COSE)
      CAPE=(XMAM+EM*SINE-ZC2)*
     1 ZC5+ZC2
      IF(ABS(CAPE-ZC2) .LE. E6A) GO TO 140
130   ZC2=CAPE

c * SHORT PERIOD PRELIMINARY QUANTITIES
140   AM=(XKE/XN)**TOTHRD
      BETA2M=1.-EM*EM
      SINOS=SIN(OMGASM)
      COSOS=COS(OMGASM)
      AXNM=EM*COSOS
      AYNM=EM*SINOS
      PM=AM*BETA2M
      G1=1./PM
      G2=.5*CK2*G1
      G3=G2*G1
      BETA=SQRT(BETA2M)
      G4=.25*A3COF*SINI
      G5=.25*A3COF*G1
      SNF=BETA*SINE*ZC5
      CSF=(COSE-EM)*ZC5
      FM=ACTAN(SNF,CSF)
      SNFG=SNF*COSOS+CSF*SINOS
      CSFG=CSF*COSOS-SNF*SINOS
      SN2F2G=2.*SNFG*CSFG
      CS2F2G=2.*CSFG**2-1.
      ECOSF=EM*CSF
      G10=FM-XMAM+EM*SNF
      RM=PM/(1.+ECOSF)
      AOVR=AM/RM
      G13=XN*AOVR
      G14=-G13*AOVR
      DR=G2*(UNMTH2*CS2F2G-3.*TTHMUN)-G4*SNFG
      DIWC=3.*G3*SINI*CS2F2G-G5*AYNM
      DI=DIWC*COSI

c * UPDATE FOR SHORT PERIOD PERIODICS
      SNI2DU=SINIO2*(
     1 G3*(.5*(1.-7.*THETA2)*SN2F2G-3.*UNM5TH*G10)-G5*SINI*CSFG*(2.+
     2 ECOSF))-.5*G5*THETA2*AXNM/COSIO2
      XLAMB=FM+OMGASM+XNODES+G3*(.5*(1.+6.*COSI-7.*THETA2)*SN2F2G-3.*
     1 (UNM5TH+2.*COSI)*G10)+G5*SINI*(COSI*AXNM/(1.+COSI)-(2.
     2 +ECOSF)*CSFG)
      Y4=SINIO2*SNFG+CSFG*SNI2DU+.5*SNFG*COSIO2*DI
      Y5=SINIO2*CSFG-SNFG*SNI2DU+.5*CSFG*COSIO2*DI
      R=RM+DR
      RDOT=XN*AM*EM*SNF/BETA+G14*(2.*G2*UNMTH2*SN2F2G+G4*CSFG)
      RVDOT=XN*AM**2*BETA/RM+
     1 G14*DR+AM*G13*SINI*DIWC

c * ORIENTATION VECTORS
      SNLAMB=SIN(XLAMB)
      CSLAMB=COS(XLAMB)
      TEMP=2.*(Y5*SNLAMB-Y4*CSLAMB)
      UX=Y4*TEMP+CSLAMB
      VX=Y5*TEMP-SNLAMB
      TEMP=2.*(Y5*CSLAMB+Y4*SNLAMB)
      UY=-Y4*TEMP+SNLAMB
      VY=-Y5*TEMP+CSLAMB
      TEMP=2.*SQRT(1.-Y4*Y4-Y5*Y5)
      UZ=Y4*TEMP 
      VZ=Y5*TEMP

c * POSITION AND VELOCITY
      X=R*UX
      Y=R*UY
      Z=R*UZ
      XDOT=RDOT*UX+RVDOT*VX
      YDOT=RDOT*UY+RVDOT*VY
      ZDOT=RDOT*UZ+RVDOT*VZ

      RETURN
      END
