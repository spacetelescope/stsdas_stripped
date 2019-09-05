      subroutine sgp4_driver(MIDTIM,IEPT,DX,DY,DZ,DXDOT,DYDOT,DZDOT,
     1                       jepo,ISTAT)
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NORAD TLES" based ephemeris program (NASA)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c      1.2       Jun 01  A. Alexov       Added IDS's common block for file
c                                        header access
c  ------------------------------------------------------------------------
c      * DRIVER 3 NOV 80 
c      * WGS-72 PHYSICAL AND GEOPOTENTIAL CONSTANTS 
c      * CK2= .5*J2*AE**2 CK4=-.375*J4*AE**4
c   Modifications:  
c   M.R. Rosa   Apr 98     using jepo, jepoold one can speed up the
c                          search on a monotonic increasing time sequence
c                          of epochs 
c
      CHARACTER*80 TLEBUF(6830)

      INTEGER*4 IEXP,J,jepo,jepoold 
      INTEGER*4 IBEXP,IFLAG,IHG,IEPT,IDEEP
      INTEGER*4 ISET(5) 
      INTEGER ISTAT
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
      CHARACTER*80 CONTXT

      REAL*8 THETAG
      REAL*8 EPDS50
      REAL*8 XMNPDA,A1,AO,XKE,XNODP
      REAL*8 TSINCE 
      REAL*8 CK2,CK4
      REAL*8 E6A,QOMS2T,S,TOTHRD,QO,SO,XJ2,XJ3,XJ4,XKMPER,AE,TS,TF

      REAL*8 DX,DY,DZ,DXDOT,DYDOT,DZDOT
      REAL*8 DEL1,DELO,DELT  
      REAL*8 TEMP,MIDTIM,UT
      INTEGER*4 YEAR,MONTH,DAY,DAYOFY,LEAPY,jepo

      REAL*8 XMO,XNODEO,OMEGAO,EO,XINCL,XNO,XNDT2O,
     &           XNDD6O,BSTAR,X,Y,Z,XDOT,YDOT,ZDOT
      REAL*8 EPOCH,DS50,OBSEPO
      COMMON /E1/EPOCH,DS50,XMO,XNODEO,OMEGAO,EO,XINCL,XNO,XNDT2O,
     &           XNDD6O,BSTAR,X,Y,Z,XDOT,YDOT,ZDOT

      COMMON/C1/CK2,CK4,E6A,QOMS2T,S,TOTHRD,XJ3,XKE,XKMPER,XMNPDA,AE 

      REAL*8 PI,PIO2,TWOPI,X3PIO2 
      REAL*8 DE2RA,D
      COMMON /C2/ DE2RA,PI,PIO2,TWOPI,X3PIO2 

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

      DATA IHG/1HG/ 
      DATA DE2RA,E6A,PI,PIO2,QO,SO,TOTHRD,TWOPI,X3PIO2,XJ2,XJ3,
     1            XJ4,XKE,XKMPER,XMNPDA,AE/.174532925E-1,1.E-6,
     2            3.14159265,1.57079633,120.0,78.0,.66666667,
     4            6.2831853,4.71238898,1.082616E-3,-.253881E-5,
     5            -1.65597E-6,.743669161E-1,6378.135,1440.,1./

      DATA ISET/3HSGP,4HSGP4,4HSDP4,4HSGP8,4HSDP8/

      CALL NORAD_TLES(TLEBUF, ISTAT)
 
C check to make sure the status is OK, otherwise exit with error
      IF(ISTAT.NE.0)THEN
              GO TO 999
      ENDIF

      PI = 4.0D0*DATAN(1.0D0)
      DE2RA = PI/180.0D0

      CK2=.5*XJ2*AE**2
      CK4=-.375*XJ4*AE**4
      QOMS2T=((QO-SO)*AE/XKMPER)**4
      S=AE*(1.+SO/XKMPER)

c * SELECT EPHEMERIS TYPE AND OUTPUT TIMES
C TS,TF are start/finish times since epoch in minutes
c2     READ (5,700) IEPT, TS,TF,DELT
c      IF(IEPT.LE.0) STOP
c      yyy = (MIDTIM - 42413.0)/365.25+1975.  ! Time in years from 1975
c
c convert MJD into year, day
       CALL MJDTODATE (MIDTIM,YEAR,MONTH,DAY,UT)
c       type *,year,month,day
       leapy = 2
       if (mod(year,4).eq.0) leapy = 1
       Dayofy = int(275*MONTH/9)-leapy*INT(MONTH+9/12)+day-30
       obsepo = (year-1900)*1000.D0+dayofy
 2    IDEEP=0
c *search nearest epoch past or equal obsepo
c * READ IN MEAN ELEMENTS FROM 2 CARD T(TRANS) OR G(INTERN) FORMAT

      jepoold = MAX(jepo,1)       
      DO J = 1,6829,2 
c      DO J = jepoold,6829,2 
c        DECODE(61,761,TLEBUF(J)) EPOCH,XNDT2O,XNDD6O,IEXP,BSTAR,IBEXP
        READ(TLEBUF(J),761) EPOCH,XNDT2O,XNDD6O,IEXP,BSTAR,IBEXP
c        type *,obsepo,epoch
        READ(TLEBUF(J+1),763) XINCL,XNODEO,EO,OMEGAO,XMO,XNO 
        IF (OBSEPO.LE.EPOCH) THEN
c          DECODE(63,763,TLEBUF(J+1)) XINCL,XNODEO,EO,OMEGAO,XMO,XNO 
          READ(TLEBUF(J+1),763) XINCL,XNODEO,EO,OMEGAO,XMO,XNO 
          jepo = j 
c          type *,jepoold,jepo
          GOTO 98765
        ENDIF
      ENDDO   
c 761  FORMAT(18X,D14.8,1X,F10.8,2(1X,F6.5,I2))
c 763  FORMAT(7X,2(1X,F8.4),1X,F7.7,2(1X,F8.4),1X,F11.8)
 761  FORMAT(18X,F14.8,1X,F10.8,1X,F6.5,1X,I2,1X,F8.5,1X,I2)
 763  FORMAT(7X,2(1X,F8.4),1X,F9.7,1X,F8.4,1X,F10.4,1X,F11.8)

98765  CONTINUE

c       type *,EPOCH,XNDT2O,XNDD6O,IEXP,BSTAR,IBEXP,XINCL,
c     &                 XNODEO,EO,OMEGAO,XMO,XNO 

c my own stuff to get start times (in minutes since Epoch)
       EPDS50 = THETAG(EPOCH,D)
C MIDTIM is MJD of tsince
C DS50 is days since 1950 0,00 of Epoch
C MJD of Epoch is DS50+MJD(1950.000)
C tsince = MIDTIM - MJD of Epoch
c      type *,EPDS50,DS50,MIDTIM-(DS50+33282)
      ts = MIDTIM-(DS50+33282)
c      type *,year,day,ts  
c Why add 1 day and 1 sec (got it by trial and error)      
      TS = (TS+1.)*24.*60.+1./60. ! -5/60.
      TF = TS                     ! +10./60
      DELT = 1./60
c      type *,year,day,ts  
c----------------------------
      XNDD6O=XNDD6O*(10.**IEXP)
C RAASCNODE,ARGPERI, MeanANo and Incli in rad
      XNODEO=XNODEO*DE2RA
      OMEGAO=OMEGAO*DE2RA
      XMO=XMO*DE2RA 
70    XINCL=XINCL*DE2RA
C revs into rad 
      XNO=XNO*TWOPI/XMNPDA
      XNDT2O=XNDT2O*TWOPI/XMNPDA/XMNPDA 
      XNDD6O=XNDD6O*TWOPI/XMNPDA/XMNPDA/XMNPDA
c * INPUT CHECK FOR PERIOD VS EPHEMERIS SELECTED
c * PERIOD GE 225 MINUTES IS DEEP SPACE
      A1=(XKE/XNO)**TOTHRD
      TEMP=1.5*CK2*(3.*COS(XINCL)**2-1.)/(1.-EO*EO)**1.5
      DEL1=TEMP/(A1*A1)
      AO=A1*(1.-DEL1*(.5*TOTHRD+DEL1*(1.+134./81.*DEL1)))
      DELO=TEMP/(AO*AO)
      XNODP=XNO/(1.+DELO)
      IF((TWOPI/XNODP/XMNPDA) .GE. .15625) IDEEP=1
c Drag
      BSTAR=BSTAR*(10.**IBEXP)/AE

c loop on time
      TSINCE=TS
      IFLAG=1
      IF(IDEEP.EQ.1.AND.(IEPT.EQ.1.OR.IEPT.EQ.2.OR.IEPT.EQ.4)) THEN
        WRITE(6,930) 
930     FORMAT("SHOULD USE DEEP SPACE EPHEMERIS")
      ENDIF 
      IF(IDEEP.EQ.0.AND.(IEPT.EQ.3.OR.IEPT.EQ.5)) THEN
        WRITE(6,940) 
940     FORMAT("SHOULD USE NEAR EARTH EPHEMERIS")
      ENDIF 

c      type *,iept,iflag,ideep
10    GOTO (21,22,23,24,25), IEPT
21    STOP 
c      CALL SGP(IFLAG,TSINCE)
c      GOTO 60
22    CALL SGP4(IFLAG,TSINCE)
      GOTO 60
23    STOP 
c      CALL SDP4(IFLAG,TSINCE)
c      GOTO 60
24    CALL SGP8(IFLAG,TSINCE)
      GOTO 60
25    STOP 
c      CALL SDP8(IFLAG,TSINCE)
C convert to units of km and sec
60    X=X*XKMPER/AE
      Y=Y*XKMPER/AE
      Z=Z*XKMPER/AE
      XDOT=XDOT*XKMPER/AE*XMNPDA/86400.
      YDOT=YDOT*XKMPER/AE*XMNPDA/86400.
      ZDOT=ZDOT*XKMPER/AE*XMNPDA/86400.
      DX = X*1000.
      DY = Y*1000.
      DZ = Z*1000.
      DXDOT = XDOT*1000.
      DYDOT = YDOT*1000.
      DZDOT = ZDOT*1000.
c      GOTO 9999
c      TYPE *,TSINCE,X+3536.6604833,Y-5513.0955943,Z-2391.1981806  
c      TYPE *,TSINCE,XDOT+6.4402302,YDOT+3.0472113,ZDOT+2.4992428
c      WRITE(6,705) TSINCE,X,Y,Z,XDOT,YDOT,ZDOT
c705   FORMAT(7F17.8)

c      TSINCE=TSINCE+DELT 
c      IF(ABS(TSINCE) .GE. ABS(TF)) RETURN 
c      GOTO 10 

c700   FORMAT(I1,3F10.0)
c703   FORMAT(79X,A1)
       ISTAT=0
       GOTO 1000

 999    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
 1000   RETURN
        END

