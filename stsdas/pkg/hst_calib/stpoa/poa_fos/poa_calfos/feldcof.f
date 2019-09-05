C
       SUBROUTINE FELDCOF(YEAR,DIMO,ISTAT)
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NEWBILCAL" program to produce IGRF geomagnetic model (GSFC)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c      1.2       Jun 01  A. Alexov       Added IDS's common block for file
c                                        header access
c  ------------------------------------------------------------------------
C------------------------------------------------------------------------
C  DETERMINES COEFFICIENTS AND DIPOL MOMENT FROM IGRF MODELS
C
C       INPUT:        YEAR       DECIMAL YEAR FOR WHICH GEOMAGNETIC FIELD 
C                                is to be CALCULATED
C       OUTPUT:       DIMO       GEOMAGNETIC DIPOL MOMENT IN GAUSS (NORMALIZED 
C                                TO EARTH'S RADIUS) AT THE TIME (YEAR)
C  D. BILITZA, NSSDC, GSFC, CODE 633, GREENBELT, MD 20771, 
C       (301)286-9536   NOV 1987.
C-----------------------------------------------------------------------
       INTEGER  IU,IS,IER,IYEA,L,NUMYE,NMAX,NMAX1,NMAX2 
       INTEGER  I,J,N,M
       REAL*4   TIME,YEAR,DTE1,DTE2,SQRT2
       REAL*4   DTEMOD(12)
       REAL*4   UMR,ERAD,AQUAD,BQUAD
       REAL*4   GH1(144),GH2(120),GHA(144)
       REAL*8 DIMO,X,F0,F 
       CHARACTER*100 FIL1,FIL2,FILMOD(12) 
       CHARACTER*100 FILMOD_NEW(12)
       CHARACTER*8   FILMOD_NEW2(12)

C
C	HEADER I/O status message
C
      INTEGER USHPNF
      PARAMETER (USHPNF = 40)
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

C
C Local variables
C
        CHARACTER*80 CONTXT
        INTEGER      ISTATS(12)
        CHARACTER*64 DGFILE(12)

c ** MR ** changed Common to align on boundaries
c ** org **     COMMON/MODEL/       FIL1,NMAX,TIME,GH1
       COMMON/MODEL/       TIME,GH1,NMAX,FIL1
       COMMON/GENER/       UMR,ERAD,AQUAD,BQUAD
C ### changed to conform with IGRF 45-95, also FILMOD, DTEMOD arrays +1
       DATA  FILMOD / 'dgr/home/ecf-poa/cal/f45.dat', 
     1 '/home/ecf-poa/cal/dgrf50.dat','/home/ecf-poa/cal/dgrf55.dat',      
     2 '/home/ecf-poa/cal/dgrf60.dat','/home/ecf-poa/cal/dgrf65.dat',
     3 '/home/ecf-poa/cal/dgrf70.dat','/home/ecf-poa/cal/dgrf75.dat',
     4 '/home/ecf-poa/cal/dgrf80.dat','/home/ecf-poa/cal/dgrf85.dat', 
     5 '/home/ecf-poa/cal/dgrf90.dat','/home/ecf-poa/cal/igrf95.dat',
     6 '/home/ecf-poa/cal/igrf95s.dat' /
       DATA  FILMOD_NEW / 'stpoa$/poa_fos/ref/dgrf45.tab', 
     1 'stpoa$/poa_fos/ref/dgrf50.tab','stpoa$/poa_fos/ref/dgrf55.tab',      
     2 'stpoa$/poa_fos/ref/dgrf60.tab','stpoa$/poa_fos/ref/dgrf65.tab',
     3 'stpoa$/poa_fos/ref/dgrf70.tab','stpoa$/poa_fos/ref/dgrf75.tab',
     4 'stpoa$/poa_fos/ref/dgrf80.tab','stpoa$/poa_fos/ref/dgrf85.tab', 
     5 'stpoa$/poa_fos/ref/dgrf90.tab','stpoa$/poa_fos/ref/igrf95.tab',
     6 'stpoa$/poa_fos/ref/igrf95s.tab' /
       DATA DTEMOD / 1945., 1950., 1955., 1960., 1965.,           
     2               1970., 1975., 1980., 1985., 1990., 1995., 2000. /    

C A. Alexov - changed FILMOD_NEW to CDBS complient tables
C (skipping 1945, 1950, 1955, in order to limit 8 char header key)
        DATA FILMOD_NEW2/'CYCCSER0','CYCCSER0','CYCCSER0',
     *             'CYCCSER1','CYCCSER2','CYCCSER3','CYCCSER4',
     *		   'CYCCSER5','CYCCSER6','CYCCSER7','CYCCSER8',
     *             'CYCCSER9'/


C Init the ISTAT values
      DO 10 I=1,12
         ISTATS(I) = 0
10    CONTINUE

C
C  IS=0 FOR SCHMIDT NORMALIZATION   IS=1 GAUSS NORMALIZATION
C  IU  IS INPUT UNIT NUMBER FOR IGRF COEFFICIENT SETS
C  numye = numye + 1 ; is number of years represented by IGRF
c ... that is to read ... there are 11 base model years and
c     one extrapolated one (igrf95s)
   
       NUMYE=11
       IU = 10
       IS = 0
C-- DETERMINE IGRF-YEARS FOR INPUT-YEAR
       TIME = YEAR
       IYEA = INT(YEAR/5.)*5
       L = (IYEA - 1945)/5 + 1
       IF(L.LT.1) L=1
       IF(L.GT.NUMYE) L=NUMYE         
       DTE1 = DTEMOD(L)   

C Open the .d0h header, to get the names of the DGRF tables
      CALL UHDGST(IDS(1), FILMOD_NEW2(L), DGFILE(L), ISTATS(L))
      CALL UHDGST(IDS(1), FILMOD_NEW2(L+1), DGFILE(L+1), ISTATS(L+1))

C
C Check for errors encountered when reading keywords
C
       IF (ISTATS(L) .NE. 0) THEN
           CONTXT='ERROR: reading .d0h keyword '//FILMOD_NEW2(L)
           GO TO 999
       ELSE IF (ISTATS(L+1) .NE. 0) THEN
           CONTXT='ERROR: reading .d0h keyword '//FILMOD_NEW2(L+1)
           GO TO 999
       END IF
C

c AA - removed       FIL1 = FILMOD(L)
C AA - removed       FIL1 = FILMOD_NEW(L)
       FIL1 = DGFILE(L)
       DTE2 = DTEMOD(L+1) 
c AA - removed       FIL2 = FILMOD(L+1) 
c AA - removed       FIL2 = FILMOD_NEW(L+1)
       FIL2 = DGFILE(L+1)

CCC       type *,time,year,iyea,l,dte1,fil1,dte2,fil2 
C-- GET IGRF COEFFICIENTS FOR THE BOUNDARY YEARS
c       CALL GETSHC (IU, FIL1, NMAX1, ERAD, GH1, IER)  
       CALL GETSHC (IU, FIL1, NMAX1, ERAD, GH1, IER, L)  
       IF (IER .NE. 0) THEN                           
ccc         type *,'Problems with igrf data fil1 =',FIL1,IER
         WRITE (CONTXT,222)FIL1,IER
         STOP
       ENDIF 
 222   FORMAT('Problems with igrf data fil1 =',A100,1X,4I)
c       CALL GETSHC (IU, FIL2, NMAX2, ERAD, GH2, IER)  
       CALL GETSHC (IU, FIL2, NMAX2, ERAD, GH2, IER, L+1)  
       IF (IER .NE. 0) THEN                    
ccc         type *,'Problems with igrf data fil2 =',FIL2,IER
         WRITE (CONTXT,223)FIL2,IER
         STOP
       ENDIF 
 223   FORMAT('Problems with igrf data fil2 =',A100,1X,4I)
C-- DETERMINE IGRF COEFFICIENTS FOR YEAR
       IF (L .LE. NUMYE-1) THEN                        
         CALL INTERSHC (YEAR,DTE1,NMAX1,GH1,DTE2,NMAX2,GH2,NMAX,GHA)
CCC         type *,'INTERP',L,NUMYE
       ELSE               
         CALL EXTRASHC (YEAR,DTE1,NMAX1,GH1,DTE2,NMAX2,GH2,NMAX,GHA)
CCC         type *,'EXTRAP',L,NUMYE
       ENDIF 
C-- DETERMINE MAGNETIC DIPOL MOMENT AND COEFFIECIENTS G
       F0=0.D0
       DO 1234 J=1,3
          F = GHA(J) * 1.D-5
          F0 = F0 + F * F
1234       CONTINUE
       DIMO = DSQRT(F0)

       GH1(1) =  0.0
       I=2          
             F0=1.D-5                
             IF(IS.EQ.0) F0=-F0 
             SQRT2=SQRT(2.)      

      DO 9 N=1,NMAX           
       X = N
             F0 = F0 * X * X / (4.D0 * X - 2.D0)               
             IF(IS.EQ.0) F0 = F0 * (2.D0 * X - 1.D0) / X
       F = F0 * 0.5D0                                    
             IF(IS.EQ.0) F = F * SQRT2
       GH1(I) = GHA(I-1) * F0
       I = I+1                                         
      DO 9 M=1,N                                    
             F = F * (X + M) / (X - M + 1.D0)                 
       IF(IS.EQ.0) F = F * DSQRT((X - M + 1.D0) / (X + M))             
       GH1(I) = GHA(I-1) * F
       GH1(I+1) = GHA(I) * F
        I=I+2
9     CONTINUE


      ISTAT=0
      GO TO 1000
999   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTATS(I))
      ISTAT=1
1000  RETURN
      END

C
