      SUBROUTINE POLIMG
C
C     This FORTRAN program produces simulated images and
C     error images corresponding to the polarizers of
C     HST instruments. Currently FOC, WFPC2, NICMOS and 
C     ACS are included. A non-HST instrument can also be 
C     handled called `SPECIAL'. The predicted observed 
C     signal at the polarizer angles of the instrument is 
C     formed from a set of total intensity, linear 
C     polarization, polarization position angle and 
C     circular polarization images. 
C
C     The total intensity, linear polarization, polarization 
C     position angle and circular polarization files are read. 
C     A table of the required rotator angles of the polarizer 
C     corresponding to the output intensity images is required. 
C     The instrument name, polarizer and colour filters names 
C     and telescope rotator position angle (PA_V3) are 
C     prompted. The instrumental linear polarization and 
C     position angle (measured in the same system as the input 
C     polarizer angles) is used to correct the input 
C     polarization to the observed frame. The conversion factor 
C     to electrons (or equivalent units for Poisson noise 
C     computation) and the read-out noise are also required 
C     in order to produce the output images. The output images 
C     and their error arrays are output. The output files have
C     a root name followed by a postfix of the integer polarizer
C     angle
C
C     Written by  J. R. Walsh, ST-ECF, ESO (jwalsh@eso.org)  December 1999
C     Version 1.00
C     Modified to include ACS and extra checking for polarizer filter November 2000
C     Version 2.00
C
      IMPLICIT NONE

      INTEGER I

      INTEGER STAT
      INTEGER STAT1
      INTEGER STAT2
      INTEGER STAT3

      INTEGER IMDSCR1
      INTEGER IMDSCR2
      INTEGER IMDSCR3
      INTEGER IMDSCR4
      INTEGER IMDSCR5
      INTEGER IMDSCR6
      INTEGER TBDSCR

      INTEGER NAXISI
      INTEGER DIMEN(7)
      INTEGER ODIMEN(7)
      INTEGER DTYPE

      INTEGER N1
      INTEGER N2
      INTEGER P1
      INTEGER P2
      INTEGER Q1
      INTEGER Q2
      INTEGER V1
      INTEGER V2
      INTEGER NROWS
      INTEGER NOTH
      INTEGER ILEN

      INTEGER MEMI(1)

      INTEGER TOTI
      INTEGER LPOL
      INTEGER POPA
      INTEGER CPOL
 
      INTEGER THETA
      INTEGER REFANG
      INTEGER CORANG
      INTEGER DETECT

      INTEGER ARRANG
      INTEGER ARRTPAR
      INTEGER ARRTPER
      INTEGER ARRRS
      INTEGER ARRRP
      INTEGER ARRDELPH
      INTEGER ARRINPO
      INTEGER ARRINPA
      INTEGER LANG

      INTEGER TPAR
      INTEGER TPER
      INTEGER RS
      INTEGER RP
      INTEGER DELPH
      INTEGER INSPO
      INTEGER INSPA

      INTEGER EFFI
      INTEGER EFFQ
      INTEGER EFFU
      INTEGER EFFV

      INTEGER SEED
      INTEGER NELEM
      INTEGER MODI

      INTEGER CUBINT
      INTEGER CUBERR
      INTEGER OUTI
      INTEGER OUTIER

      REAL PA_V3
      REAL ORIENT
      REAL UNBACK
      REAL E2ADU
      REAL RON

      REAL MATPOL(4,4)
      REAL MATROP(4,4)
      REAL MATMIR(4,4)
      REAL MATROT(4,4)
      REAL WORK(4,4)
      REAL MATSYS(4,4)

      REAL MEMR(1)

      CHARACTER*16 INSTNAME
      CHARACTER*16 POLFNAME
      CHARACTER*16 FILTNAME
      CHARACTER*16 APOLFNAM(64)
      
      CHARACTER*32 INTOIN
      CHARACTER*32 INPOL
      CHARACTER*32 INPA
      CHARACTER*32 INCIR
      CHARACTER*32 TABNAM

      CHARACTER*32 OUTROOT
      CHARACTER*32 ONAMI
      CHARACTER*32 ONAME
      
      CHARACTER*16 ARRNAM(128)

      CHARACTER*132 OUTEXT

      LOGICAL LINST
      LOGICAL LMATCH
      LOGICAL TEXIST
      LOGICAL LSTAT
      LOGICAL LUSTAT
      LOGICAL MEMB(1)

      COMMON/MEM/MEMR
      EQUIVALENCE (MEMI,MEMR,MEMB)
C
C     Get name of the total intensity data set name 
C
100   CALL UCLGST('inint',INTOIN,STAT)
      CALL UIMOPN(INTOIN,1,IMDSCR1,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,105) INTOIN
105     FORMAT(' Error opening data file ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMGID(IMDSCR1,DTYPE,NAXISI,DIMEN,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,107) INTOIN
107     FORMAT(' Error reading data file header ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      IF (NAXISI.NE.2) THEN
        WRITE(OUTEXT,109) 
109     FORMAT(' Data file is not 2-dimensional')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      N1=DIMEN(1)
      N2=DIMEN(2)
C
C     Allocate dynamic memory for the 2-D input image
C     of dimensions N1 by N2 (real number)
C
      NELEM=N1*N2
      CALL UDMGET(NELEM,6,TOTI,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,121) 
121     FORMAT(' Unable to assign memory for internal 2-D array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read the input image to TOTI
C
      CALL UIGS2R(IMDSCR1,1,N1,1,N2,MEMR(TOTI),STAT3)
      IF (STAT3.NE.0) THEN
        WRITE(OUTEXT,123) INTOIN
123     FORMAT(' Error reading data from file ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get name of the linear polarization data set name 
C
140   CALL UCLGST('inpol',INPOL,STAT)
      CALL UIMOPN(INPOL,1,IMDSCR2,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,105) INPOL
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMGID(IMDSCR2,DTYPE,NAXISI,DIMEN,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,107) INPOL
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      IF (NAXISI.NE.2) THEN
        WRITE(OUTEXT,109) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      P1=DIMEN(1)
      P2=DIMEN(2)
C
C     Check input dimensions of polarization image same 
C     as total intensity image
C
      IF (P1.NE.N1.OR.P2.NE.N2) THEN
        WRITE(OUTEXT,145) 
145     FORMAT(' Dimensions of input intensity and polarization
     : images not equal')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the 2-D input image
C     of dimensions P1 by P2 (real number)
C
      NELEM=P1*P2
      CALL UDMGET(NELEM,6,LPOL,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,121) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read the input image to LPOL
C
      CALL UIGS2R(IMDSCR2,1,P1,1,P2,MEMR(LPOL),STAT3)
      IF (STAT3.NE.0) THEN
        WRITE(OUTEXT,123) INPOL
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get name of the polarization position angle data set 
C     name 
C
180   CALL UCLGST('inpopa',INPA,STAT)
      CALL UIMOPN(INPA,1,IMDSCR3,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,105) INPA
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMGID(IMDSCR3,DTYPE,NAXISI,DIMEN,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,107) INPA
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      IF (NAXISI.NE.2) THEN
        WRITE(OUTEXT,109) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      Q1=DIMEN(1)
      Q2=DIMEN(2)
C
C     Check input dimensions of polarization position angle
C     image same as total intensity image
C
      IF (Q1.NE.N1.OR.Q2.NE.N2) THEN
        WRITE(OUTEXT,185) 
185     FORMAT(' Dimensions of input intensity and polarization
     : position angle images not equal')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the 2-D input image
C     of dimensions Q1 by Q2 (real number)
C
      NELEM=Q1*Q2
      CALL UDMGET(NELEM,6,POPA,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,121) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read the input image to POPA
C
      CALL UIGS2R(IMDSCR3,1,Q1,1,Q2,MEMR(POPA),STAT3)
      IF (STAT3.NE.0) THEN
        WRITE(OUTEXT,123) INPA
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get name of the circular polarization data set name 
C
190   CALL UCLGST('incir',INCIR,STAT)
      CALL UIMOPN(INCIR,1,IMDSCR4,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,105) INCIR
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMGID(IMDSCR4,DTYPE,NAXISI,DIMEN,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,107) INCIR
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      IF (NAXISI.NE.2) THEN
        WRITE(OUTEXT,109) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      V1=DIMEN(1)
      V2=DIMEN(2)
C
C     Check input dimensions of polarization position angle
C     image same as total intensity image
C
      IF (V1.NE.N1.OR.V2.NE.N2) THEN
        WRITE(OUTEXT,195) 
195     FORMAT(' Dimensions of input intensity and circular
     : polarization images not equal')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the 2-D input image
C     of dimensions V1 by V2 (real number)
C
      NELEM=V1*V2
      CALL UDMGET(NELEM,6,CPOL,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,121) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read the input image to CPOL
C
      CALL UIGS2R(IMDSCR4,1,V1,1,V2,MEMR(CPOL),STAT3)
      IF (STAT3.NE.0) THEN
        WRITE(OUTEXT,123) INCIR
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Close the input polarization and position angle images
C
      CALL UIMCLO(IMDSCR2,STAT)
      CALL UIMCLO(IMDSCR3,STAT)
      CALL UIMCLO(IMDSCR4,STAT)
C
C     Get the instrument specifics - instrument name,
C     polarizer and colour filter name and telescope rotator
C     angle
C
C     First get the name of the HST instrument and set the
C     number of output files (NOTH)
C
      CALL UCLGST('instr',INSTNAME,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,311)
311     FORMAT(' Failed to get name of HST instrument')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Check that name of instrument is valid
C
      LINST=.FALSE.
      IF (INSTNAME(:3).EQ.'ACS') THEN
        NOTH=3
        LINST=.TRUE.
      ENDIF
      IF (INSTNAME(:3).EQ.'FOC') THEN
        NOTH=3
        LINST=.TRUE.
      ENDIF
      IF (INSTNAME(:6).EQ.'NICMOS') THEN
        NOTH=3
        LINST=.TRUE.
      ENDIF
      IF (INSTNAME(:5).EQ.'WFPC2') THEN
        NOTH=7
        LINST=.TRUE.
      ENDIF
      IF (INSTNAME(:7).EQ.'SPECIAL') THEN
        LINST=.TRUE.
      ENDIF
      IF (.NOT.LINST) THEN
        WRITE(OUTEXT,315)
315     FORMAT(' Failed to get valid name of HST instrument')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the name of the polarizer filter (only for NICMOS)
C
      IF (INSTNAME(:6).EQ.'NICMOS') THEN
        CALL UCLGST('polname',POLFNAME,STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,321)
321       FORMAT(' Failed to get name of polarizer filter')
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
      ENDIF
C
C     Get the name of the polarizer filter (only for ACS)
C
      IF (INSTNAME(:3).EQ.'ACS') THEN
        CALL UCLGST('polname',POLFNAME,STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,323)
323       FORMAT(' Failed to get name of polarizer filter')
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
      ENDIF
C
C     Get the name of the filter (only for SPECIAL)
C
      IF (INSTNAME(:7).EQ.'SPECIAL') THEN
        CALL UCLGST('filtnam',FILTNAME,STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,326)
326       FORMAT(' Failed to get name of filter')
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
C
C       Check name of filter is not empty
C
        IF (FILTNAME(:5).EQ.'     ') THEN
          WRITE(OUTEXT,328)
328       FORMAT(' Empty field for name of filter')
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
      ENDIF
C
C     Get the name of the colour filter (not for NICMOS)
C
      IF (INSTNAME(:6).NE.'NICMOS') THEN
        CALL UCLGST('filtnam',FILTNAME,STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,331)
331       FORMAT(' Failed to get name of colour filter')
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
C
C       Check name of filter is not empty
C
        IF (FILTNAME(:5).EQ.'     ') THEN
          WRITE(OUTEXT,335)
335       FORMAT(' Empty field for name of colour filter')
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
      ENDIF
C
C     Get the name of the telescope rotator position angle
C     (HST PA_V3)
C
      CALL UCLGSR('PA_V3',PA_V3,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,341)
341     FORMAT(' Failed to get telescope rotator position angle')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the name of the table file for the list of 
C     instrument polarization and filter properties. The 
C     tables are instrument specific
C
350   CALL UCLGST('instpoltab',TABNAM,STAT)
      CALL UTTACC(TABNAM,TEXIST,STAT)
      ILEN=INDEX(TABNAM,'   ') -1
      IF (.NOT.TEXIST) THEN
        WRITE(OUTEXT,361) TABNAM(:ILEN) 
361     FORMAT(' Table file ',A,' does not exist ')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UTTOPN(TABNAM,1,TBDSCR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,363) TABNAM(:ILEN)
363     FORMAT(' Table file ',A,' could not be opened')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the number of rows in the table
C
      CALL UTPGTI(TBDSCR,21,NROWS,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,365) TABNAM(:ILEN)
365     FORMAT(' Failed to find no. rows in table file ',A)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     For the SPECIAL instrument the number of rotator 
C     angles is set by the number of entries in the 
C     table file
C
      IF (INSTNAME(:7).EQ.'SPECIAL') THEN
        NOTH=NROWS
      ENDIF
C
C     Allocate dynamic memory for the array THETA of
C     polarizer rotator angles, REFANG for the corrections 
C     to the polarizer angles, CORANG for the corrections to
C     the instrument position angle and DETECT for the array
C     of the WFPC2 detector number
C
      CALL UDMGET(NOTH,6,THETA,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,371)
371     FORMAT(' Unable to assign memory for internal',
     :' 1-D real array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOTH,6,REFANG,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,371)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOTH,6,CORANG,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,371)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOTH,4,DETECT,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,373)
373     FORMAT(' Unable to assign memory for internal',
     :' 1-D integer array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Depending on the instrument, the name of the polarizing
C     filter and/or the colour filter set the polarizer angle, 
C     the correction to polarizer angle, the correction to the
C     rotator (ORIENT) angle, the array of polarizer filter names
C     and the detector number (WFPC2 only)
C
400   CALL INSTSET(INSTNAME,POLFNAME,FILTNAME,NOTH,
     :             MEMR(THETA),MEMR(REFANG),MEMR(CORANG),
     :             APOLFNAM,MEMI(DETECT),PA_V3,ORIENT,LMATCH)
C
C     Check that filter name was matched
C
      IF (.NOT.LMATCH) THEN
        WRITE(OUTEXT,411)
411     FORMAT(' Error matching polarizer filter name ')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the data read from the
C     polarizer table file for the arrays of polarizer
C     angle (ARRANG), transmission parallel (ARRTPAR),
C     transmission perpendicular (ARRTPER), reflectance Rs
C     (ARRRS),  reflectance Rp (ARRRP), retardance angle
C     (ARRDELPH), instumental polarization (ARRINPO) and
C     PA of instrumental polarization (ARRINPA) and the 
C     logical flag array required by UTCGT 
C
      CALL UDMGET(NROWS,6,ARRANG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
421     FORMAT(' Unable to assign memory for internal 1-D real array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRTPAR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRTPER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRRS,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRRP,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRDELPH,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRINPO,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRINPA,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,1,LANG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,423)
423     FORMAT(' Unable to assign memory for internal 1-D ',
     :' Boolean array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the arrays of polarizer
C     transmission parallel (TPAR), transmission 
C     perpendicular (TPER), reflectance Rs (RS), 
C     reflectance Rp (RP), retardance angle (DELPH), 
C     instumental polarization (INPO) and PA of 
C     instrumental polarization (INPA) for the NOTH
C     data images
C
      CALL UDMGET(NOTH,6,TPAR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOTH,6,TPER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOTH,6,RS,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOTH,6,RP,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOTH,6,DELPH,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOTH,6,INSPO,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOTH,6,INSPA,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     From the table read the instrumental polarization parameters
C     appropriate to this instrument given by polarizer / colour 
C     filter, polarizer angle if appropriate.
C
      CALL READ_TABPOL(TBDSCR,INSTNAME,POLFNAME,FILTNAME,
     :                 APOLFNAM,NROWS,ARRNAM,MEMR(ARRANG),
     :                 MEMR(ARRTPAR),MEMR(ARRTPER),MEMR(ARRRS),
     :                 MEMR(ARRRP),MEMR(ARRDELPH),MEMR(ARRINPO),
     :                 MEMR(ARRINPA),MEMB(LANG),NOTH,
     :                 MEMR(THETA),MEMR(TPAR),MEMR(TPER),
     :                 MEMR(RS),MEMR(RP),MEMR(DELPH),MEMR(INSPO),
     :                 MEMR(INSPA),LSTAT)
      IF (.NOT.LSTAT) THEN
        WRITE(OUTEXT,433) TABNAM
433     FORMAT(' Error reading data from table file ',A32)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Close down the polarization parameter table 
C
      CALL UTTCLO(TBDSCR,STAT)
C
C     Free the dynamic memory allocated for arrays of
C     polarization parameters read from the table file
C
      CALL UDMFRE(ARRANG,6,STAT)
      CALL UDMFRE(ARRTPAR,6,STAT)
      CALL UDMFRE(ARRTPER,6,STAT)
      CALL UDMFRE(ARRRS,6,STAT)
      CALL UDMFRE(ARRRP,6,STAT)
      CALL UDMFRE(ARRDELPH,6,STAT)
      CALL UDMFRE(ARRINPO,6,STAT)
      CALL UDMFRE(ARRINPA,6,STAT)
      CALL UDMFRE(LANG,1,STAT)
C
C     Form the system of equations for the polarization
C     response of the system to convert Stokes Parameters 
C     I, Q and U to measured counts and set the arrays of 
C     efficiencies for the I (EFFI), Q (EFFQ), U (EFFU)
C     and V (EFFV) Stokes parameters 
C
C     First allocate space for the efficiency arrays
C
      CALL UDMGET(NOTH,6,EFFI,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOTH,6,EFFQ,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOTH,6,EFFU,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOTH,6,EFFV,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,421)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Call the subroutine to compute the system efficiences
C
      CALL SYSEFFIC(NOTH,MEMR(TPAR),MEMR(TPER),MEMR(THETA),
     :              MEMR(REFANG),MEMR(RS),MEMR(RP),
     :              MEMR(DELPH),ORIENT,MEMR(CORANG),
     :              4,4,MATPOL,MATROP,MATMIR,MATROT,WORK,
     :              MATSYS,MEMR(EFFI),MEMR(EFFQ),MEMR(EFFU),
     :              MEMR(EFFV))
C
C     Free up dynamic memory no longer required for arrays
C     of image instrumental polarization parameters
C
      CALL UDMFRE(TPAR,6,STAT)
      CALL UDMFRE(TPER,6,STAT)
      CALL UDMFRE(RS,6,STAT)
      CALL UDMFRE(RP,6,STAT)
      CALL UDMFRE(DELPH,6,STAT)
C
C     Get the value of the unpolarized background to add to the 
C     output images 
C
      CALL UCLGSR('unback',UNBACK,STAT2) 
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,451)
451     FORMAT(' Failed to get additive unpolarized background value')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the Poisson units (electrons) to ADU conversion factor 
C     for computing the noise 
C
      CALL UCLGSR('e_adu',E2ADU,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,453)
453     FORMAT(' Failed to get e/ADU conversion factor')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the read-out noise for the images
C
      CALL UCLGSR('read_noise',RON,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,455)
455     FORMAT(' Failed to get read-out noise')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the integer value of the seed for the random
C     number generator
C
      CALL UCLGSI('seed',SEED,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,457)
457     FORMAT(' Failed to get value for seed for random
     : number generation')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the arrays required by
C     SIMINST
C     Real arrays: MODI, CUBINT, CUBERR
C
C     1-D array for holding values of intensity as a function
C     of polarizer angle
C
500   CALL UDMGET(NOTH,6,MODI,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,511)
511     FORMAT(' Unable to assign memory for internal 1-D',
     :' real array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     3-D array of NOTH output intensity images of dimensions 
C     N1 by N2 (real numbers)
C
      NELEM=NOTH*N1*N2 
      CALL UDMGET(NELEM,6,CUBINT,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,550) 
550     FORMAT(' Unable to assign memory for internal 3-D array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     3-D array of NOANG output intensity error images of 
C     dimensions N1 by N2 (real numbers)
C
      CALL UDMGET(NELEM,6,CUBERR,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,550) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Call the grand subroutine to compute the intensity
C     image in ADU and the error image at each of the 
C     NOTH angles of the polarizing element
C
      CALL SIMINST(N1,N2,MEMR(TOTI),MEMR(LPOL),MEMR(POPA),
     :             MEMR(CPOL),NOTH,MEMR(EFFI),MEMR(EFFQ),
     :             MEMR(EFFU),MEMR(EFFV),UNBACK,E2ADU,RON,
     :             MEMR(INSPO),MEMR(INSPA),ORIENT,
     :             MEMR(CORANG),SEED,MEMR(MODI),MEMR(CUBINT),
     :             MEMR(CUBERR))
C
C     Free the dynamic memory allocated which is no longer 
C     required
C
      CALL UDMFRE(TOTI,6,STAT)
      CALL UDMFRE(LPOL,6,STAT)
      CALL UDMFRE(POPA,6,STAT)
      CALL UDMFRE(CPOL,6,STAT)
      CALL UDMFRE(EFFI,6,STAT)
      CALL UDMFRE(EFFQ,6,STAT)
      CALL UDMFRE(EFFU,6,STAT)
      CALL UDMFRE(EFFV,6,STAT)
      CALL UDMFRE(INSPO,6,STAT)
      CALL UDMFRE(INSPA,6,STAT)
      CALL UDMFRE(MODI,6,STAT)
C
C     Get the rootname for the output images
C
600   CALL UCLGST('outroot',OUTROOT,STAT)
      ODIMEN(1)=N1
      ODIMEN(2)=N2
C
C     Allocate memory for the output 2D arrays
C     of intensity and error
C
      NELEM=N1*N2
      CALL UDMGET(NELEM,6,OUTI,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,611)
611     FORMAT(' Unable to assign memory for internal 2-D',
     :' real array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,OUTIER,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,611)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Loop through the NOTH planes of the cubes CUBINT 
C     and CUBERR, copy each to 2-D arrays and then 
C     write these out
C
      DO I=1,NOTH,1
C
C       Copy the intensity array from the I'th plane of
C       CUBINT and the intensity error array from the
C       I'th plane of CUBERR
C
        CALL OUTOCUB(N1,N2,NOTH,MEMR(CUBINT),N1,N2,MEMR(OUTI),I)
        CALL OUTOCUB(N1,N2,NOTH,MEMR(CUBERR),N1,N2,MEMR(OUTIER),I)
C
C       From the rootname and the angle produce file names 
C       for the intensity and error 
C         
        CALL NAMMAK(OUTROOT,NOTH,MEMR(THETA),I,ONAMI,ONAME)
C
C       Create the intensity file
C
        CALL UIMCRE(ONAMI,6,2,ODIMEN,IMDSCR5,STAT1)
        IF (STAT1.EQ.0) THEN
          CALL UHDCPY(IMDSCR1,IMDSCR5,STAT2)
          CALL UIPS2R(IMDSCR5,1,N1,1,N2,MEMR(OUTI),STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,621) ONAMI
621         FORMAT(' Error writing data to file ',A24)
            CALL UMSPUT(OUTEXT,3,0,STAT)
            GO TO 990
          ENDIF
C
C         Update header with instrument name, filter names(s),
C         detector (WFPC2 only) and instrument rotator
C
          CALL HEADUPD(ONAMI,IMDSCR5,INSTNAME,APOLFNAM,FILTNAME,
     :                 NOTH,I,MEMR(THETA),MEMI(DETECT),ORIENT,
     :                 LUSTAT)
          IF (.NOT.LUSTAT) THEN
            WRITE(OUTEXT,623) ONAMI
623         FORMAT(' Error updating file header ',A24)
            CALL UMSPUT(OUTEXT,3,0,STAT)
          ENDIF
          CALL UIMCLO(IMDSCR5,STAT3)
        ELSE
          WRITE(OUTEXT,625) ONAMI
625       FORMAT(' Failed to open data file ',A24)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
C
C       Create the intensity error file
C
        CALL UIMCRE(ONAME,6,2,ODIMEN,IMDSCR6,STAT1)
        IF (STAT1.EQ.0) THEN
          CALL UHDCPY(IMDSCR1,IMDSCR6,STAT2)
          CALL UIPS2R(IMDSCR6,1,N1,1,N2,MEMR(OUTIER),STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,621) ONAME
            CALL UMSPUT(OUTEXT,3,0,STAT)
            GO TO 990
          ENDIF
C
C         Update header with instrument name, filter names(s),
C         detector (WFPC2 only) and instrument rotator
C
          CALL HEADUPD(ONAME,IMDSCR6,INSTNAME,APOLFNAM,FILTNAME,
     :                 NOTH,I,MEMR(THETA),MEMI(DETECT),ORIENT,
     :                 LUSTAT)
          IF (.NOT.LUSTAT) THEN
            WRITE(OUTEXT,623) ONAME
            CALL UMSPUT(OUTEXT,3,0,STAT)
          ENDIF
          CALL UIMCLO(IMDSCR6,STAT3)
        ELSE
          WRITE(OUTEXT,625) ONAME
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
      ENDDO
C
C     Free the allocated memory
C
      CALL UDMFRE(DETECT,4,STAT)
      CALL UDMFRE(THETA,6,STAT)
      CALL UDMFRE(OUTI,6,STAT)
      CALL UDMFRE(OUTIER,6,STAT)
      CALL UDMFRE(CUBINT,6,STAT)
      CALL UDMFRE(CUBERR,6,STAT)
C
C     Close the input intensity image
C
      CALL UIMCLO(IMDSCR1,STAT)
      GO TO 999

990   WRITE(OUTEXT,991)
991   FORMAT(' Program failed. No output')
      CALL UMSPUT(OUTEXT,1,0,STAT)

999   END

      SUBROUTINE INSTSET(INSTNAME,POLNAME,FILTNAME,N,
     :                   THETA,REFANG,CORANG,APNAME,DETECT,
     :                   PA_V3,ORIENT,LMATCH)
C
C     This subroutine sets the array of polarizer angles (THETA),
C     the array of additive corrections to the polarizer angle
C     (REFANG), the array of corrections to the instrument 
C     rotator angle (CORANG) and the array of polarizer names
C     (APNAME) for the specific instrument, polarizer and filter 
C     combination specified by INSTNAME, POLNAME and FILTNAME 
C     respectively. The input PA_V3 is converted to the ORIENT
C     angle value required by SYSEFFIC depending on the instrument 
C     If the polarizer filter name is not matched then LMATCH
C     is returned false
C
      IMPLICIT NONE
      INTEGER N ! No. of elements in output arrays 
      INTEGER DETECT(N) ! Index number of WFPC2 detector (for POLQ only)

      REAL THETA(N) ! The position angle of the polarizer
      REAL REFANG(N) ! Reference angle to subtract from THETA
      REAL CORANG(N) ! Additive angle correction to instrument rotator angle
      REAL PA_V3 ! Input value of telescope rotation (HST V3 position angle)
      REAL ORIENT ! Output required instrument position angle

      CHARACTER*16 INSTNAME ! Name of instrument
      CHARACTER*16 POLNAME ! Name of polarizer filter
      CHARACTER*16 FILTNAME ! Name of colour filter
      CHARACTER*16 APNAME(N) ! Output array of names of polarizer filters

      LOGICAL LMATCH ! Flag to indicate polarizer filters values set (TRUE=set)
C
      LMATCH=.FALSE.

      IF (INSTNAME(:3).EQ.'ACS') THEN
        CALL GET_ACS(POLNAME,N,THETA,REFANG,CORANG,APNAME,LMATCH)
        ORIENT=PA_V3 + 0.0
      ENDIF

      IF (INSTNAME(:3).EQ.'FOC') THEN
        CALL GET_FOC(N,THETA,REFANG,CORANG,APNAME,LMATCH)
        ORIENT=PA_V3 - 213.9
      ENDIF

      IF (INSTNAME(:6).EQ.'NICMOS') THEN
        CALL GET_NICMOS(POLNAME,N,REFANG,CORANG,APNAME,LMATCH)
        ORIENT=PA_V3 - 44.6
      ENDIF

      IF (INSTNAME(:5).EQ.'WFPC2') THEN
        CALL GET_WFPC2(N,THETA,REFANG,CORANG,APNAME,DETECT,LMATCH)
        ORIENT=PA_V3
      ENDIF

      IF (INSTNAME(:7).EQ.'SPECIAL') THEN
        CALL GET_SPECIAL(N,THETA,REFANG,CORANG,FILTNAME,APNAME,LMATCH)
        ORIENT=PA_V3
      ENDIF

99    END

      SUBROUTINE GET_ACS(POLNAME,N,THETA,REFANG,CORANG,APNAME,LMATCH)
C
C     This subroutine sets the arrays of polarizer position angle
C     (THETA), reference angle to subtract from THETA (REFANG),
C     additive angle correction to instrument rotator angle
C     (CORANG) and array of polarizer names for the polarizer 
C     filter (APNAME) given by POLNAME for ACS
C     If the polarizer filter name is not matched then LMATCH is
C     FALSE
C
      IMPLICIT NONE
      INTEGER N ! No. of elements in output arrays 

      REAL THETA(N) ! The position angle of the polarizer
      REAL REFANG(N) ! Reference angle to subtract from THETA
      REAL CORANG(N) ! Additive angle correction to instrument rotator angle

      CHARACTER*16 POLNAME ! Name of polarizer filter
      CHARACTER*16 APNAME(N) ! Output array of names of polarizer filters

      LOGICAL LMATCH ! Flag to indicate polarizer filters values set (TRUE=set)
C
C     Local variables
C
      INTEGER ILEN
      
      LMATCH=.FALSE.
      ILEN=INDEX(POLNAME,' ') -1
      IF (POLNAME(:3).EQ.'POL') THEN
        IF (POLNAME(ILEN:ILEN).EQ.'V') THEN
          THETA(1)=0.0 
          THETA(2)=60.0
          THETA(3)=120.0
          REFANG(1)=135.0 ! Need to be checked ??
          REFANG(2)=135.0
          REFANG(3)=135.0
          CORANG(1)=90.0 ! Need to be checked ??
          CORANG(2)=90.0
          CORANG(3)=90.0
          APNAME(1)='POL0V'
          APNAME(2)='POL60V'
          APNAME(3)='POL120V'
          LMATCH=.TRUE.
        ENDIF
        IF (POLNAME(ILEN-1:ILEN).EQ.'UV') THEN
          THETA(1)=0.0 
          THETA(2)=60.0
          THETA(3)=120.0
          REFANG(1)=135.0 ! Need to be checked ??
          REFANG(2)=135.0
          REFANG(3)=135.0
          CORANG(1)=90.0 ! Need to be checked ??
          CORANG(2)=90.0
          CORANG(3)=90.0
          APNAME(1)='POL0UV'
          APNAME(2)='POL60UV'
          APNAME(3)='POL120UV'
          LMATCH=.TRUE.
        ENDIF
      ENDIF

99    END

      SUBROUTINE GET_FOC(N,THETA,REFANG,CORANG,APNAME,LMATCH)
C
C     This subroutine sets the arrays of polarizer position angle
C     (THETA), reference angle to subtract from THETA (REFANG),
C     additive angle correction to instrument rotator angle
C     (CORANG) and the array of polarizer names (APNAME) for FOC
C
      IMPLICIT NONE
      INTEGER N ! No. of elements in output arrays 

      REAL THETA(N) ! The position angle of the polarizer
      REAL REFANG(N) ! Reference angle to subtract from THETA
      REAL CORANG(N) ! Additive angle correction to instrument rotator angle
      
      CHARACTER*16 APNAME(N) ! Output array of names of polarizer filters

      LOGICAL LMATCH ! Flag to indicate polarizer filters values set (TRUE=set)

      THETA(1)=0.0
      THETA(2)=60.0
      THETA(3)=120.0
      REFANG(1)=0.0
      REFANG(2)=0.0
      REFANG(3)=0.0
      CORANG(1)=90.0
      CORANG(2)=90.0
      CORANG(3)=90.0
      APNAME(1)='POL0'
      APNAME(2)='POL60'
      APNAME(3)='POL120'
      LMATCH=.TRUE.

99    END

      SUBROUTINE GET_NICMOS(POLNAME,N,REFANG,CORANG,APNAME,LMATCH)
C
C     This subroutine sets the arrays of reference angle to 
C     subtract from THETA (REFANG) additive angle correction 
C     to instrument rotator angle (CORANG) and the array of 
C     polarizer names (APNAME) for NICMOS. The polarizer angles 
C     depend on which polarizer (short or long wavelength) is 
C     required.
C     If the polarizer filter name is not matched then LMATCH is
C     FALSE
C
      IMPLICIT NONE
      INTEGER N ! No. of elements in output arrays 

      REAL REFANG(N) ! Reference angle to subtract from THETA
      REAL CORANG(N) ! Additive angle correction to instrument rotator angle
      
      CHARACTER*16 POLNAME ! Name of polarizer filter
      CHARACTER*16 APNAME(N) ! Output array of names of polarizer filters

      LOGICAL LMATCH ! Flag to indicate polarizer filters values set (TRUE=set)
C
C     Local variables
C
      INTEGER ILEN
      CHARACTER*1 PCHAR

      LMATCH=.FALSE.
      ILEN=INDEX(POLNAME,' ') -1
      PCHAR=POLNAME(ILEN:ILEN)
      IF (PCHAR.EQ.'S') THEN ! Short wavelength polarizer
        REFANG(1)=0.0
        REFANG(2)=0.0
        REFANG(3)=0.0
        CORANG(1)=0.0
        CORANG(2)=0.0
        CORANG(3)=0.0
        APNAME(1)='POL0S'
        APNAME(2)='POL120S'
        APNAME(3)='POL240S'
        LMATCH=.TRUE.
      ENDIF

      IF (PCHAR.EQ.'L') THEN ! Long wavelength polarizer
        REFANG(1)=0.0
        REFANG(2)=0.0
        REFANG(3)=0.0
        CORANG(1)=0.0
        CORANG(2)=0.0
        CORANG(3)=0.0
        APNAME(1)='POL0L'
        APNAME(2)='POL120L'
        APNAME(3)='POL240L'
        LMATCH=.TRUE.
      ENDIF

99    END

      SUBROUTINE GET_WFPC2(N,THETA,REFANG,CORANG,APNAME,DETECT,LMATCH)
C
C     This subroutine sets the arrays of polarizer position angle
C     (THETA), reference angle to subtract from THETA (REFANG),
C     additive angle correction to instrument rotator angle
C     (CORANG) and the array of polarizer names (APNAME) for WFPC2
C
      IMPLICIT NONE
      INTEGER N ! No. of elements in output arrays 
      INTEGER DETECT(N) ! Index number of WFPC2 detector (for POLQ only)

      REAL THETA(N) ! The position angle of the polarizer
      REAL REFANG(N) ! Reference angle to subtract from THETA
      REAL CORANG(N) ! Additive angle correction to instrument rotator angle
      
      CHARACTER*16 APNAME(N) ! Output array of names of polarizer filters

      LOGICAL LMATCH ! Flag to indicate polarizer filters values set (TRUE=set)

      THETA(1)=0.0 ! POLQ WF2
      THETA(2)=45.0 ! POLQ WF3
      THETA(3)=90.0 ! POLQ WF4 
      THETA(4)=135.0 ! POLQ PC1
      THETA(5)=102.0 ! POLQN33
      THETA(6)=117.0 ! POLQN18
      THETA(7)=15.0 ! POLQP15
      REFANG(1)=135.0
      REFANG(2)=135.0
      REFANG(3)=135.0
      REFANG(4)=135.0
      REFANG(5)=135.0
      REFANG(6)=135.0
      REFANG(7)=135.0
      CORANG(1)=90.0
      CORANG(2)=90.0
      CORANG(3)=90.0
      CORANG(4)=90.0
      CORANG(5)=90.0
      CORANG(6)=90.0
      CORANG(7)=90.0
      APNAME(1)='POLQ'
      APNAME(2)='POLQ'
      APNAME(3)='POLQ'
      APNAME(4)='POLQ'
      APNAME(5)='POLQN33'
      APNAME(6)='POLQN18'
      APNAME(7)='POLQP15'
      DETECT(1)=2
      DETECT(2)=3
      DETECT(3)=4
      DETECT(4)=1
      DETECT(5)=0
      DETECT(6)=0
      DETECT(7)=0
      LMATCH=.TRUE.

99    END

      SUBROUTINE GET_SPECIAL(N,THETA,REFANG,CORANG,FILTNAM,APNAME,
     :                       LMATCH)
C
C     This subroutine sets the arrays of polarizer position angle
C     (THETA), reference angle to subtract from THETA (REFANG),
C     additive angle correction to instrument rotator angle
C     (CORANG) and the array of polarizer names (APNAME) for FOC
C
      IMPLICIT NONE
      INTEGER N ! No. of elements in output arrays 

      REAL THETA(N) ! The position angle of the polarizer
      REAL REFANG(N) ! Reference angle to subtract from THETA
      REAL CORANG(N) ! Additive angle correction to instrument rotator angle
      
      CHARACTER*16 FILTNAM ! Name of filter
      CHARACTER*16 APNAME(N) ! Output array of names of polarizer filters

      LOGICAL LMATCH ! Flag to indicate polarizer filters values set (TRUE=set)
C
C     Local variables
C
      INTEGER I
C
C     THETA is not set since polarizer angles taken from
C     instrument table file. Only set REFANG and CORANG (to 
C     zero). Set APNAME(I) to FILTNAM
C
      DO I=1,N,1
        REFANG(I)=0.0
        CORANG(I)=0.0
        APNAME(I)=FILTNAM
      ENDDO
      LMATCH=.TRUE.

99    END

      SUBROUTINE READ_TABPOL(TBDSCR,INSTNAME,POLNAME,FILTNAME,APNAME,
     :                        NR,AFNAME,APANG,ATPAR,ATPER,ARS,ARP,
     :                        ADELPH,AINSPO,AINSPA,LFLAG,N,THETA,
     :                        TPAR,TPER,RS,RP,DELPH,INSPO,
     :                        INSPA,LSTAT)
C
C     This subroutine reads columns of the table file, whose descriptor 
C     is TBDSCR, into the following arrays:
C        column FILTER to array AFNAME
C        column POLANG to array APANG
C        column TRANSPAR to array ATPAR
C        column TRANSPER to array ATPER
C        column REFLECTRS to array ARS
C        column REFLECTRP to array ARP
C        column RETARDPHI to array ADELPH
C        column INSTPOL to array AINSPO
C        column INSTPA to array AINSPA
C     A match is then sought between FILTNAME and the list of 
C     filter names AFNAME. If a match occurs the corresponding 
C     values are read into the output arrays TPAR, TPER, RS, RP, 
C     DELPH, INSPO and INSPA respectively. All APANG values
C     are read into THETA for SPECIAL only.
C     If a column is not found in the table (or filter name not 
C     matched for SPECIAL), then LSTAT is set FALSE
C
      IMPLICIT NONE
      INTEGER TBDSCR ! Descriptor of table to be read
      INTEGER NR ! Number of rows in the table to be read
      INTEGER N ! First dimension of output arrays

      REAL APANG(NR) ! Array of table values for polarizer angle
      REAL ATPAR(NR) ! Array of table values for polarizer parallel transmission
      REAL ATPER(NR) ! Array of table values for polarizer perpendicular transmission
      REAL ARS(NR) ! Array of table values of mirror reflectance for s-wave (E vector)
      REAL ARP(NR) ! Array of table values of mirror reflectance for p-wave (M vector)
      REAL ADELPH(NR) ! Array of table values of mirror retardance 
      REAL AINSPO(NR) ! Array of table values of instrumental polarization
      REAL AINSPA(NR) ! Array of table values of position angle of instrumental polarization

      REAL THETA(N) ! The position angle of the polarizer 
      REAL TPAR(N) ! The parallel component of polarizer transmission
      REAL TPER(N) ! The perpendicular component of polarizer transmission
      REAL RS(N) ! Reflectance of s wave
      REAL RP(N) ! Reflectance of p wave
      REAL DELPH(N) ! Retardance of s wave relative to p wave
      REAL INSPO(N) ! Instrumental polarization (%)
      REAL INSPA(N) ! Position angle of instrumental polarization (deg.)

      CHARACTER*16 INSTNAME ! Name of instrument
      CHARACTER*16 POLNAME ! Name of polarizer filter
      CHARACTER*16 FILTNAME ! Name of colour filter
      CHARACTER*16 APNAME(N) ! Array of names of polarizer filters
      CHARACTER*16 AFNAME(NR) ! Array of table values of name of colour/polarizer filter

      LOGICAL LFLAG(NR) ! Array of flags for status in reading table column values
      LOGICAL LSTAT ! True if all expected columns with correct names found in table 
C
C     Local variables
C
      INTEGER I,J
      INTEGER ILEN
      INTEGER STAT
      INTEGER STATO
      INTEGER COLID1
      INTEGER COLID2
      INTEGER COLID3
      INTEGER COLID4
      INTEGER COLID5
      INTEGER COLID6
      INTEGER COLID7
      INTEGER COLID8
      INTEGER COLID9
      REAL MTHETA
      CHARACTER*16 BNAME
      CHARACTER*16 POLNAM
      CHARACTER*16 FILNAM
      CHARACTER*80 TEXT

      LSTAT=.TRUE.
C
C     Match the column names one by one and read the values into 
C     appropriate arrays
C
C     Get column identifier for column FILTER
C   
      CALL UTCFND(TBDSCR,'FILTER',1,COLID1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,21)
21      FORMAT(' Failed to find column named  FILTER ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array AFNAME
C
      CALL UTCGTT(TBDSCR,COLID1,1,NR,AFNAME,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,22)
22      FORMAT(' Failed to read values from column named  FILTER ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column POLANG
C   
      CALL UTCFND(TBDSCR,'POLANG',1,COLID2,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,31)
31      FORMAT(' Failed to find column named  POLANG ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array APANG
C
      CALL UTCGTR(TBDSCR,COLID2,1,NR,APANG,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,32)
32      FORMAT(' Failed to read values from column named  POLANG ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column TRANSPAR
C   
      CALL UTCFND(TBDSCR,'TRANSPAR',1,COLID3,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,41)
41      FORMAT(' Failed to find column named  TRANSPAR ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array ATPAR
C
      CALL UTCGTR(TBDSCR,COLID3,1,NR,ATPAR,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,42)
42      FORMAT(' Failed to read values from column named  TRANSPAR ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column TRANSPER
C   
      CALL UTCFND(TBDSCR,'TRANSPER',1,COLID4,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,51)
51      FORMAT(' Failed to find column named  TRANSPER ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array ATPER
C
      CALL UTCGTR(TBDSCR,COLID4,1,NR,ATPER,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,52)
52      FORMAT(' Failed to read values from column named  TRANSPER ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column REFLECTRS
C   
      CALL UTCFND(TBDSCR,'REFLECTRS',1,COLID5,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,61)
61      FORMAT(' Failed to find column named  REFLECTRS ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array ARS
C
      CALL UTCGTR(TBDSCR,COLID5,1,NR,ARS,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,72)
72      FORMAT(' Failed to read values from column named  REFLECTRS ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column REFLECTRP
C   
      CALL UTCFND(TBDSCR,'REFLECTRP',1,COLID6,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,81)
81      FORMAT(' Failed to find column named  REFLECTRP ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array ARP
C
      CALL UTCGTR(TBDSCR,COLID6,1,NR,ARP,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,92)
92      FORMAT(' Failed to read values from column named  REFLECTRP ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column RETARDPHI
C   
      CALL UTCFND(TBDSCR,'RETARDPHI',1,COLID7,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,101)
101     FORMAT(' Failed to find column named  RETARDPHI ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array ADELPH
C
      CALL UTCGTR(TBDSCR,COLID7,1,NR,ADELPH,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,112)
112     FORMAT(' Failed to read values from column named  RETARDPHI ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column INSTPOL
C   
      CALL UTCFND(TBDSCR,'INSTPOL',1,COLID8,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,121)
121     FORMAT(' Failed to find column named  INSTPOL ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array AINSPO
C
      CALL UTCGTR(TBDSCR,COLID8,1,NR,AINSPO,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,132)
132     FORMAT(' Failed to read values from column named  INSTPOL ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column INSTPA
C   
      CALL UTCFND(TBDSCR,'INSTPA',1,COLID9,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,141)
141     FORMAT(' Failed to find column named  INSTPA ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array AINSPO
C
      CALL UTCGTR(TBDSCR,COLID9,1,NR,AINSPA,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,152)
152     FORMAT(' Failed to read values from column named  INSTPA ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     The matching of the filter/polarizer name depends on instrument
C 
      LSTAT=.FALSE.
      IF (INSTNAME(:3).EQ.'ACS') THEN
        DO J=1,N,1
C
C         Attempt to match the filter name FILTNAM with the value
C         in AFNAME and the polarizer name POLENAME  with the
C         
          FILNAM=FILTNAME
          POLNAM=APNAME(J)
          ILEN=INDEX(FILNAM,'  ') -1 
          DO I=1,NR,1
            BNAME=AFNAME(I)
            IF (FILNAM(:ILEN).EQ.BNAME(:ILEN)) THEN
              TPAR(J)=ATPAR(I)
              TPER(J)=ATPER(I)
              RS(J)=ARS(I)
              RP(J)=ARP(I)
              DELPH(J)=ADELPH(I)
              INSPO(J)=AINSPO(I)
              INSPA(J)=AINSPA(I)
              LSTAT=.TRUE.
            ENDIF        
          ENDDO
        ENDDO
      ENDIF
  
      IF (INSTNAME(:3).EQ.'FOC') THEN
        DO J=1,N,1
C
C         Attempt to match the filter name FILTNAM with the value
C         in AFNAME and the polarizer angle THETA(J) with the
C         angle in APANG
C     
          FILNAM=FILTNAME
          MTHETA=THETA(J)
          ILEN=INDEX(FILNAM,'  ') -1 
          DO I=1,NR,1
            BNAME=AFNAME(I)
            IF (FILNAM(:ILEN).EQ.BNAME(:ILEN).AND.
     :          MTHETA.EQ.APANG(I)) THEN
              TPAR(J)=ATPAR(I)
              TPER(J)=ATPER(I)
              RS(J)=ARS(I)
              RP(J)=ARP(I)
              DELPH(J)=ADELPH(I)
              INSPO(J)=AINSPO(I)
              INSPA(J)=AINSPA(I)
              LSTAT=.TRUE.
            ENDIF
          ENDDO
        ENDDO
      ENDIF

      IF (INSTNAME(:6).EQ.'NICMOS') THEN
        DO J=1,N,1
C
C         Attempt to match the polarizer name POLNAME with the value
C         in AFNAME 
C     
          FILNAM=APNAME(J)
          ILEN=INDEX(FILNAM,'  ') -1
          DO I=1,NR,1
            BNAME=AFNAME(I)
            IF (FILNAM(:ILEN).EQ.BNAME(:ILEN)) THEN
              THETA(J)=APANG(I)
              TPAR(J)=ATPAR(I)
              TPER(J)=ATPER(I)
              RS(J)=ARS(I)
              RP(J)=ARP(I)
              DELPH(J)=ADELPH(I)
              INSPO(J)=AINSPO(I)
              INSPA(J)=AINSPA(I)
              LSTAT=.TRUE.
            ENDIF
          ENDDO
        ENDDO
      ENDIF

      IF (INSTNAME(:5).EQ.'WFPC2') THEN
        DO J=1,N,1
C
C         Attempt to match the filter name FILTNAM with the value
C         in AFNAME
C     
          FILNAM=FILTNAME
          ILEN=INDEX(FILNAM,'  ') -1
          DO I=1,NR,1
            BNAME=AFNAME(I)
            IF (FILNAM(:ILEN).EQ.BNAME(:ILEN)) THEN
              TPAR(J)=ATPAR(I)
              TPER(J)=ATPER(I)
              RS(J)=ARS(I)
              RP(J)=ARP(I)
              DELPH(J)=ADELPH(I)
              INSPO(J)=AINSPO(I)
              INSPA(J)=AINSPA(I)
              LSTAT=.TRUE.
             ENDIF
          ENDDO
        ENDDO
      ENDIF

      IF (INSTNAME(:7).EQ.'SPECIAL') THEN
C
C       Check that first and last filter names are identical.
C       If not set status flag false and end. If names same
C       then read all table array entries to output arrays
C
        IF (AFNAME(1).NE.AFNAME(NR)) THEN
          LSTAT=.FALSE.
          GO TO 999
        ELSE
          BNAME=AFNAME(1)
        ENDIF
        FILNAM=FILTNAME
        ILEN=INDEX(FILNAM,'  ') -1 
        IF (FILNAM(:ILEN).EQ.BNAME(:ILEN)) THEN
C
C         Copy all the table array values to the output
C         arrays 
C
          DO I=1,NR,1
            THETA(I)=APANG(I)
            TPAR(I)=ATPAR(I)
            TPER(I)=ATPER(I)
            RS(I)=ARS(I)
            RP(I)=ARP(I)
            DELPH(I)=ADELPH(I)
            INSPO(I)=AINSPO(I)
            INSPA(I)=AINSPA(I)
            LSTAT=.TRUE.
          ENDDO
        ELSE
          LSTAT=.FALSE.
        ENDIF
      ENDIF

999   END

      SUBROUTINE SYSEFFIC(NV,TPAR,TPER,THETA,REFANG,RS,RP,DELPH,
     :                    ORIENT,CORANG,M,N,MATPOL,MATROP,MATMIR,
     :                    MATROT,WORK,MATSYS,EFFI,EFFQ,EFFU,EFFV)
C
C     This subroutine determines the system efficiency for Stokes
C     I (EFFI), for Stokes Q (EFFQ) and for Stokes U (EFFU) by
C     determining the system matrix of effective instrumental 
C     (de)polarization (see e.g. Biretta & McMaster, 1997, WFPC2
C     ISR 97-11)
C
      IMPLICIT NONE
      INTEGER NV ! Number of polarizer efficiencies to calculate
      INTEGER M ! No. of rows of matrixes MAT...
      INTEGER N ! No. of columns of matrices MAT...

      REAL TPAR(NV) ! The parallel component of polarizer transmission
      REAL TPER(NV) ! The perpendicular component of polarizer transmission
      REAL THETA(NV) ! The position angle of the polarizer
      REAL REFANG(NV) ! Reference angle to subtract from THETA
      REAL RS(NV) ! Reflectance of s wave
      REAL RP(NV) ! Reflectance of p wave
      REAL DELPH(NV) ! Retardance of s wave relative to p wave
      REAL ORIENT ! The position angle of the telescope/instrument
      REAL CORANG(NV) ! Additive position angle correction

      REAL MATPOL(M,N) ! Mueller matrix of polarizer
      REAL MATROP(M,N) ! Mueller matrix of polarizer rotation
      REAL MATMIR(M,N) ! Mueller matrix of mirror polarization
      REAL MATROT(M,N) ! Mueller matrix of PA rotation 
      REAL WORK(M,N) ! Work array for matrix multiplications
      REAL MATSYS(M,N) ! Mueller matrix of system polarization
  
      REAL EFFI(NV) ! Output efficiency for Stokes I detection for NV polarizer positions
      REAL EFFQ(NV) ! Output efficiency for Stokes Q detection for NV polarizer positions
      REAL EFFU(NV) ! Output efficiency for Stokes U detection for NV polarizer positions
      REAL EFFV(NV) ! Output efficiency for Stokes V detection for NV polarizer positions
C
C     Local variables
C
      INTEGER I
      REAL DELPHI

      DO I=1,NV,1
        IF (DELPH(I).NE.0.0) THEN
          DELPHI=180.0-DELPH(I) ! Correct Delta Phi for WFPC2
        ELSE
          DELPHI=0.0
        ENDIF
        CALL SYSMATRIX(THETA(I),REFANG(I),TPAR(I),TPER(I),RS(I),
     :                 RP(I),DELPHI,ORIENT,CORANG(I),M,N,
     :                 MATPOL,MATROP,MATMIR,MATROT,WORK,MATSYS)
        EFFI(I)=MATSYS(1,1)
        EFFQ(I)=MATSYS(1,2)
        EFFU(I)=MATSYS(1,3)
        EFFV(I)=MATSYS(1,4)
      ENDDO

99    END

      SUBROUTINE OUTOCUB(M1,M2,M3,CUB,N1,N2,ARRY,KS)
C
C     This subroutine copies the KS'th plane of the
C     3-D array CUB to the 2-D array ARRY
C
      IMPLICIT NONE
      INTEGER M1
      INTEGER M2
      INTEGER M3
      INTEGER N1
      INTEGER N2
      INTEGER KS

      REAL CUB(M1,M2,M3)
      REAL ARRY(N1,N2)
C
C     Local variables
C
      INTEGER I,J
C
C     Initialize array ARRY
C
      DO J=1,N2,1
        DO I=1,N1,1
          ARRY(I,J)=0.0
        ENDDO
      ENDDO
C
C     Copy the KS'th section of the 3-D array CUB to the
C     2-D array ARRY
C
      DO J=1,N2,1
        DO I=1,N1,1
          ARRY(I,J)=CUB(I,J,KS)
        ENDDO
      ENDDO

99    END      

      SUBROUTINE NAMMAK(ROOT,N,ANG,NS,NAM1,NAM2)
C
C     This subroutine creates two output names
C     from the following concatenations:
C     NAM1: ROOT + int + INTEGER(ANG)
C     NAM2: ROOT + err + INTEGER(ANG)
C     where ANG is the NS'th element of array ANANG
C
      IMPLICIT NONE
      INTEGER N ! Dimension of ANANG
      INTEGER NS ! Element of ANANG to convert
        
      REAL ANG(N)
 
      CHARACTER*24 ROOT ! Root name for output files
      CHARACTER*32 NAM1 ! First concatenated output name 
      CHARACTER*32 NAM2 ! Second concatenated output name
C
C     Local variables
C
      INTEGER ILEN
      INTEGER NUM
      INTEGER NUM1
      INTEGER NUM2
      INTEGER NUM3

      CHARACTER*3 CNUM
      CHARACTER*1 CNUM1
      CHARACTER*1 CNUM2
      CHARACTER*1 CNUM3

      ILEN=INDEX(ROOT,'   ') -1
C
C     Get the integer value of the angle
C
      NUM=INT(ANG(NS))
C
C     Convert NUM to a 3 digit character variable       
C
      NUM1=NUM/100
      CALL CEVAL(NUM1,CNUM1)
      NUM2=(NUM - NUM1*100)/10
      CALL CEVAL(NUM2,CNUM2)        
      NUM3=NUM - NUM1*100 - NUM2*10
      CALL CEVAL(NUM3,CNUM3)
C
C     Concatenate the three 1 digit character variables
C
      CNUM=CNUM1//CNUM2//CNUM3
C
C     Form the output concatenated names from 
C     ROOT + INT / ERR and number
C
      NAM1=ROOT(:ILEN)//'int'//CNUM
      NAM2=ROOT(:ILEN)//'err'//CNUM

99    END

      SUBROUTINE CEVAL(N,CN)
C
C     This subroutine converts the value of
C     integer N into character CN
C
      INTEGER N
      CHARACTER*1 CN

      IF (N.EQ.0) THEN
        CN='0'
      ENDIF
      IF (N.EQ.1) THEN
        CN='1'
      ENDIF
      IF (N.EQ.2) THEN
        CN='2'
      ENDIF
      IF (N.EQ.3) THEN
        CN='3'
      ENDIF
      IF (N.EQ.4) THEN
        CN='4'
      ENDIF
      IF (N.EQ.5) THEN
        CN='5'
      ENDIF
      IF (N.EQ.6) THEN
        CN='6'
      ENDIF
      IF (N.EQ.7) THEN
        CN='7'
      ENDIF
      IF (N.EQ.8) THEN
        CN='8'
      ENDIF
      IF (N.EQ.9) THEN
        CN='9'
      ENDIF
      
99    END

      SUBROUTINE HEADUPD(ONAM,IMDSCR,INSTNAME,APOLFNAM,FILTNAME,
     :                   N,NI,THETA,DETECT,ORIENT,LUSTAT)
C
C     This subroutine writes into the header keywords 
C     the instrument - polarizer specifics as follows:
C       keyword INSTRUME writes INSTNAME
C       keyword FILTER writes APNAME (NICMOS and SPECIAL only)
C       keyword FILTNAM1 writes APNAME (FOC only)
C       keyword FILTNAM2 writes FILTNAME (FOC only)
C       keyword FILTNAM1 writes FILTNAME (WFPC2 only)
C       keyword FILTNAM2 writes APNAME (WFPC2 only)
C       keyword POLANG write THETA (SPECIAL only)
C       keyword DETECTOR write DETECT (WFPC2 only)
C       keyword PA_V3 writes PA_V3 (WFPC2 only)
C       keyword ORIENTAT writes PA_V3 + correction (FOC and NICMOS)
C     If an error occurs in updating a header LUSTAT is
C     set FALSE
C
      IMPLICIT NONE
      INTEGER IMDSCR ! Descriptor of image to be updated
      INTEGER N ! No. of elements in array DETECT
      INTEGER NI ! Index no. of element of DETECT to read
      INTEGER DETECT(N) ! No. of WFPC2 detector
      
      REAL THETA(N) ! Position angle of polarizer rotation
      REAL ORIENT ! Position angle of instrument rotator

      CHARACTER*32 ONAM ! Name of output file being written
      CHARACTER*16 INSTNAME ! Name of HST instrument
      CHARACTER*16 APOLFNAM(N) ! Array of names of instrument polarizer 
      CHARACTER*16 FILTNAME ! Name of polarizer/colour filter
      LOGICAL LUSTAT ! Status flag for return (=FALSE if error updating header)
C
C     Local variables
C
      INTEGER HTYPE
      INTEGER STAT
      INTEGER STAT1
      INTEGER STAT2

      REAL NORIENT
      
      CHARACTER*80 OUTEXT
      CHARACTER*72 COMMENT

      LUSTAT=.TRUE.
      HTYPE=1
C
C     Try to update INSTRUME header. If it doesn't exist create
C     this header keyword and write value of INSTNAME to it
C
      CALL UHDPST(IMDSCR,'INSTRUME',INSTNAME,STAT1)
      IF (STAT1.GE.40) THEN
        COMMENT=' instrument in use '
        CALL UHDAST(IMDSCR,'INSTRUME',INSTNAME,COMMENT,HTYPE,STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,11) ONAM 
11        FORMAT(' Error updating image header ',A16)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          LUSTAT=.FALSE.
          GO TO 99
        ENDIF
      ENDIF
C
C     For ACS. Try to update FILTNAM1 and FILTNAM2 header. If 
C     they don't exist create these header keywords and write 
C     values of FILTNAM1 and FILTNAM2 to them.
C     Try to update the value of ORIENTAT. If it don't exist 
C     create the header keyword and write value of ORIENT 
C     to it
C
      IF (INSTNAME(:3).EQ.'ACS') THEN
        CALL UHDPST(IMDSCR,'FILTNAM1',APOLFNAM(NI),STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' filter element name for wheel 1 '
          CALL UHDAST(IMDSCR,'FILTNAM1',APOLFNAM(NI),COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
        CALL UHDPST(IMDSCR,'FILTNAM2',FILTNAME,STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' filter element name for wheel 2 '
          CALL UHDAST(IMDSCR,'FILTNAM2',FILTNAME,COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
        IF (ORIENT.LT.0.0) THEN
          ORIENT=360.0 + ORIENT
        ENDIF
        CALL UHDPSR(IMDSCR,'ORIENTAT',ORIENT,STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' position angle of image y axis (deg. e of n)'
          CALL UHDASR(IMDSCR,'ORIENTAT',ORIENT,COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
      ENDIF
C
C     For FOC. Try to update FILTNAM1 and FILTNAM2 header. If 
C     they don't exist create these header keywords and write 
C     values of FILTNAM1 and FILTNAM2 to them.
C     Try to uppdate the value of ORIENTAT. If it don't exist 
C     create the header keyword and write value of ORIENT 
C     to it
C
      IF (INSTNAME(:3).EQ.'FOC') THEN
        CALL UHDPST(IMDSCR,'FILTNAM1',APOLFNAM(NI),STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' filter element name for wheel 1 '
          CALL UHDAST(IMDSCR,'FILTNAM1',APOLFNAM(NI),COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
        CALL UHDPST(IMDSCR,'FILTNAM2',FILTNAME,STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' filter element name for wheel 2 '
          CALL UHDAST(IMDSCR,'FILTNAM2',FILTNAME,COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
        IF (ORIENT.LT.0.0) THEN
          ORIENT=360.0 + ORIENT
        ENDIF
        CALL UHDPSR(IMDSCR,'ORIENTAT',ORIENT,STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' position angle of image y axis (deg. e of n)'
          CALL UHDASR(IMDSCR,'ORIENTAT',ORIENT,COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
      ENDIF
C
C     For NICMOS. Try to update FILTER header. If it doesn't exist 
C     create this header keyword and write value of FILTER to it
C     Try to uppdate the value of ORIENTAT. If it don't exist 
C     create the header keyword and write value of ORIENT to it
C
      IF (INSTNAME(:6).EQ.'NICMOS') THEN
        CALL UHDPST(IMDSCR,'FILTER',APOLFNAM(NI),STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' filter wheel element in beam during observation '
          CALL UHDAST(IMDSCR,'FILTER',APOLFNAM(NI),COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
        IF (ORIENT.LT.0.0) THEN
          ORIENT=360.0 + ORIENT
        ENDIF
        CALL UHDPSR(IMDSCR,'ORIENTAT',ORIENT,STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' position angle of image y axis (deg. e of n)'
          CALL UHDASR(IMDSCR,'ORIENTAT',ORIENT,COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
      ENDIF
C
C     For WFPC2. Try to update FILTNAM1 and FILTNAM2 header. If 
C     they don't exist create these header keywords and write 
C     values of FILTNAM1 and FILTNAM2 to them.
C     Try to update the values of PA_V3 and ORIENTAT. If 
C     they don't exist create these header keywords and write 
C     values of ORIENT (actually equal to PA_V3 for WFPC2) tp
C     PA_V3 and ORIENT- 135.3 to ORIENTAT
C
      IF (INSTNAME(:5).EQ.'WFPC2') THEN
        CALL UHDPST(IMDSCR,'FILTNAM1',FILTNAME,STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' first filter name '
          CALL UHDAST(IMDSCR,'FILTNAM1',FILTNAME,COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
        CALL UHDPST(IMDSCR,'FILTNAM2',APOLFNAM(NI),STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' second filter name '
          CALL UHDAST(IMDSCR,'FILTNAM2',APOLFNAM(NI),COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
        CALL UHDPSR(IMDSCR,'PA_V3',ORIENT,STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' position angle of v3 axis of HST '
          CALL UHDASR(IMDSCR,'PA_V3',ORIENT,COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
        NORIENT=ORIENT - 135.3
        IF (NORIENT.LT.0.0) THEN
          NORIENT=360.0 + NORIENT
        ENDIF
        CALL UHDPSR(IMDSCR,'ORIENTAT',NORIENT,STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' position angle of image y axis (deg. e of n)'
          CALL UHDASR(IMDSCR,'ORIENTAT',NORIENT,COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
C
C       Try to update detector keyword
C
        CALL UHDPSI(IMDSCR,'DETECTOR',DETECT(NI),STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' detector number: 1=PC, 2=WF2, 3=WF3, 4=WF4 '
          CALL UHDASI(IMDSCR,'DETECTOR',DETECT(NI),COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
      ENDIF
C
C     For SPECIAL. Try to update FILTER header. If it doesn't exist 
C     create this header keyword and write value of FILTER to it
C
      IF (INSTNAME(:7).EQ.'SPECIAL') THEN
        CALL UHDPST(IMDSCR,'FILTER',APOLFNAM(NI),STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' filter name '
          CALL UHDAST(IMDSCR,'FILTER',APOLFNAM(NI),COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
C
C       Try to update POLANG keyword for value of polarizer
C       position angle. If it doesn't exist create it and write
C       value of THETA to it
C
        CALL UHDPSR(IMDSCR,'POLANG',THETA(NI),STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' polarizer position angle '
          CALL UHDASR(IMDSCR,'POLANG',THETA(NI),COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
        CALL UHDPSR(IMDSCR,'ORIENTAT',ORIENT,STAT1)
        IF (STAT1.GE.40) THEN
          COMMENT=' position angle of image y axis (deg. e of n)'
          CALL UHDASR(IMDSCR,'ORIENTAT',ORIENT,COMMENT,
     :                HTYPE,STAT2)
          IF (STAT2.NE.0) THEN
            WRITE(OUTEXT,11) ONAM 
            CALL UMSPUT(OUTEXT,3,0,STAT)
            LUSTAT=.FALSE.
            GO TO 99
          ENDIF
        ENDIF
      ENDIF

99    END

      SUBROUTINE SIMINST(N1,N2,TOTI,LPOL,POPA,CPOL,M,EFFI,
     :                   EFFQ,EFFU,EFFV,BACK,E2ADU,RON,
     :                   INSPO,INSPA,PA_V3,CORANG,SEED,
     :                   MODI,CUBINT,CUBERR)
C
C     This subroutine takes the 2-D arrays of the total
C     intensity, TOTI, the linear polarization (%, LPOL)
C     and the polarization position angle (degrees, POPA)
C     converts them to Stokes parameters and corrects for
C     the instrumental polarization. The Stokes parameters 
C     are then converted to predicted signal at the M 
C     polarizer positions using the instrument dependent 
C     efficiency terms EFFI, EFFQ and EFFU for Stokes I, 
C     Stokes Q and Stokes U respectively. 
C     An unpolarized background (BACK) is added to the 
C     predicted signal. A random number generator then 
C     determines a normally distributed value of the signal 
C     and the error taking account of the readout noise and 
C     electron to ADU conversion factor. Each 2-D image of the 
C     intensity and error corresponding to the given angle of 
C     the polarizer is written to a plane of the output arrays
C     CUBINT and CUBERR respectively.
C
      IMPLICIT NONE
      INTEGER N1 ! 1st dimension of input images
      INTEGER N2 ! 2nd dimension of input images
      INTEGER M ! No. of values of polarizer position to process
      INTEGER SEED ! Initial value for seed for random number generator
C
      REAL TOTI(N1,N2) ! Input array of total intensity
      REAL LPOL(N1,N2) ! Input array of linear polarization (%)
      REAL POPA(N1,N2) ! Input array of polarization position angle (degrees)
      REAL CPOL(N1,N2) ! Input array of circular polarization (%)
      REAL EFFI(M) ! Array of efficiencies for Stokes I for polarizer positions
      REAL EFFQ(M) ! Array of efficiencies for Stokes Q for polarizer positions
      REAL EFFU(M) ! Array of efficiencies for Stokes U for polarizer positions
      REAL EFFV(M) ! Array of efficiencies for Stokes V for polarizer positions
      REAL BACK ! Additive unpolarized background for output images
      REAL E2ADU ! Electrons/ADU conversion factor
      REAL RON ! Read-out noise in electrons
      REAL INSPO(M) ! Instrumental linear polarization (%)
      REAL INSPA(M) ! Instrumental polarization position angle (degrees)
      REAL PA_V3 ! Position angle of HST V3 axis
      REAL CORANG(M) ! Additive position angle correction
      REAL MODI(M) ! Predicted intensity at polarizer positions
      REAL CUBINT(N1,N2,M) ! Output 3-D array of intensities at polarizer angles
      REAL CUBERR(N1,N2,M) ! Output 3-D array of intensity errors at polarizer angles
C
C     Local variables
C
      INTEGER I,J,K

      REAL VAL
      REAL RAN1
      REAL INSTPO
      REAL INSTPA
      REAL NORDEV
      REAL INT1
      REAL POL1
      REAL PA1
      REAL Q1
      REAL U1
      REAL V1
      REAL Q2
      REAL U2
      REAL V2
      REAL MODE
      REAL ERRV
      REAL INTE
      REAL ERRE
      REAL SINRD
      REAL COSRD

C      
C     Initialize random number generator
C
      VAL=RAN1(-SEED)
C
C     Work through the input arrays determining the intensity 
C     at each polarizer angle corresponding to the values of
C     total intensity, linear polarization and position angle
C     and circular polarization
C
      DO J=1,N2,1
        DO I=1,N1,1
          INT1=TOTI(I,J)
          POL1=LPOL(I,J)/100.0
          IF (POL1.GT.100.0) THEN
            POL1=100.0
          ENDIF
          PA1=POPA(I,J)
          V1=CPOL(I,J)/100.0
          IF (INSPO(1).NE.0.0) THEN
            INSTPO=INSPO(1)/100.0
C
C           Correct position angle of instrumental polarization
C           to absolute value on sky
C
            INSTPA=INSPA(1) + CORANG(1)
C
C           Correct for instrumental polarization
C 
C           Form the Q and U normalised Stokes parameters for
C           this polarization and position angle 
C
            Q1=POL1*COSRD(2.*PA1)
            U1=POL1*SINRD(2.*PA1)
C
C           Correct Q1 and U1 to measured polarization using the
C           instrumental polarization
C   
            CALL INSCOR(Q1,U1,V1,INSTPO,INSTPA,Q2,U2,V2)
          ELSE
            Q2=POL1*COSRD(2.*PA1)
            U2=POL1*SINRD(2.*PA1)
            V2=V1
          ENDIF
C
C         Form the intensities at the NANG polarizer angles
C         for this total intensity, linear polarization and
C         position angle from the Cosine curve
C 
          CALL POL_SIG(INT1,Q2,U2,V2,M,EFFI,EFFQ,EFFU,EFFV,MODI)
C
C         At each of the M polarizer angles convert the
C         output intensity to electrons, find a normally
C         distributed estimate. Take the SQRT error on
C         this value if the error is greater than the 
C         read-out noise, otherwise take the read-out noise
C         as error. Write the intensity and error in ADU to 
C         the plane of the output arrays CUBINT and CUBERR 
C         respectively
C
          DO K=1,M,1
            MODE=(MODI(K)+BACK)*E2ADU ! Signal in  electrons
            IF (MODE.GE.0.0) THEN
              ERRV=SQRT(MODE + RON*RON) ! Error in electrons
            ELSE
              MODE=0.0
              ERRV=RON
            ENDIF
            INTE=NORDEV(SEED,MODE,ERRV)
            IF (INTE.GT.0.0) THEN
              ERRE=SQRT(INTE)
              IF (ERRE.LT.RON) THEN
                ERRE=RON
              ENDIF
            ELSE
              INTE=0.0
              ERRE=RON
            ENDIF
            CUBINT(I,J,K)=INTE/E2ADU ! Signal in ADU
            CUBERR(I,J,K)=ERRE/E2ADU ! Error in ADU
          ENDDO
        ENDDO
      ENDDO

99    END 

      SUBROUTINE INSCOR(ACQ,ACU,ACV,INPO,INPA,OBQ,OBU,OBV)
C
C     This subroutine amends the actual Stokes parameters 
C     ACQ, ACU and ACV for instrumental polarization INPO at 
C     position angle INPA. The instrumentally affected
C     Stokes parameters OBQ, OBU amd OBV are returned.
C     The treatment of the instrumental polarization follows
C     the Appendix of Goodrich, ApJ, 311, 882, 1986
C
      IMPLICIT NONE
      REAL ACQ ! Input Stokes Q in the absence of instrumental polarization 
      REAL ACU ! Input Stokes U in the absence of instrumental polarization
      REAL ACV ! Input Stokes V in the absence of instrumental polarization
      REAL INPO ! Instrumental polarization
      REAL INPA ! Position angle of instrumental polarization
      REAL OBQ ! Output Stokes Q with instrumental polarization included
      REAL OBU ! Output Stokes U with instrumental polarization included
      REAL OBV ! Output Stokes V with instrumental polarization included
C
C     Local variables
C
      REAL CC
      REAL SS
      REAL DD
      REAL SINRD
      REAL COSRD

      CC=COSRD(2.*INPA)
      SS=SINRD(2.*INPA)
      DD=(1.0 + INPO*CC*ACQ + INPO*SS*ACU)

      OBQ=(INPO*CC + ACQ*(1.0-(INPO*SS*SS)) + ACU*INPO*CC*SS)/DD
      OBU=(INPO*SS + ACQ*INPO*CC*SS + ACU*(1.0-(INPO*CC*CC)))/DD
      OBV=(ACV*(1.0-(INPO*SS)))/DD

99    END

      SUBROUTINE POL_SIG(INTI,Q,U,V,N,EFFI,EFFQ,EFFU,EFFV,OUTI)
C
C     This subroutine computes the output signal values
C     (MODI) for the M polarizer positions specified by
C     the efficiences for Stokes I (EFFI), Stokes Q (EFFQ)
C     Stokes U (EFFU) and Stokes V (EFFV) for Stokes total 
C     intensity INTI, Stokes Q, Stokes U and Stokes V
C
      IMPLICIT NONE
      INTEGER N ! Number of elements in arrays
   
      REAL INTI ! Input total intensity
      REAL Q ! Input normalised Stokes Q
      REAL U ! Input normalised Stokes U
      REAL V ! Input normalised Stokes V
      REAL EFFI(N) ! Array of efficiences for Stokes I for polarizer positions
      REAL EFFQ(N) ! Array of efficiences for Stokes Q for polarizer positions
      REAL EFFU(N) ! Array of efficiences for Stokes U for polarizer positions
      REAL EFFV(N) ! Array of efficiences for Stokes V for polarizer positions
      REAL OUTI(N) ! Output array of detected signal values at polarizer positions
C
C     Local variables
C
      INTEGER I

      DO I=1,N,1
        OUTI(I)=INTI*(EFFI(I) + Q*EFFQ(I) + U*EFFU(I) + V*EFFV(I))
      ENDDO

99    END


