      SUBROUTINE POLIMF
C
C     This FORTRAN program produces images of the total
C     signal, the linear polarization and the position angle
C     of linear polarization from any number (>=3) of images 
C     and corresponding error images from sets of data  
C     taken at different angles of a polarizer for any of the
C     HST instruments. Currently FOC, WFPC2, NICMOS and ACS
C     are supported. A non-HST instrument can also be handled 
C     called `SPECIAL'. Error arrays for total signal, linear 
C     polarization and polarization position angle are also 
C     produced. In addition images of the Stokes parameters 
C     Q and U and their errors files are output.
C
C     A list of files is entered for the sky-subtracted object
C     images and another list of files for the error images.
C     The header is parsed for the name of the instrument and,
C     depending on the HST instrument, the polarizer filter,
C     the colour filter, the angle of the polarizer filter 
C     and the position angle of the telescope (PA_V3) are
C     determined. Depending on the instrument a table of
C     instrumental parameters is read which determines the
C     instrumental effect on the polarization. The table
C     lists for each filter
C       the polarizer position angle
C       the transmission parallel to the poalrizer axis 
C       the transmission perpendicular to the polarizer axis, 
C       the reflectance of the electric vector
C       the reflectance of the magnetic vector
C       the phase retardance of the mirror
C     (only applicable for the case of large angle of incidence
C     on mirrors in the instrument [e.g. WFPC2])
C       the instrumental poalrization 
C       the position angle of instrumental polarization.
C     The system of equations to determine the polarization is
C     then set-up based on the instrument parameters from the
C     header and the table file. 
C     The factor by which to bin the output maps is required and 
C     the factor times the rms on the mean of signal in a binned 
C     pixel to reject a point from inclusion in the calculation of 
C     the mean. A cut-off in polarization error is specified and 
C     output binned pixels failing to meet this limit are set to 
C     zero. 
C
C     The rootname of the output files is specified and 
C     output files with the following suffixes are produced:
C        rootint   - total signal
C        rootinter - total signal error
C        rootpol   - linear polarization (in percent)
C        rootpoler - linear polarization error (in percent)
C        rootpa    - position angle of linear polarization (in degrees)
C        rootpaer  - position angle error of polarization (in degrees)
C        rootu     - Stokes parameter U (fractional)
C        rootuer   - Stokes parameter U error (fractional)
C        rootq     - Stokes parameter Q (fractional)
C        rootqer   - Stokes parameter Q error (fractional)
C
C     Written by  J. R. Walsh, ST-ECF, ESO (jwalsh@eso.org)  December 1999
C     Version 1.00
C     Modified to include ACS and extra checking for polarizer filter November 2000
C     Version 2.00
C
      IMPLICIT NONE
      INTEGER J,K
      INTEGER N

      INTEGER STAT
      INTEGER STAT1
      INTEGER STAT2
      INTEGER STAT3
      INTEGER STAT4
      INTEGER STATT

      INTEGER IMDSCRL
      INTEGER IMDSCRI(64)
      INTEGER IMDSCRM
      INTEGER IMDSCRE(64)
      INTEGER IMDSCRO
      INTEGER TBDSCR

      INTEGER NAXISI
      INTEGER DIMEN(7)
      INTEGER ODIMEN(7)
      INTEGER DTYPE

      INTEGER NOINI
      INTEGER NOINE
      INTEGER N1
      INTEGER N2
      INTEGER N3
      INTEGER N4
      INTEGER NROWS
      INTEGER NBIN
      INTEGER M1
      INTEGER M2
      INTEGER M3
      INTEGER M4
      INTEGER O1
      INTEGER O2

      INTEGER NELEM
      INTEGER HOLD
      INTEGER OBJCUB
      INTEGER ERRCUB

      INTEGER ARRANG
      INTEGER ARRTPAR
      INTEGER ARRTPER
      INTEGER ARRRS
      INTEGER ARRRP
      INTEGER ARRDELPH
      INTEGER ARRINPO
      INTEGER ARRINPA
      INTEGER LANG

      INTEGER THETA
      INTEGER IMTHETA
      INTEGER REFANG
      INTEGER TPAR
      INTEGER TPER
      INTEGER RS
      INTEGER RP
      INTEGER DELPH
      INTEGER ORIENT
      INTEGER CORANG
      INTEGER INSPO
      INTEGER INSPA
 
      INTEGER EFFI
      INTEGER EFFQ
      INTEGER EFFU

      INTEGER XYBIN

      INTEGER HOLD1
      INTEGER HOLD2
      INTEGER HOLD3
      INTEGER HOLD4
      INTEGER IHOLD
      INTEGER HOLDT1
      INTEGER HOLDT2
      INTEGER HOLDE1
      INTEGER HOLDE2
      INTEGER HOLDE3
      INTEGER HOLDQ
      INTEGER HOLDX
      INTEGER HOLDY
      INTEGER HOLDZ
      INTEGER HOLDF
      INTEGER HOLDW

      INTEGER ILEN
      INTEGER IMINT
      INTEGER IMINTER
      INTEGER IMQ
      INTEGER IMQER
      INTEGER IMU
      INTEGER IMUER
      INTEGER IMPOL
      INTEGER IMPOLER
      INTEGER IMPA
      INTEGER IMPAER

      INTEGER MEMI(1)

      REAL MTHETA
      REAL MATPOL(4,4)
      REAL MATROP(4,4)
      REAL MATMIR(4,4)
      REAL MATROT(4,4)
      REAL WORK(4,4)
      REAL MATSYS(4,4)

      REAL BINREJ
      REAL ERLIM
      REAL PACOR

      REAL MEMR(1)

      DOUBLE PRECISION MEMD(1)
      
      CHARACTER*32 LINAMI
      CHARACTER*32 LINAME
      CHARACTER*32 NAMIM(64)
      CHARACTER*32 NAMER(64)
      CHARACTER*16 INSTB   
      CHARACTER*16 INSTNAME
      CHARACTER*16 FILTNAME
      CHARACTER*16 ARRNAM(128)
      CHARACTER*32 TABNAM
      CHARACTER*32 ONAME
      CHARACTER*32 OUTROOT
      CHARACTER*132 OUTEXT

      LOGICAL LSTAT
      LOGICAL TEXIST
      LOGICAL MEMB(1)

      COMMON/MEM/MEMD
      EQUIVALENCE (MEMI,MEMR,MEMD,MEMB)

11     format(a32)
12     format(I8)

C
C     Initialize the filename template processing for the 
C     list of object-sky images at differing rotations of 
C     a polarizing element
C
120   CALL UCLGST('imalis',LINAMI,STAT)
      CALL TIMOTP(LINAMI,IMDSCRL,STAT)
C      CALL UIMOTP(LINAMI,IMDSCRL,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,111) 
111     FORMAT(' Error initializing filename template')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read all the names into the array NAMIM
C
      N=2
      J=1
      DO WHILE (N.GT.1)
110     CALL TIMXTP(IMDSCRL,NAMIM(J),STATT)
C120     CALL UIMXTP(IMDSCRL,NAMIM(J),STATT)
        IF (STATT.EQ.0) THEN
          J=J+1
        ENDIF
        IF (STATT.EQ.10) THEN
          WRITE(OUTEXT,121) 
121       FORMAT(' Error processing filename template')
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        IF (STATT.EQ.-2) THEN
C
C         End of list
C
          GO TO 125
        ENDIF
      ENDDO
125   NOINI=J-1
C
C     Close down the filename template
C
C127   CALL UIMCTP(IMDSCRL,STAT)
127   CALL TIMCTP(IMDSCRL,STAT)
C
C     Check that at least three file names present
C
      IF (NOINI.LT.3) THEN
        WRITE(OUTEXT,128) 
128     FORMAT(' Three or more input files required')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Open the first image. Get the header info and
C     determine the size of the image
C     
130   CALL UIMOPN(NAMIM(1),1,IMDSCRI(1),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,131) NAMIM(1)
131     FORMAT(' Error opening data file ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMGID(IMDSCRI(1),DTYPE,NAXISI,DIMEN,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,132) NAMIM(1)
132     FORMAT(' Error reading data file ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      IF (NAXISI.NE.2) THEN
        WRITE(OUTEXT,133) 
133     FORMAT(' Data file is not 2-dimensional')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      N1=DIMEN(1)
      N2=DIMEN(2)
C        
C     Get the name of the instrument from the header
C
      CALL UHDGST(IMDSCRI(1),'INSTRUME',INSTB,STATT)
      IF (STATT.EQ.40) THEN
        WRITE(OUTEXT,136) NAMIM(1) 
136     FORMAT(' Cannot find HST instrument name in data file header ',
     :         A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      IF (STATT.EQ.0) THEN
        INSTNAME=INSTB
      ELSE
        WRITE(OUTEXT,132) NAMIM(1)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the arrays of header
C     data for polarizer angle (THETA), correction angle
C     for THETA (REFANG), position angle of telescope 
C     (ORIENT) and correction to position angle (CORANG)
C     for the NOINI input images
C
      CALL UDMGET(NOINI,6,THETA,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,141)
141     FORMAT(' Unable to assign memory for internal 1-D real array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,REFANG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,141)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,ORIENT,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,141)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,CORANG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,141)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Depending on the instrument get the name of the polarizing
C     filter or the colour filter, the polarizer angle, the
C     correction to polarizer angle, the orientation angle (PA_V3) 
C     of the instrument and the correction to this angle.
C     IMTHETA indicates the instrument; depending on the 
C     instrument the polarizer angle may have to be matched up 
C     in the table of instrument polarization parameters
C
      CALL INSTHEAD(NOINI,IMDSCRI(1),INSTNAME,FILTNAME,
     :              MEMR(THETA),MEMR(REFANG),MEMR(ORIENT),
     :              MEMR(CORANG),1,MTHETA,IMTHETA,LSTAT)
      IF (.NOT.LSTAT) THEN
        WRITE(OUTEXT,146) NAMIM(1)
146     FORMAT(' Error reading header of data file ',A32)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the 2-D input image
C     of dimensions N1 by N2 (real number)
C
      NELEM=N1*N2
      CALL UDMGET(NELEM,6,HOLD,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,151) 
151     FORMAT(' Unable to assign memory for internal 2-D array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the cube of NOIN input
C     images of dimensions N1 by N2 (real numbers)
C
      NELEM=NOINI*N1*N2 
      CALL UDMGET(NELEM,6,OBJCUB,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,155) 
155     FORMAT(' Unable to assign memory for internal 3-D array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read the input image to HOLD
C
      CALL UIGS2R(IMDSCRI(1),1,N1,1,N2,MEMR(HOLD),STAT3)
      IF (STAT3.NE.0) THEN
        WRITE(OUTEXT,161) NAMIM(1)
161     FORMAT(' Error reading data from file ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Copy the array into the first plane of cube
C
      CALL INTOCUB(N1,N2,MEMR(HOLD),N1,N2,NOINI,MEMR(OBJCUB),1)
C
C     Get the name of the table file for the list of 
C     instrument polarization and filter properties. The 
C     tables are instrument specific
C
170   CALL UCLGST('instpoltab',TABNAM,STAT)
      CALL UTTACC(TABNAM,TEXIST,STAT)
      ILEN=INDEX(TABNAM,'   ') -1
      IF (.NOT.TEXIST) THEN
        WRITE(OUTEXT,171) TABNAM(:ILEN) 
171     FORMAT(' Table file ',A,' does not exist ')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UTTOPN(TABNAM,1,TBDSCR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,173) TABNAM(:ILEN)
173     FORMAT(' Table file ',A,' could not be opened')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the number of rows in the table
C
      CALL UTPGTI(TBDSCR,21,NROWS,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,175) TABNAM(:ILEN)
175     FORMAT(' Failed to find no. rows in table file ',A)
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
C     logical flag array required by UTCGT. 
C
      CALL UDMGET(NROWS,6,ARRANG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
181     FORMAT(' Unable to assign memory for internal 1-D real array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRTPAR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRTPER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRRS,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRRP,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRDELPH,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRINPO,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,ARRINPA,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,1,LANG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,183)
183     FORMAT(' Unable to assign memory for internal 1-D',
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
C     instrumental polarization (INPA) for the NOINI
C     data images
C
      CALL UDMGET(NOINI,6,TPAR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,TPER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,RS,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,RP,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,DELPH,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,INSPO,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,INSPA,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     From the table read the instrumental polarization parameters
C     appropriate to this setting given by polarizer / colour 
C     filter, polarizer angle if appropriate.
C
      CALL READ_POLTAB(TBDSCR,FILTNAME,MTHETA,IMTHETA,NROWS,
     :                 ARRNAM,MEMR(ARRANG),MEMR(ARRTPAR),
     :                 MEMR(ARRTPER),MEMR(ARRRS),MEMR(ARRRP),
     :                 MEMR(ARRDELPH),MEMR(ARRINPO),
     :                 MEMR(ARRINPA),MEMB(LANG),NOINI,1,
     :                 MEMR(THETA),MEMR(TPAR),MEMR(TPER),
     :                 MEMR(RS),MEMR(RP),MEMR(DELPH),MEMR(INSPO),
     :                 MEMR(INSPA),LSTAT)
      IF (.NOT.LSTAT) THEN
        WRITE(OUTEXT,193) TABNAM
193     FORMAT(' Error reading data from table file ',A32)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Loop through the list of data sets reading the values for
C     each into array HOLD and copying each into a plane
C     of the 3-D array OBJCUB
C
200   K=2
      DO J=2,NOINI,1
        CALL UIMOPN(NAMIM(J),1,IMDSCRI(J),STAT1)
        IF (STAT1.NE.0) THEN
          WRITE(OUTEXT,131) NAMIM(J)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 999
        ENDIF
        CALL UIMGID(IMDSCRI(J),DTYPE,NAXISI,DIMEN,STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,132) NAMIM(J)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        IF (NAXISI.NE.2) THEN
          WRITE(OUTEXT,133) 
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 999
        ENDIF
        N3=DIMEN(1)
        N4=DIMEN(2)
        IF (N3.NE.N1.OR.N4.NE.N2) THEN
          WRITE(OUTEXT,215) NAMIM(J)
215       FORMAT(' File ',A32,' not same size')
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 999
        ENDIF
C        
C       Get the name of the HST instrument from the header
C
        CALL UHDGST(IMDSCRI(J),'INSTRUME',INSTB,STATT)
        IF (STATT.EQ.40) THEN
          WRITE(OUTEXT,146) NAMIM(J) 
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        IF (STATT.EQ.0) THEN
          IF (INSTB.NE.INSTNAME) THEN
            WRITE(OUTEXT,216) NAMIM(J)
216         FORMAT(' Data file not necessarily same instrument ',A24)
            CALL UMSPUT(OUTEXT,3,0,STAT)
            GO TO 990
          ENDIF
        ELSE
          WRITE(OUTEXT,132) NAMIM(J)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
C
C       Depending on the instrument get the name of the polarizing
C       filter or the colour filter, the polarizer angle, the
C       correction to polarizer angle, the orientation angle (PA_V3) 
C       of the instrument and the correction to this angle
C
        CALL INSTHEAD(NOINI,IMDSCRI(J),INSTNAME,FILTNAME,MEMR(THETA),
     :                MEMR(REFANG),MEMR(ORIENT),MEMR(CORANG),
     :                J,MTHETA,IMTHETA,LSTAT)
        IF (.NOT.LSTAT) THEN
          WRITE(OUTEXT,146) NAMIM(J)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF

        IF (STAT2.EQ.0) THEN
          CALL UIGS2R(IMDSCRI(J),1,N3,1,N4,MEMR(HOLD),STAT3)
          IF (STAT3.NE.0) THEN
            WRITE(OUTEXT,161) NAMIM(J)
            CALL UMSPUT(OUTEXT,3,0,STAT)
            GO TO 999
          ENDIF
          CALL UIMCLO(IMDSCRI(J),STAT4)
          CALL INTOCUB(N3,N4,MEMR(HOLD),N1,N2,NOINI,
     :                 MEMR(OBJCUB),J)
          K=K+1
        ELSE
          WRITE(OUTEXT,219) NAMIM(J)
219       FORMAT(' Failed to read ',A24)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
C
C       From the table read the instrumental polarization parameters
C       appropriate to this setting given by polarizer / colour 
C       filter, polarizer angle if appropriate
C
        CALL READ_POLTAB(TBDSCR,FILTNAME,MTHETA,IMTHETA,NROWS,
     :                   ARRNAM,MEMR(ARRANG),MEMR(ARRTPAR),
     :                   MEMR(ARRTPER),MEMR(ARRRS),MEMR(ARRRP),
     :                   MEMR(ARRDELPH),MEMR(ARRINPO),
     :                   MEMR(ARRINPA),MEMB(LANG),NOINI,J,
     :                   MEMR(THETA),MEMR(TPAR),MEMR(TPER),
     :                   MEMR(RS),MEMR(RP),MEMR(DELPH),MEMR(INSPO),
     :                   MEMR(INSPA),LSTAT)
        IF (.NOT.LSTAT) THEN
          WRITE(OUTEXT,193) TABNAM
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
      ENDDO
      
230   IF (K-1.NE.NOINI) THEN
        WRITE(OUTEXT,231)
231     FORMAT(' Incompatible number of expected and read data files')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Close down the polarization parameter table since not
C     required to repeat parameters for error frames
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
C     Initialize the filename template processing for the 
C     list of error images at the same set of rotations of 
C     the polarizing element as the object-sky images
C
320   CALL UCLGST('errlis',LINAME,STAT)
      CALL TIMOTP(LINAME,IMDSCRM,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,121) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read all the names into the array NAMER
C
      J=1
      DO WHILE (N.GT.1)
330     CALL TIMXTP(IMDSCRM,NAMER(J),STATT)
C330     CALL UIMXTP(IMDSCRM,NAMER(J),STATT)
        IF (STATT.EQ.0) THEN
          J=J+1
        ENDIF
        IF (STATT.EQ.10) THEN
          WRITE(OUTEXT,131) 
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        IF (STATT.EQ.-2) THEN
C
C         End of list
C
          GO TO 335
        ENDIF
      ENDDO
335   NOINE=J-1
C
C     Check number of error frames ame as number of
C     data frames
C
      IF (NOINE.NE.NOINI) THEN
        WRITE(OUTEXT,336)
336     FORMAT(' No. of error frames must equal no. of
     : data frames')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Close down the filename template
C
C337   CALL UIMCTP(IMDSCRM,STAT)
337   CALL TIMCTP(IMDSCRM,STAT)
C
C     Get first error file and from the header determine
C     the size of the image
C
340   CALL UIMOPN(NAMER(1),1,IMDSCRE(1),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,131) NAMER(1)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMGID(IMDSCRE(1),DTYPE,NAXISI,DIMEN,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,132) NAMER(1)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      IF (NAXISI.NE.2) THEN
        WRITE(OUTEXT,133) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      M1=DIMEN(1)
      M2=DIMEN(2)
      IF (M1.NE.N1.OR.M2.NE.N2) THEN
        WRITE(OUTEXT,343)
343     FORMAT(' Object and error images not same size')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C        
C     Get the name of the HST instrument from the header
C
      CALL UHDGST(IMDSCRE(1),'INSTRUME',INSTB,STATT)
      IF (STATT.EQ.40) THEN
        WRITE(OUTEXT,136) NAMER(1) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      IF (STATT.EQ.0) THEN
        IF (INSTB.NE.INSTNAME) THEN
          WRITE(OUTEXT,216) NAMER(1)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
      ELSE
        WRITE(OUTEXT,132) NAMER(1)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the cube of NOINE input
C     error images of dimensions N1 by N2 (real numbers)
C
      NELEM=NOINE*M1*M2
      CALL UDMGET(NELEM,6,ERRCUB,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,155) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read the input image to HOLD
C
      CALL UIGS2R(IMDSCRE(1),1,M1,1,M2,MEMR(HOLD),STAT3)
      IF (STAT3.NE.0) THEN
        WRITE(OUTEXT,161) NAMER(1)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMCLO(IMDSCRE(1),STAT)
C
C     Copy the array into the first plane of cube
C
      CALL INTOCUB(M1,M2,MEMR(HOLD),M1,M2,NOINE,MEMR(ERRCUB),1)
C
C     Loop through the list of data sets reading the values for
C     each into array HOLD and copying each into a plane
C     of the 3-D array ERRCUB
C
400   J=2
      DO K=2,NOINE,1
        CALL UIMOPN(NAMER(J),1,IMDSCRE(J),STAT1)
        IF (STAT1.EQ.10) THEN
          WRITE(OUTEXT,131) NAMER(J)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 999
        ENDIF
        CALL UIMGID(IMDSCRE(J),DTYPE,NAXISI,DIMEN,STAT2)
        IF (NAXISI.NE.2) THEN
          WRITE(OUTEXT,133) 
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 999
        ENDIF
        M3=DIMEN(1)
        M4=DIMEN(2)
        IF (M3.NE.M1.OR.M4.NE.M2) THEN
          WRITE(OUTEXT,215) NAMER(J)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 999
        ENDIF
C        
C       Get the name of the HST instrument from the header
C
        CALL UHDGST(IMDSCRE(J),'INSTRUME',INSTB,STATT)
        IF (STATT.EQ.40) THEN
          WRITE(OUTEXT,136) NAMER(J) 
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        IF (STATT.EQ.0) THEN
          IF (INSTB.NE.INSTNAME) THEN
            WRITE(OUTEXT,216) NAMER(J)
            CALL UMSPUT(OUTEXT,3,0,STAT)
            GO TO 990
          ENDIF
        ELSE
          WRITE(OUTEXT,132) NAMER(J)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF

        IF (STAT2.EQ.0) THEN
          CALL UIGS2R(IMDSCRE(J),1,M3,1,M4,MEMR(HOLD),STAT3)
          IF (STAT3.NE.0) THEN
            WRITE(OUTEXT,161) NAMER(J)
            CALL UMSPUT(OUTEXT,3,0,STAT)
            GO TO 999
          ENDIF
          CALL UIMCLO(IMDSCRE(J),STAT4)
          CALL INTOCUB(M3,M4,MEMR(HOLD),M1,M2,NOINE,MEMR(ERRCUB),J)
          J=J+1
        ELSE
          WRITE(OUTEXT,219) NAMER(J)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
      ENDDO

430   IF (J-1.NE.NOINE) THEN
        WRITE(OUTEXT,231)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Free the dynamic memory allocated for HOLD
C
      CALL UDMFRE(HOLD,6,STAT)
C
C     Form the system of equations for the polarization
C     response of the system to convert measured counts
C     to Stokes Parameters I, Q and U for each input
C     image and set the arrays of efficiencies for the
C     I, Q and U Stokes parameters 
C
C     First allocate arrays for the efficiency arrays
C
      CALL UDMGET(NOINI,6,EFFI,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,EFFQ,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,EFFU,STAT)
      IF (STAT.NE.0) THEN 
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Do not need to set the polarizer rotation matrix
C     since have to solve the signal in the input images
C     as a function of polarizer rotation. 
C
      CALL POLEFFIC(NOINI,MEMR(THETA),MEMR(REFANG),MEMR(TPAR),
     :              MEMR(TPER),MEMR(RS),MEMR(RP),MEMR(DELPH),
     :              MEMR(ORIENT),MEMR(CORANG),4,4,MATPOL,
     :              MATROP,MATMIR,MATROT,WORK,MATSYS,MEMR(EFFI),
     :              MEMR(EFFQ),MEMR(EFFU))
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
C     Get the user variable input parameters to control the
C     binning and rejection thresholds
C
C     Get the value for the number of pixels in the 
C     input images to bin in forming the output images
C     (same in X and Y)
C
      CALL UCLGSI('xybin',XYBIN,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,623)
623     FORMAT(' Failed to get the binning factor')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Compute the size of the output arrays
C
      O1=NINT(REAL(N1)/REAL(XYBIN))
      O2=NINT(REAL(N2)/REAL(XYBIN))
C
C     Get the value of the factor times the rms on the
C     mean of signal in a binned pixel to reject a point 
C     from inclusion in the calculation of the mean
C
      CALL UCLGSR('binrej',BINREJ,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,627)
627     FORMAT(' Failed to get rejection factor for bin mean')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the value of the limiting polarization error in %
C     per output binned pixel above which no output data
C     will be written to the pixel
C
      CALL UCLGSR('errlim',ERLIM,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,639)
639     FORMAT(' Failed to get limiting polarization error')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      ERLIM=ERLIM/100.0
C
C     Additive correction to output polarization position 
C     angle taken as zero
C
      PACOR=0.0
C
C     Allocate dynamic memory for the arrays required by
C     POLANY
C     Integer array: HOLDQ
C     Real arrays: HOLD1, HOLD2, HOLDT1,HOLDT2, HOLDE1,
C                  HOLDE2, HOLDE3
C     Double Precision arrays: HOLDX, HOLDY, HOLDZ, HOLDTF, 
C                              HOLDW 
C
700   CALL UDMGET(NOINI,4,HOLDQ,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,711)
711     FORMAT(' Unable to assign memory for internal 1-D',
     :' integer array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Real arrays 
C 
      NBIN=XYBIN*XYBIN
      IF (NBIN.LT.4) THEN
C
C       Assign work arrays for CRMEAN sorting of minimum length 4
C
        NBIN=4
      ENDIF
      CALL UDMGET(NBIN,6,HOLD1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,713)
713     FORMAT(' Unable to assign memory for internal 1-D',
     :' real array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NBIN,6,HOLD2,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,713)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NBIN,6,HOLD3,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,713)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NBIN,6,HOLD4,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,713)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NBIN,4,IHOLD,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,713)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,HOLDT1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,713)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,HOLDT2,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,713)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,HOLDE1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,HOLDE2,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,6,HOLDE3,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Double precision
C
      CALL UDMGET(NOINI,7,HOLDX,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
715     FORMAT(' Unable to assign memory for internal 1-D',
     :' double precision array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,7,HOLDY,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,7,HOLDZ,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,7,HOLDF,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NOINI,7,HOLDW,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the output arrays 
C
      NELEM=O1*O2
      CALL UDMGET(NELEM,6,IMINT,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,725)
725     FORMAT(' Unable to assign memory for internal 2-D',
     :' real array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,IMINTER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,725)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,IMQ,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,725)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,IMQER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,725)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,IMU,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,725)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,IMUER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,725)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,IMPOL,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,725)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,IMPOLER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,725)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,IMPA,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,725)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,IMPAER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,725)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Call the grand subroutine to compute the total signal,
C     Stokes parameters Q and U, linear polarization (%),
C     polarization position angle (degrees) and error images
C     from the NOINI input object-sky and error images
C
      CALL POLANF(N1,N2,NOINI,MEMR(OBJCUB),MEMR(ERRCUB),
     :            MEMR(THETA),MEMR(EFFI),MEMR(EFFQ),
     :            MEMR(EFFU),MEMR(REFANG),MEMR(INSPO),
     :            MEMR(INSPA),MEMR(ORIENT),MEMR(CORANG),
     :            XYBIN,BINREJ,ERLIM,PACOR,NBIN,MEMR(HOLD1),
     :            MEMR(HOLD2),MEMR(HOLD3),MEMR(HOLD4),
     :            MEMI(IHOLD),MEMR(HOLDT1),MEMR(HOLDT2),
     :            MEMR(HOLDE1),MEMR(HOLDE2),MEMR(HOLDE3),
     :            MEMI(HOLDQ),MEMD(HOLDX),MEMD(HOLDY),
     :            MEMD(HOLDZ),MEMD(HOLDF),MEMD(HOLDW),O1,
     :            O2,MEMR(IMINT),MEMR(IMINTER),MEMR(IMQ),
     :            MEMR(IMQER),MEMR(IMU),MEMR(IMUER),
     :            MEMR(IMPOL),MEMR(IMPOLER),MEMR(IMPA),
     :            MEMR(IMPAER))
C
C     Free the dynamic memory allocated which is no longer 
C     required
C
      CALL UDMFRE(OBJCUB,6,STAT)
      CALL UDMFRE(ERRCUB,6,STAT)
      CALL UDMFRE(THETA,6,STAT)
      CALL UDMFRE(REFANG,6,STAT)
      CALL UDMFRE(ORIENT,6,STAT)
      CALL UDMFRE(CORANG,6,STAT)
      CALL UDMFRE(EFFI,6,STAT)
      CALL UDMFRE(EFFQ,6,STAT)
      CALL UDMFRE(EFFU,6,STAT)
      CALL UDMFRE(INSPO,6,STAT)
      CALL UDMFRE(INSPA,6,STAT)
      CALL UDMFRE(HOLDQ,4,STAT)
      CALL UDMFRE(HOLD1,6,STAT)
      CALL UDMFRE(HOLD2,6,STAT)
      CALL UDMFRE(HOLD3,6,STAT)
      CALL UDMFRE(HOLD4,6,STAT)
      CALL UDMFRE(IHOLD,4,STAT)
      CALL UDMFRE(HOLDT1,6,STAT)
      CALL UDMFRE(HOLDT2,6,STAT)
      CALL UDMFRE(HOLDE1,6,STAT)
      CALL UDMFRE(HOLDE2,6,STAT)
      CALL UDMFRE(HOLDE3,6,STAT)
      CALL UDMFRE(HOLDX,7,STAT)
      CALL UDMFRE(HOLDY,7,STAT)
      CALL UDMFRE(HOLDZ,7,STAT)
      CALL UDMFRE(HOLDF,7,STAT)
      CALL UDMFRE(HOLDW,7,STAT)
C
C     Get the rootname for the output images
C
800   CALL UCLGST('outroot',OUTROOT,STAT)
      ILEN=INDEX(OUTROOT,'   ') - 1
      ODIMEN(1)=O1
      ODIMEN(2)=O2
C
C     Open an output file for the total signal image
C
820   ONAME=OUTROOT(:ILEN)//'int'
      CALL UIMCRE(ONAME,6,2,ODIMEN,IMDSCRO,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCRI(1),IMDSCRO,STAT2)
        CALL UIPS2R(IMDSCRO,1,O1,1,O2,MEMR(IMINT),STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,821) ONAME
821       FORMAT(' Error writing data to file ',A24)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCRO,STAT3)
      ELSE
        WRITE(OUTEXT,823) ONAME
823     FORMAT(' Failed to open data file ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Open an output file for the total signal error image 
C
830   ONAME=OUTROOT(:ILEN)//'inter'
      CALL UIMCRE(ONAME,6,2,ODIMEN,IMDSCRO,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCRI(1),IMDSCRO,STAT2)
        CALL UIPS2R(IMDSCRO,1,O1,1,O2,MEMR(IMINTER),STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,821) ONAME
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCRO,STAT3)
      ELSE
        WRITE(OUTEXT,823) ONAME
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Open an output file for the percentage linear polarization 
C
840   ONAME=OUTROOT(:ILEN)//'pol'
      CALL UIMCRE(ONAME,6,2,ODIMEN,IMDSCRO,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCRI(1),IMDSCRO,STAT2)
        CALL UIPS2R(IMDSCRO,1,O1,1,O2,MEMR(IMPOL),STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,821) ONAME
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCRO,STAT3)
      ELSE
        WRITE(OUTEXT,823) ONAME
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Open an output file for the percentage linear polarization 
C     error
C
850   ONAME=OUTROOT(:ILEN)//'poler'
      CALL UIMCRE(ONAME,6,2,ODIMEN,IMDSCRO,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCRI(1),IMDSCRO,STAT2)
        CALL UIPS2R(IMDSCRO,1,O1,1,O2,MEMR(IMPOLER),STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,821) ONAME
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCRO,STAT3)
      ELSE
        WRITE(OUTEXT,823) ONAME
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Open an output file for the polarization position angle
C     in degrees
C
860   ONAME=OUTROOT(:ILEN)//'pa'
      CALL UIMCRE(ONAME,6,2,ODIMEN,IMDSCRO,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCRI(1),IMDSCRO,STAT2)
        CALL UIPS2R(IMDSCRO,1,O1,1,O2,MEMR(IMPA),STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,821) ONAME
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCRO,STAT3)
      ELSE
        WRITE(OUTEXT,823) ONAME
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Open an output file for the polarization position angle
C     error in degrees
C
870   ONAME=OUTROOT(:ILEN)//'paer'
      CALL UIMCRE(ONAME,6,2,ODIMEN,IMDSCRO,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCRI(1),IMDSCRO,STAT2)
        CALL UIPS2R(IMDSCRO,1,O1,1,O2,MEMR(IMPAER),STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,821) ONAME
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCRO,STAT3)
      ELSE
        WRITE(OUTEXT,823) ONAME
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Open an output file for the Stokes Q parameter
C
900   ONAME=OUTROOT(:ILEN)//'q'
      CALL UIMCRE(ONAME,6,2,ODIMEN,IMDSCRO,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCRI(1),IMDSCRO,STAT2)
        CALL UIPS2R(IMDSCRO,1,O1,1,O2,MEMR(IMQ),STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,821) ONAME
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCRO,STAT3)
      ELSE
        WRITE(OUTEXT,823) ONAME
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Open an output file for the Stokes Q parameter
C     error
C
910   ONAME=OUTROOT(:ILEN)//'qer'
      CALL UIMCRE(ONAME,6,2,ODIMEN,IMDSCRO,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCRI(1),IMDSCRO,STAT2)
        CALL UIPS2R(IMDSCRO,1,O1,1,O2,MEMR(IMQER),STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,821) ONAME
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCRO,STAT3)
      ELSE
        WRITE(OUTEXT,823) ONAME
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Open an output file for the Stokes U parameter
C
920   ONAME=OUTROOT(:ILEN)//'u'
      CALL UIMCRE(ONAME,6,2,ODIMEN,IMDSCRO,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCRI(1),IMDSCRO,STAT2)
        CALL UIPS2R(IMDSCRO,1,O1,1,O2,MEMR(IMU),STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,821) ONAME
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCRO,STAT3)
      ELSE
        WRITE(OUTEXT,823) ONAME
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Open an output file for the Stokes U parameter
C     error
C
930   ONAME=OUTROOT(:ILEN)//'uer'
      CALL UIMCRE(ONAME,6,2,ODIMEN,IMDSCRO,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCRI(1),IMDSCRO,STAT2)
        CALL UIPS2R(IMDSCRO,1,O1,1,O2,MEMR(IMUER),STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,821) ONAME
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCRO,STAT3)
      ELSE
        WRITE(OUTEXT,823) ONAME
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Free the dynamic memory allocated for the output
C     images
C
      CALL UDMFRE(IMINT,6,STAT)
      CALL UDMFRE(IMINTER,6,STAT)
      CALL UDMFRE(IMQ,6,STAT)
      CALL UDMFRE(IMQER,6,STAT)
      CALL UDMFRE(IMU,6,STAT)
      CALL UDMFRE(IMUER,6,STAT)
      CALL UDMFRE(IMPOL,6,STAT)
      CALL UDMFRE(IMPOLER,6,STAT)
      CALL UDMFRE(IMPA,6,STAT)
      CALL UDMFRE(IMPAER,6,STAT)
C
C     Close down the first image read (used as template
C     for output files)
C
      CALL UIMCLO(IMDSCRI(1),STAT)
      GO TO 999

990   WRITE(OUTEXT,991)
991   FORMAT(' Program failed. No output')
      CALL UMSPUT(OUTEXT,1,0,STAT)

999   END

      SUBROUTINE POLANF(N1,N2,N3,OBJCUB,ERRCUB,ANG,EFFI,EFFQ,
     :                  EFFU,REFANG,INSPO,INSPA,ORIENT,CORANG,
     :                  XYBIN,SIGREJ,LPLIM,PAZERO,N12,HOLD11,
     :                  HOLD12,HOLD13,HOLD14,IHOLD1,TOTO,TOTE,
     :                  HEFFI,HEFFQ,HEFFU,DQ,X,Y,DY,YF,WT,M1,
     :                  M2,STOI,STOIER,STOQ,STOQER,STOU,STOUER,
     :                  LP,LPERR,TH,THERR)
C
C     Calculates the total intensity (Stokes I), the linear 
C     polarization and polarization position angle and errors 
C     from the N3 object-sky images and error images taken at 
C     polarizer angles specified by the values in the array 
C     ANG (in instrumental reference frame). 
C     The output images are binned in X and Y by the integer 
C     values given by XYBIN. Rejection of points greater than 
C     SIGREJ*sigma from the mean for the bin (and considering 
C     surrounding pixel values for small bins) is applied. 
C     The polarizarion is calculated using the instrumental 
C     efficiencies for Stokes I, Q and U given by EFFI, EFFQU 
C     and EFFUQ respectively and the instrumental polarization is 
C     also corrected. 
C
C     Linear polarization is calculated correcting for the bias 
C     as follows:
C        For P/sigma(P) > 5
C           P=SQRT(Q**2. + U**2. - SIGP**2.)
C           (Serkowski,1962, Advances in Astron. & Astrophys. 1, 289)
C        For P/sigma(P) < 5
C           the most probable value of P from the fit to the 
C           peak of the Rice distribution given by Equ. A2 
C           (Wardle & Kronberg (ApJ, 194, 249, 1974).
C           
C     Polarization position angle is calculated as follows:
C        THETA=0.5*ATAN(U/Q)
C     and corrected to some standard position angle system by the 
C     offset PAZERO.
C
C     The errors on polarization are calculated according to 
C     Serkowski 1962 (Advances in Astron. & Astrophys. 1, 289)
C     and for polarization position angle according to 
C     Naghizadeh-Khouei & Clarke, A&A, 274, 968, 1993. 
C
C     Input & output parameters:
C       N1,N2    X and Y dimensions of input cubes
C       N3       Z dimension of input cubes
C       OBJCUB   Cube of N3 object - sky images
C       ERRCUB   Cube of N3 error images
C       ANG      Array of the input polarization analyser position angles
C       EFFI     Array of efficiences for detection of Stokes I
C       EFFQ     Array of efficiences for detection of Stokes Q
C       EFFU     Array of efficiences for detection of Stokes U
C       REFANG   Array of corrections to polarizer position angle
C       INSPO    Fractional instrumental polarization
C       INSPA    Position angle of instrumental polarization
C       ORIENT   Position angle of HST V3 axis (wrt reference axis of instrument) 
C       CORANG   Additive correction to position angle (to instrument frame)
C       XYBIN    Integer binning factor for X and Y axes
C       SIGREJ   Factor times sigma on mean for rejection of points
C       LPLIM    Limiting polarization error to accept for output
C       PAZERO   Position angle offset to correct PA 0
C       N12      Dimension of holding arays HOLD11 and HOLD12
C       HOLD11   1-D holding array for sorting in CRMEAN
C       HOLD12   1-D holding array for sorting in CRMEAN
C       HOLD13   1-D holding array for sorting in CRMEAN
C       HOLD14   1-D holding array for sorting in CRMEAN
C       IHOLD1   1-D holding array for sorting in CRMEAN
C       TOTO     1-D holding array for output obj-sky values from CRMEAN
C       TOTE     1-D holding array for output error values from CRMEAN
C       HEFFI    Holding array for efficiences for detection of Stokes I
C       HEFFQ    Holding array for efficiences for detection of Stokes Q
C       HEFFU    Holding array for efficiences for detection of Stokes U
C       DQ       1-D holding array for data quality in LS fit
C       X        1-D holding array for position angle in LS fit
C       Y        1-D holding array for signal values in LS fit
C       DY       1-D holding array for error on signal in LS fit
C       YF       1-D holding array for fitted signal values in LS fit
C       WT       1-D holding array for signal weights in LS fit
C       M1,M2    X and Y dimensions of output arrays
C       STOI     Output array of the I Stokes parameter
C       STOIER   Output array of the I Stokes parameter error
C       STOQ     Output array of the Q Stokes parameter
C       STOQER   Output array of the Q Stokes parameter error
C       STOU     Output array of the U Stokes parameter
C       STOUER   Output array of the U Stokes parameter error
C       LP       Output array of the linear polarization (%)
C       LPERR    Output array of the polarization error (%)
C       TH       Output array of the polarization position angle (degrees)
C       THERR    Output array of the position angle error (degrees)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N1
      INTEGER N2
      INTEGER N3
      INTEGER XYBIN
      INTEGER N12
      INTEGER M1
      INTEGER M2
      INTEGER IHOLD1(N12)
      INTEGER DQ(N3)

      REAL OBJCUB(N1,N2,N3)
      REAL ERRCUB(N1,N2,N3)
      REAL ANG(N3)
      REAL REFANG(N3)
      REAL EFFI(N3)
      REAL EFFQ(N3)
      REAL EFFU(N3)
      REAL INSPO(N3)
      REAL INSPA(N3)
      REAL ORIENT(N3)
      REAL CORANG(N3)
      REAL SIGREJ
      REAL LPLIM
      REAL PAZERO
      REAL HOLD11(N12)
      REAL HOLD12(N12)
      REAL HOLD13(N12)
      REAL HOLD14(N12)
      REAL TOTO(N3)
      REAL TOTE(N3)
      REAL HEFFI(N3)
      REAL HEFFQ(N3)
      REAL HEFFU(N3)
      REAL STOI(M1,M2)
      REAL STOIER(M1,M2)
      REAL STOQ(M1,M2)
      REAL STOQER(M1,M2)
      REAL STOU(M1,M2)
      REAL STOUER(M1,M2)
      REAL LP(M1,M2)
      REAL LPERR(M1,M2)
      REAL TH(M1,M2)
      REAL THERR(M1,M2)

      DOUBLE PRECISION X(N3)
      DOUBLE PRECISION Y(N3)
      DOUBLE PRECISION DY(N3)
      DOUBLE PRECISION YF(N3)
      DOUBLE PRECISION WT(N3)
C
C     Local variables
C
      INTEGER I,J,K
      INTEGER II,JJ
      INTEGER NMEAN
      INTEGER STAT
      INTEGER NOZI
      INTEGER NOZO
      INTEGER NIFAIL
      INTEGER INDX(3)
      INTEGER IFAIL

      REAL INSTPO
      REAL INSTPA
      REAL TOTI
      REAL TOTIER
      REAL TOTQ
      REAL TOTQER
      REAL TOTU
      REAL TOTUER
      REAL LPOL
      REAL LPOLER
      REAL LPA
      REAL LPAER

      DOUBLE PRECISION ARRI(3,3)
      DOUBLE PRECISION COVI(3,3)

      CHARACTER*132 TEXT

      LOGICAL LEFCHK
C
C     Check that all the matrix coefficients EFFI, EFFQ and EFFU 
C     are distinct. If all equal then calculation of polarization 
C     not possible. Report an error and exit subroutine
C
      CALL EFFCHK(N3,EFFI,EFFQ,EFFU,LEFCHK)
      IF (LEFCHK) THEN
        WRITE(TEXT,21) 
21      FORMAT(' All polarizer efficiencies equal. ', 
     :'No polarization calculation possible')
        CALL UMSPUT(TEXT,1,0,STAT)
        GO TO 999
      ENDIF

c      CALL UMSPUT(' Image     EFFI       EFFQ       EFFU',1,0,STAT)
c      DO I=1,N3,1
c        WRITE(TEXT,11) I,EFFI(I),EFFQ(I),EFFU(I)
c11      FORMAT(2X,I3,2X,F9.4,2X,F9.4,2X,F9.4)
c        CALL UMSPUT(TEXT,1,0,STAT)
c      ENDDO

      NOZI=0
      NOZO=0
      NIFAIL=0
C
C     Set instrumental polarization and position angle.
C     Need to correct position angle of instrumental 
C     polarization to absolute value on sky
C
      INSTPO=INSPO(1)/100. ! Fractional
      INSTPA=INSPA(1) + CORANG(1) ! CORANG constant for all N3 
C
C     For the case of only three input polarizer values
C     form the matrix of system efficiences, the LU
C     decomposition of this matrix and the inverse of the
C     matrix of system efficiences for the covariance matrix
C
      IF (N3.EQ.3) THEN
        CALL LUSETUP(N3,EFFI,EFFQ,EFFU,ARRI,COVI,INDX)
      ENDIF
C
C     Loop through all pixels in the output arrays, summing the 
C     data pixel values, computing Stokes I, Q and U, linear 
C     polarization and position angle and errors
C
      JJ=1
c      WRITE(TEXT,'(A)') '   I    J   TOTI    TOTIERR     Q    Q ERR'//
c     :'     U    U ERR    P(%)   PERR(%)  TH(DEG) THERR(DEG)'
c      CALL UMSPUT(TEXT,1,0,STAT)

      DO J=1,N2,XYBIN
        II=1
        DO I=1,N1,XYBIN
C
C       Initialize the running totals
C
          DO K=1,N3,1
            TOTO(K)=0.0
            TOTE(K)=0.0
C
C           Form the total object-sky and error in the bin
C           rejecting points greater than SIGREJ*sigma from 
C           the mean (e.g. cosmic rays) from the bin
C
            CALL CRMEANE(N1,N2,N3,OBJCUB,ERRCUB,K,I,J,XYBIN,
     :                   SIGREJ,N12,HOLD11,HOLD12,HOLD13,HOLD14,
     :                   IHOLD1,TOTO(K),TOTE(K),NMEAN)
C
C           Check for crazily small values which can cause 
C           overflows later. If found set to zero and issue 
C           a warning
C
            IF (TOTO(K).LT.1.0E-10) THEN
c              WRITE(TEXT,21) 
c21            FORMAT(' Warning: small or negative values set to zero')
c              CALL UMSPUT(TEXT,1,0,STAT)
              TOTO(K)=0.0
              TOTE(K)=0.0
              NOZI=NOZI+1
            ENDIF
c            write(text,883) K,toto(K),tote(K)
c883         format('K,TOTO,TOTE ',I5,2(E12.5))
c            CALL UMSPUT(TEXT,1,0,STAT)

          ENDDO
C
C         Form the linear polarization and position angle and
C         errors for this bin
C
          IFAIL=0
          CALL POLCALF(N3,TOTO,TOTE,ANG,EFFI,EFFQ,EFFU,
     :                 INSTPO,INSTPA,LPLIM,ARRI,COVI,INDX,
     :                 HEFFI,HEFFQ,HEFFU,DQ,X,Y,DY,YF,WT,
     :                 TOTI,TOTIER,TOTQ,TOTQER,TOTU,TOTUER,
     :                 LPOL,LPOLER,LPA,LPAER,IFAIL)
          IF (IFAIL.NE.0) THEN
            NIFAIL=NIFAIL+1
          ENDIF
          IF (TOTI.EQ.0.0.AND.LPOL.EQ.0.0) THEN
            NOZO=NOZO+1
          ENDIF
C
C         Write output values to result arrays
C
          IF (II.LE.M1.AND.JJ.LE.M2) THEN
            STOI(II,JJ)=TOTI
            STOIER(II,JJ)=TOTIER
            STOQ(II,JJ)=TOTQ
            STOQER(II,JJ)=TOTQER
            STOU(II,JJ)=TOTU
            STOUER(II,JJ)=TOTUER
            LP(II,JJ)=LPOL
            LPERR(II,JJ)=LPOLER
C
C           Set position angle, add zero offset and check for
C           negative values only for points with valid output data
C
            IF (LP(II,JJ).EQ.0.0.AND.LPERR(II,JJ).EQ.0.0) THEN
              TH(II,JJ)=0.0 ! Not valid output
            ENDIF
            IF (LP(II,JJ).NE.0.0.AND.LPERR(II,JJ).EQ.0.0) THEN
              TH(II,JJ)=LPA + PAZERO ! Valid output without errors
            ENDIF
            IF (LPERR(II,JJ).NE.0.0) THEN
              TH(II,JJ)=LPA + PAZERO
            ENDIF
            IF (TH(II,JJ).LT.0.0) THEN
              TH(II,JJ)=180.0 + TH(II,JJ)
            ENDIF
            IF (TH(II,JJ).GT.180.0) THEN
              TH(II,JJ)=TH(II,JJ) - 180.0
            ENDIF
            THERR(II,JJ)=LPAER
c            IF (LPOL.NE.0.0) THEN
c              WRITE(TEXT,77) I,J,TOTI,TOTIER,TOTQ,
c     :TOTQER,TOTU,TOTUER,LPOL,LPOLER,LPA,LPAER
c77            FORMAT(1X,I4,1X,I4,E10.3,1X,F8.1,1X,4(1X,F6.3),1X,
c     :2(1X,F6.2),2X,F7.2,1X,F7.2)
c              CALL UMSPUT(TEXT,1,0,STAT)
c            ENDIF
          ENDIF
          II=II+1
        ENDDO
        JJ=JJ+1
      ENDDO
C
C     Report number of small-negative values set to zero
C
      IF (NOZI.GT.0) THEN
        WRITE(TEXT,960) NOZI
960     FORMAT(' Warning: ',I6,' small-negative values set to',
     :' zero in processing')
        CALL UMSPUT(TEXT,1,0,STAT)
      ENDIF
C
C     Report number of failures to calulate I, pol and theta' for
C     least squares case
C
      IF (NIFAIL.GT.0) THEN
        WRITE(TEXT,970) NIFAIL
970     FORMAT(' Warning: ',I6,' failures occurred in least',
     :' squares computation')
        CALL UMSPUT(TEXT,1,0,STAT)
      ENDIF
C
C     Report number of output bins set to zero
C
      IF (NOZO.GT.0) THEN
        WRITE(TEXT,980) NOZO
980     FORMAT(' Warning: ',I6,' bins set to zero',
     :' in output files')
        CALL UMSPUT(TEXT,1,0,STAT)
      ENDIF

999   END

      SUBROUTINE CRMEANE(N1,N2,N3,CUBV,CUBE,NK,NI,NJ,XYBIN,
     :                   REJ,N12,VARRY,EARRY,WARRY,FARRY,IWKSP,
     :                   VTOT,ETOT,NMEAN)
C
C     This subroutine determines the mean value of the NKth 
C     plane of the cube CUBV and the mean error from CUBE over 
C     the 1st dimension range NI to NI+XYBIN-1 and the 2nd 
C     dimension range NJ to NJ+XYBIN-1 rejecting points which 
C     are greater than REJ*sigma from the mean. The rejection 
C     pass is iterated until no more points are rejected. The 
C     total value for the bin (in XYBIN*XYBIN pixels), the mean 
C     error in the bin and the number of pixel values used in 
C     forming the mean are returned.
C
      IMPLICIT NONE
      INTEGER N1 ! 1st dimension of input CUB arrays
      INTEGER N2 ! 2nd dimension of input CUB arrays
      INTEGER N3 ! 3rd dimension of input CUB arrays
      INTEGER NK ! Plane (3rd dimension) on which to determine mean value
      INTEGER NI ! 1st dimension starting value to sum for mean 
      INTEGER NJ ! 2nd dimension starting value to sum for mea
      INTEGER XYBIN ! Bin size in pixels for summing
      INTEGER N12 ! Dimension of work arrays
      INTEGER IWKSP(N12) ! Work array for sorting values
      INTEGER NMEAN ! Output number of points included in mean

      REAL CUBV(N1,N2,N3) ! Input 3 dimension array of input values
      REAL CUBE(N1,N2,N3) ! Input 3 dimension array of input errors
      REAL REJ ! Rejection factor (* rms) for point rejection from mean
      REAL VARRY(N12) ! Holding array for data values
      REAL EARRY(N12) ! Holding array for error values
      REAL WARRY(N12) ! Holding array for unrejected values
      REAL FARRY(N12) ! Holding array for unrejected errors
      REAL VTOT ! Output mean value in binned region
      REAL ETOT ! Output total error for mean in binned region
C
C     Local variables
C
      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER NV
      INTEGER NEX
      INTEGER KJ

      REAL MEAN
      REAL RMS
      REAL SUMER
      REAL DISCR
      REAL DISCM
      REAL DISC
C
C     Copy region of input array from NI to NI+XYBIN
C     and NJ to NJ+XYBIN to holding array VARRY for
C     signal and EARRY for error
C
      K=1
      DO J=NJ,NJ+XYBIN-1,1
        DO I=NI,NI+XYBIN-1,1
          IF (I.GE.1.AND.I.LE.N1.AND.J.GE.1.AND.J.LE.N2) THEN
            VARRY(K)=CUBV(I,J,NK)
            EARRY(K)=CUBE(I,J,NK)
            K=K+1
          ENDIF
        ENDDO
      ENDDO
      NV=K-1
C
C     Deal with silly cases of none or only 1 points to average
C
      IF (NV.EQ.0) THEN
        VTOT=0.0
        ETOT=0.0
        NMEAN=0
        GO TO 999
      ENDIF
      IF (NV.EQ.1) THEN
        VTOT=VARRY(1)
        ETOT=EARRY(1)
        NMEAN=1
        GO TO 999
      ENDIF
C
C     Sort the arrays VARRY and EARRY into ascending numerical order 
C     on the value of VARRY using the QUICKSORT algorithm
C    
      CALL SORT2(NV,VARRY,EARRY,IWKSP)
C
C     Based on the number of values in array SARRY determine 
C     how many points to initially exclude when forming the 
C     mean. Exclude the top 10% of pixel values.
C
      NEX=NV/10
      IF (NEX.LT.1) THEN
        NEX=1
      ENDIF
C
C     Form the mean and RMS of SARRY on points 1 upto NK-NEX 
C     (if REJ positive) or NEX to NK (if REJ negative)
C
      CALL MEANARE(NV,NEX,VARRY,EARRY,REJ,MEAN,RMS,SUMER,NMEAN)
C
C     If rms is zero then all points have same value so no 
C     necesssity to run rejection iterations
C
      IF (RMS.EQ.0.0) THEN
        GO TO 900
      ENDIF
C
C     If no points in bin(!) then exit
C
      IF (NMEAN.EQ.0) THEN
        VTOT=0.0
        ETOT=0.0
        GO TO 999
      ENDIF
C
C     Set the discriminator value to reject points from 
C     inclusion in the mean, 
C     REJ times the rms on the mean, or 
C     REJ times the noise on the value of the mean 
C     (using the conversion to electrons), 
C     whichever is the larger. Copy unrejected points to 
C     array TARRY
C
100   DISCR=RMS
      IF (MEAN.GT.0.0) THEN
        DISCM=SQRT(MEAN)
      ELSE
        DISCM=RMS
      ENDIF
      DISC=AMAX1(DISCR,DISCM)
      J=1
      DO I=1,NV,1
        IF (ABS(VARRY(I)-MEAN).LT.(REJ*DISC)) THEN
          WARRY(J)=VARRY(I)
          FARRY(J)=EARRY(I)
          J=J+1
        ENDIF
      ENDDO
      KJ=J-1
C
C     Form the mean of TARRY on points 1 upto KJ
C
      CALL MEANARE(KJ,0,WARRY,FARRY,REJ,MEAN,RMS,SUMER,NMEAN)
      IF (NMEAN.EQ.0) THEN
C
C       Escape for craziness (very small rms causes all 
C       points to be rejected). Form the mean of SARRY on 
C       points 1 upto NV and set total value to mean per 
C       pixel times number of pixels
C
        CALL MEANARE(NV,0,VARRY,FARRY,REJ,MEAN,RMS,SUMER,NMEAN)
        GO TO 900
      ENDIF
      IF (KJ.EQ.NV) THEN
C
C       No more points to reject. Set total value to mean per 
C       pixel times number of pixels
C
        GO TO 900
      ENDIF
C
C     Copy array WARRY to array VARRY for next rejection 
C     iteration
C
      DO I=1,KJ,1
        VARRY(I)=WARRY(I)
        EARRY(I)=FARRY(I)
      ENDDO
      NV=KJ

      GO TO 100 ! While there are points to reject
  
900   VTOT=MEAN*REAL(XYBIN*XYBIN)
      ETOT=SQRT(SUMER)

999   END

      SUBROUTINE MEANARE(N,EX,ARRY,ERRY,REJ,MEAN,RMS,TOTER,NMEAN)
C
C     This subroutine computes the mean of the values in 
C     the array ARRY for the range of points 1 to N-EX (if 
C     REJ is positive) or EX to N (if REJ is negative), 
C     returning the mean, the rms on the mean, the sum of
C     the errors in quadrature for this range and the number 
C     of points used for the mean
C
      IMPLICIT NONE
      INTEGER N ! No. of points in input arrays
      INTEGER EX ! No. of points to exclude from the mean of the sorted values 
      INTEGER NMEAN ! No. of points used in calculating mean
      REAL ARRY(N) ! Input array of values
      REAL ERRY(N) ! Input array of errors
      REAL REJ ! Rejection factor (* rms) for point rejection from mean
      REAL MEAN ! Output mean value 
      REAL RMS ! Output rms on mean value
      REAL TOTER ! Output total error for mean value
C
C     Local variables
C
      INTEGER I
      INTEGER SUMN
      REAL SUM,SUME

      SUM=0.0
      SUME=0.0
      SUMN=0
      IF (REJ.LT.0.0) THEN
        DO I=EX+1,N,1
          SUM=SUM+ARRY(I)
          SUME=SUME + ERRY(I)*ERRY(I)
          SUMN=SUMN+1
        ENDDO
      ELSE
        DO I=1,N-EX,1
          SUM=SUM+ARRY(I)
          SUME=SUME + ERRY(I)*ERRY(I)
          SUMN=SUMN+1
        ENDDO
      ENDIF
      IF (SUMN.NE.0) THEN
        MEAN=SUM/REAL(SUMN)
        NMEAN=SUMN
        TOTER=SUME
      ELSE
        MEAN=0.0
        NMEAN=0
        TOTER=0.0
        RMS=0.0
        GO TO 99
      ENDIF
C
C     Compute rms on mean
C
      SUM=0.0
      IF (REJ.LT.0.0) THEN
        DO I=EX+1,N,1
          SUM=SUM + (ARRY(I)-MEAN)**2.  
        ENDDO
      ELSE
        DO I=1,N-EX,1
          SUM=SUM + (ARRY(I)-MEAN)**2.  
        ENDDO
      ENDIF   
      RMS=SQRT(SUM/REAL(SUMN))

99    END 
