      SUBROUTINE POLIMH
C
C     This FORTRAN program calculates the total signal, the 
C     linear polarization and the position angle of linear 
C     polarization for an input table of apertures from any 
C     number (>=3) of images and corresponding error images 
C     The input data are taken at different angles of a 
C     polarizer for any of the HST instruments. Currently 
C     FOC, WFPC2, NICMOS and ACS are supported. A non-HST 
C     instrument can also be handled called `SPECIAL'. Error 
C     arrays for total signal, linear polarization and 
C     polarization position angle are also produced. In 
C     addition images of the Stokes parameters Q and U and 
C     their errors files are output.
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
C     A table listing the target name, the X,Y central pixel,
C     the radius for summing the object flux, the object-sky gap
C     (pixels) and the delta radius of the sky annulus is read.
C     The object and sky statistics are determined, the
C     integrated flux, linear polarization and position angle 
C     and errors are calculated and written to an output table.
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
      INTEGER TBDSCR

      INTEGER NAXISI
      INTEGER DIMEN(7)
      INTEGER DTYPE

      INTEGER NOINI
      INTEGER NOINE
      INTEGER N1
      INTEGER N2
      INTEGER N3
      INTEGER N4
      INTEGER NROWS
      INTEGER M1
      INTEGER M2
      INTEGER M3
      INTEGER M4

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

      INTEGER OSTDSCR
      INTEGER LOBJV
      INTEGER OSROWS
      INTEGER OBJPOX
      INTEGER OBJPOY
      INTEGER OBJRAD
      INTEGER OSARAD
      INTEGER SKYRAD
      INTEGER MNOSKY
      INTEGER NREJS

      INTEGER HOLD1
      INTEGER HOLD2
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
      INTEGER TINT
      INTEGER TINTER
      INTEGER TQ
      INTEGER TQER
      INTEGER TU
      INTEGER TUER
      INTEGER TPOL
      INTEGER TPOLER
      INTEGER TPA
      INTEGER TPAER

      INTEGER OTDSCR
      INTEGER COLID(13)
 
      INTEGER MEMI(1)

      REAL MTHETA
      REAL MATPOL(4,4)
      REAL MATROP(4,4)
      REAL MATMIR(4,4)
      REAL MATROT(4,4)
      REAL WORK(4,4)
      REAL MATSYS(4,4)

      REAL SKYREJ
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
      CHARACTER*32 OSTNAM
      CHARACTER*32 OBJNAME(2048)
      CHARACTER*32 OUTTAB
      CHARACTER*132 OUTEXT

      LOGICAL LSTAT
      LOGICAL TEXIST
      LOGICAL MEMB(1)

      COMMON/MEM/MEMD
      EQUIVALENCE (MEMI,MEMR,MEMD,MEMB)

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
C     Close down the first image
C
      CALL UIMCLO(IMDSCRI(1),STAT)
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
C     Get the name of the input table specifying 
C       Object name
C       X,Y pixel coordinates
C       Radius of object aperture
C       Radial distance between object circle and sky annulus
C       Radial width of sky annulus
C
500   CALL UCLGST('pointab',OSTNAM,STAT)
      CALL UTTACC(OSTNAM,TEXIST,STAT)
      ILEN=INDEX(OSTNAM,'   ') -1
      IF (.NOT.TEXIST) THEN
        WRITE(OUTEXT,171) OSTNAM(:ILEN) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UTTOPN(OSTNAM,1,OSTDSCR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,173) OSTNAM(:ILEN)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the number of rows in the table
C
      CALL UTPGTI(OSTDSCR,21,OSROWS,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,175) OSTNAM(:ILEN)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the data read from the
C     object position parameters table file for the 
C     arrays of X position (OBJPOX), Y position (OBJPOY),
C     object aperture radius (OBJRAD), the width of the
C     annulus between the sky aperture and the sky 
C     annulus (OSARAD) and the width of the sky annulus
C     (SKYRAD)
C
      CALL UDMGET(OSROWS,6,OBJPOX,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,OBJPOY,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,OBJRAD,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,OSARAD,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,SKYRAD,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,181)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,1,LOBJV,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,183)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     From the table read the X and Y positions of all
C     the object apertures, their radii, the separation of
C     the object circle and the sky annulus and the sky annular
C     width. Return the maximum no. of points in the sky 
C     annulus for subsequent array dimensioning
C
      CALL READ_OSTAB(OSTDSCR,OSROWS,OBJNAME,MEMR(OBJPOX),
     :                MEMR(OBJPOY),MEMR(OBJRAD),MEMR(OSARAD),
     :                MEMR(SKYRAD),MEMB(LOBJV),MNOSKY,
     :                LSTAT)
      IF (.NOT.LSTAT) THEN
        WRITE(OUTEXT,193) OSTNAM
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Close down table
C     
      CALL UTTCLO(OSTDSCR,STAT)
C
C     Get the number of iteration cycles for sigma
C     rejection on the mean sky level
C
      CALL UCLGSI('iterej',NREJS,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,611)
611     FORMAT(' Failed to get no. of sigma clip iterations
     : for mean sky')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the value of the factor times the sigma on the
C     mean sky to reject points for inclusion in the 
C     calculation of the mean sky
C
      CALL UCLGSR('sigsky',SKYREJ,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,621)
621     FORMAT(' Failed to get sigma rejection factor for mean sky')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
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
      CALL UDMGET(MNOSKY,6,HOLD1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,713)
713     FORMAT(' Unable to assign memory for internal 1-D',
     :' real array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(MNOSKY,6,HOLD2,STAT)
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
      CALL UDMGET(OSROWS,6,TINT,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,TINTER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,TQ,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,TQER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,TU,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,TUER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,TPOL,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,TPOLER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,TPA,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(OSROWS,6,TPAER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,715)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Call the grand subroutine to compute the object signals
C     in the apertures, the sky signals, the object - sky 
C     signals and the Stokes parameters I, Q and U, linear 
C     polarization (%), polarization position angle (degrees) 
C     and error images from the NOINI input object and error 
C     images
C
      CALL POLANH(N1,N2,NOINI,MEMR(OBJCUB),MEMR(ERRCUB),
     :            MEMR(THETA),MEMR(EFFI),MEMR(EFFQ),
     :            MEMR(EFFU),MEMR(REFANG),MEMR(INSPO),
     :            MEMR(INSPA),MEMR(ORIENT),MEMR(CORANG),
     :            PACOR,OSROWS,MEMR(OBJPOX),
     :            MEMR(OBJPOY),MEMR(OBJRAD),MEMR(OSARAD),
     :            MEMR(SKYRAD),NREJS,
     :            SKYREJ,MNOSKY,MEMR(HOLD1),MEMR(HOLD2),
     :            MEMR(HOLDT1),MEMR(HOLDT2),
     :            MEMR(HOLDE1),MEMR(HOLDE2),MEMR(HOLDE3),
     :            MEMI(HOLDQ),MEMD(HOLDX),MEMD(HOLDY),
     :            MEMD(HOLDZ),MEMD(HOLDF),MEMD(HOLDW),
     :            MEMR(TINT),MEMR(TINTER),MEMR(TQ),
     :            MEMR(TQER),MEMR(TU),MEMR(TUER),MEMR(TPOL),
     :            MEMR(TPOLER),MEMR(TPA),MEMR(TPAER))
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
C     Get the name for the output results table
C
800   CALL UCLGST('outtab',OUTTAB,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,803) OUTTAB
803     FORMAT(' Error getting name of output table ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 999
      ENDIF
      ILEN=INDEX(OUTTAB,'   ') - 1
C
C     Check this table doesn't already exist
C
      CALL UTTACC(OUTTAB,TEXIST,STAT1)
      IF (STAT1.EQ.0) THEN
        IF (TEXIST) THEN
          WRITE(OUTEXT,805) OUTTAB
805       FORMAT(' Table file ',A,' already exists')
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
      ENDIF
C
C     Initialize the new table
C
      CALL UTTINN(OUTTAB,OTDSCR,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,811) OUTTAB(:ILEN)
811     FORMAT(' Table file ',A,' could not be opened')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Set up the 13 column by OSROWS table 
C
      CALL TSETUP(OUTTAB,ILEN,OTDSCR,OSROWS,COLID,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,821) OUTTAB(:ILEN)
821     FORMAT(' Error setting up output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Create the new table
C
      CALL UTTCRE(OTDSCR,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,831) OUTTAB(:ILEN)
831     FORMAT(' Failed to create output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Put all the values into the columns of the output table
C
      CALL UTCPTT(OTDSCR,COLID(1),1,OSROWS,OBJNAME,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,850) OUTTAB(:ILEN)
850     FORMAT(' Failed to write column of OBJECT NAME to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF      
      CALL UTCPTR(OTDSCR,COLID(2),1,OSROWS,MEMR(OBJPOX),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,851) OUTTAB(:ILEN)
851     FORMAT(' Failed to write column of POS_X to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCPTR(OTDSCR,COLID(3),1,OSROWS,MEMR(OBJPOY),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,852) OUTTAB(:ILEN)
852     FORMAT(' Failed to write column of POS_Y to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCPTR(OTDSCR,COLID(4),1,OSROWS,MEMR(TINT),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,853) OUTTAB(:ILEN)
853     FORMAT(' Failed to write column of Stokes I to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCPTR(OTDSCR,COLID(5),1,OSROWS,MEMR(TINTER),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,854) OUTTAB(:ILEN)
854     FORMAT(' Failed to write column of Stokes I Error to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCPTR(OTDSCR,COLID(6),1,OSROWS,MEMR(TQ),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,855) OUTTAB(:ILEN)
855     FORMAT(' Failed to write column of Stokes Q to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCPTR(OTDSCR,COLID(7),1,OSROWS,MEMR(TQER),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,856) OUTTAB(:ILEN)
856     FORMAT(' Failed to write column of Stokes Q Error to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCPTR(OTDSCR,COLID(8),1,OSROWS,MEMR(TU),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,857) OUTTAB(:ILEN)
857     FORMAT(' Failed to write column of Stokes U to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCPTR(OTDSCR,COLID(9),1,OSROWS,MEMR(TUER),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,858) OUTTAB(:ILEN)
858     FORMAT(' Failed to write column of Stokes U Error to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCPTR(OTDSCR,COLID(10),1,OSROWS,MEMR(TPOL),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,859) OUTTAB(:ILEN)
859     FORMAT(' Failed to write column of Lin. Poln. to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCPTR(OTDSCR,COLID(11),1,OSROWS,MEMR(TPOLER),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,860) OUTTAB(:ILEN)
860     FORMAT(' Failed to write column of Lin. Poln. Error to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCPTR(OTDSCR,COLID(12),1,OSROWS,MEMR(TPA),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,861) OUTTAB(:ILEN)
861     FORMAT(' Failed to write column of Poln. PA to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCPTR(OTDSCR,COLID(13),1,OSROWS,MEMR(TPAER),STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,862) OUTTAB(:ILEN)
862     FORMAT(' Failed to write column of Poln. PA Error to
     : output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Close the output table
C
      CALL UTTCLO(OTDSCR,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,870) OUTTAB(:ILEN)
870     FORMAT(' Error closing output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Free the dynamic memory allocated for the result arrays
C
      CALL UDMFRE(TINT,6,STAT)
      CALL UDMFRE(TINTER,6,STAT)
      CALL UDMFRE(TQ,6,STAT)
      CALL UDMFRE(TQER,6,STAT)
      CALL UDMFRE(TU,6,STAT)
      CALL UDMFRE(TUER,6,STAT)
      CALL UDMFRE(TPOL,6,STAT)
      CALL UDMFRE(TPOLER,6,STAT)
      CALL UDMFRE(TPA,6,STAT)
      CALL UDMFRE(TPAER,6,STAT)
      GO TO 999

990   WRITE(OUTEXT,991)
991   FORMAT(' Program failed. No output')
      CALL UMSPUT(OUTEXT,1,0,STAT)

999   END

      SUBROUTINE READ_OSTAB(TBDSCR,NR,OBJNAME,OBJPOX,OBJPOY,
     :                      OBJRAD,OSARAD,SKYRAD,LFLAG,MNOSKY,
     :                      LSTAT)
C
C     This subroutine reads columns of the table file, whose descriptor 
C     is TBDSCR, into the following arrays:
C        column NAME to array OBJNAME
C        column POS_X to array OBJPOX
C        column POS_Y to array OBJPOY
C        column RADIUS to array OBJRAD
C        column GAP to array OSARAD
C        column ANNULUS to array SKYRAD
C     The maximum number of sky pixels is calculated based on the
C     width of the sky annulus.
C     If an error occurs reading values from the table then
C     LSTAT is set FALSE
C
      IMPLICIT NONE
      INTEGER TBDSCR ! Descriptor of table to be read
      INTEGER NR ! Number of rows in the table to be read
      INTEGER MNOSKY ! Maximum number of pixels in sky apertures

      REAL OBJPOX(NR) ! Array of X pixel coordinate of aperture centre
      REAL OBJPOY(NR) ! Array of Y pixel coordinate of aperture centre
      REAL OBJRAD(NR) ! Array of pixel radius of object aperture 
      REAL OSARAD(NR) ! Array of pixel width of object-sky gap annulus
      REAL SKYRAD(NR) ! Array of pixel width of sky annulus

      CHARACTER*32 OBJNAME(2048) ! Array of text for object description

      LOGICAL LFLAG(NR) ! Array of flags for status in reading table column values
      LOGICAL LSTAT ! FALSE if an error occurs in reading table column values
C
C     Local variables
C
      INTEGER I
      INTEGER COLID1
      INTEGER COLID2
      INTEGER COLID3
      INTEGER COLID4
      INTEGER COLID5
      INTEGER COLID6
      INTEGER STAT
      INTEGER STATO
      INTEGER SUMI

      REAL RAD1
      REAL RAD2
      REAL PIE
    
      CHARACTER*72 TEXT

      LSTAT=.TRUE.
      PIE=3.1415926535898
C
C     Match the column names one by one and read the values into 
C     appropriate arrays
C
C     Get column identifier for column OBJECT_NAME
C   
      CALL UTCFND(TBDSCR,'OBJECT_NAME',1,COLID1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,21)
21      FORMAT(' Failed to find column named  OBJECT_NAME ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array OBJNAME
C
      CALL UTCGTT(TBDSCR,COLID1,1,NR,OBJNAME,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,22)
22      FORMAT(' Failed to read values from column named  NAME ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column POS_X
C   
      CALL UTCFND(TBDSCR,'POS_X',1,COLID2,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,31)
31      FORMAT(' Failed to find column named  POS_X ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array OBJPOX
C
      CALL UTCGTR(TBDSCR,COLID2,1,NR,OBJPOX,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,32)
32      FORMAT(' Failed to read values from column named  POS_X ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column POS_Y
C   
      CALL UTCFND(TBDSCR,'POS_Y',1,COLID3,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,41)
41      FORMAT(' Failed to find column named  POS_Y ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array OBJPOY
C
      CALL UTCGTR(TBDSCR,COLID3,1,NR,OBJPOY,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,42)
42      FORMAT(' Failed to read values from column named  POS_Y ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column RADIUS
C   
      CALL UTCFND(TBDSCR,'RADIUS',1,COLID4,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,51)
51      FORMAT(' Failed to find column named  RADIUS ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array OBJRAD
C
      CALL UTCGTR(TBDSCR,COLID4,1,NR,OBJRAD,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,52)
52      FORMAT(' Failed to read values from column named  RADIUS ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column GAP
C   
      CALL UTCFND(TBDSCR,'GAP',1,COLID5,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,61)
61      FORMAT(' Failed to find column named  GAP ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array OSARAD
C
      CALL UTCGTR(TBDSCR,COLID5,1,NR,OSARAD,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,62)
62      FORMAT(' Failed to read values from column named  GAP ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Get column identifier for column ANNULUS
C   
      CALL UTCFND(TBDSCR,'ANNULUS',1,COLID6,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,71)
71      FORMAT(' Failed to find column named  ANNULUS ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Read all values for this column into array SKYRAD
C
      CALL UTCGTR(TBDSCR,COLID6,1,NR,SKYRAD,LFLAG,STAT)
      IF (STAT.NE.0) THEN
        WRITE(TEXT,72)
72      FORMAT(' Failed to read values from column named  ANNULUS ')
        CALL UMSPUT(TEXT,3,0,STATO)
        LSTAT=.FALSE.
      ENDIF
C
C     Find the maximum number of sky pixels in the sky annuli
C     (if sky annulus zero then set MNOSKY=1 for array size)
C
      SUMI=0
      MNOSKY=1
      DO I=1,NR,1
        IF (SKYRAD(I).GT.0.0) THEN
          RAD1=OBJRAD(I) + OSARAD(I) - 1.0
          RAD2=OBJRAD(I) + OSARAD(I) + SKYRAD(I) + 1.0
          SUMI=NINT(PIE*(RAD2*RAD2 - RAD1*RAD1))
          IF (SUMI.GT.MNOSKY) THEN
            MNOSKY=SUMI
          ENDIF
        ENDIF
      ENDDO

99    END

      SUBROUTINE TSETUP(OUTTAB,ILEN,TBDSCR,ROWS,COLID,RSTAT)
C
C     This subroutine sets up the output table -
C     13 columns, ROWS rows, column ordered
C     and labels the columns and gets the column 
C     identifiers
C
      IMPLICIT NONE
      INTEGER ILEN ! Character length of actual table name
      INTEGER TBDSCR ! Table descriptor
      INTEGER ROWS ! No. of rows for output table
      INTEGER RSTAT ! Return status flag - not 0 for set-up problem

      INTEGER COLID(13) ! Array of column identifiers

      CHARACTER*32 OUTTAB ! Name of output table

C     Local variables
C
      INTEGER STAT
      INTEGER STAT1
      INTEGER COLTYP(13) 

      CHARACTER*16 COLNAM(13)
      CHARACTER*12 COLUNIT(13)
      CHARACTER*5 COLFMT(13)
      CHARACTER*72 OUTEXT

      CALL UTPPTI(TBDSCR,7,13,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,121) OUTTAB(:ILEN)
121     FORMAT(' Error assigning columns to output table file ',
     :A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
      ENDIF
      CALL UTPPTI(TBDSCR,3,ROWS,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,123) OUTTAB(:ILEN)
123     FORMAT(' Error assigning rows to output table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
      ENDIF
      CALL UTPPTI(TBDSCR,5,12,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,125) OUTTAB(:ILEN)
125     FORMAT(' Error setting column ordering of output
     : table file ',A)
        CALL UMSPUT(OUTEXT,1,0,STAT)
      ENDIF
C
C     Define the columns
C
      COLNAM(1)='OBJECT_NAME'
      COLUNIT(1)=' '
      COLFMT(1)='A16'
      COLTYP(1)=-32

      COLNAM(2)='POS_X'
      COLUNIT(2)='Pixels'
      COLFMT(2)='F10.3'
      COLTYP(2)=6

      COLNAM(3)='POS_Y'
      COLUNIT(3)='Pixels'
      COLFMT(3)='F10.3'
      COLTYP(3)=6

      COLNAM(4)='Stoke_I'
      COLUNIT(4)=' '
      COLFMT(4)='E12.5'
      COLTYP(4)=6

      COLNAM(5)='Stoke_I_Error'
      COLUNIT(5)=' '
      COLFMT(5)='E12.5'
      COLTYP(5)=6

      COLNAM(6)='Stoke_Q'
      COLUNIT(6)='Normalised'
      COLFMT(6)='E12.5'
      COLTYP(6)=6

      COLNAM(7)='Stoke_Q_Error'
      COLUNIT(7)='Normalised'
      COLFMT(7)='E12.5'
      COLTYP(7)=6

      COLNAM(8)='Stoke_U'
      COLUNIT(8)='Normalised'
      COLFMT(8)='E12.5'
      COLTYP(8)=6

      COLNAM(9)='Stoke_U_Error'
      COLUNIT(9)='Normalised'
      COLFMT(9)='E12.5'
      COLTYP(9)=6

      COLNAM(10)='Lin._Poln.'
      COLUNIT(10)='%'
      COLFMT(10)='F8.4'
      COLTYP(10)=6

      COLNAM(11)='Lin._Poln._Error'
      COLUNIT(11)='%'
      COLFMT(11)='F8.4'
      COLTYP(11)=6

      COLNAM(12)='Poln._PA'
      COLUNIT(12)='Degrees'
      COLFMT(12)='F8.4'
      COLTYP(12)=6

      COLNAM(13)='Poln._PA_Error'
      COLUNIT(13)='Degrees'
      COLFMT(13)='F8.4'
      COLTYP(13)=6

      CALL UTCDEF(TBDSCR,COLNAM,COLUNIT,COLFMT,COLTYP,
     :            13,COLID,RSTAT)

99    END

      SUBROUTINE POLANH(N1,N2,N3,OBJCUB,ERRCUB,ANG,EFFI,EFFQ,
     :                  EFFU,REFANG,INSPO,INSPA,ORIENT,CORANG,
     :                  PAZERO,TROWS,OPOSX,OPOSY,
     :                  ORAD,OSARAD,SRAD,NREJ,SREJ,MSKY,
     :                  ARREJ1,ARREJ2,TOTO,TOTE,
     :                  HEFFI,HEFFQ,HEFFU,DQ,X,Y,DY,YF,WT,
     :                  STOI,STOIER,STOQ,STOQER,STOU,STOUER,
     :                  LP,LPER,TH,THER)
C
C     Calculates the total intensity (Stokes I), the linear 
C     polarization and polarization position angle and errors 
C     for the signal in specified apertures for the N3 object 
C     images and error images taken at polarizer angles 
C     specified by the values in the array ANG (in instrumental 
C     reference frame). The signal in the circular object 
C     apertures defined by pixel centres OPOX and OPOY and 
C     radius ORAD have the sky in the annulus ORAD+OSARAD to 
C     ORAD+OSARAD+SRAD subtracted with errors propagated. Sigma
C     clipping, specified by SREJ, of the mean sky is performed
C     in NREJ iterations.
C     The polarization and Stokes parameters are calculated using 
C     the instrumental efficiencies for Stokes I, Q and U given by 
C     EFFI, EFFQU and EFFUQ respectively and the instrumental 
C     polarization is also corrected. 
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
C       PAZERO   Position angle offset to correct PA 0
C       TROWS    No. of rows in parture arrays
C       OPOSX    1-D array of X pixel coordinates of objects
C       OPOSY    1-D array of Y pixel coordinates of objects
C       ORAD     1-D array of radius of object apertures
C       OSARAD   1-D array of width of gap object asperture to sky annulus
C       SRAD     1-D array of width of sky annulus
C       MSKY     Maximum number of points in sky annuli
C       NREJ     No. of sigma clipping cycles for computing mean sky
C       SREJ     Sigma*rms on sky mean for rejection 
C       ARRREJ   Holding array for sky sigma rejection cycles
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
C       M1       Dimension of output arrays
C       STOI     Output array of the object I Stokes parameter
C       STOIER   Output array of the object I Stokes parameter error
C       STOQ     Output array of the object Q Stokes parameter
C       STOQER   Output array of the object Q Stokes parameter error
C       STOU     Output array of the object U Stokes parameter
C       STOUER   Output array of the object U Stokes parameter error
C       LP       Output array of the object linear polarization (%)
C       LPER     Output array of the object polarization error (%)
C       TH       Output array of the object polarization position angle (degrees)
C       THER     Output array of the object position angle error (degrees)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N1
      INTEGER N2
      INTEGER N3
      INTEGER TROWS
      INTEGER NREJ
      INTEGER MSKY
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
      REAL PAZERO

      REAL OPOSX(TROWS)
      REAL OPOSY(TROWS)
      REAL ORAD(TROWS)
      REAL OSARAD(TROWS)
      REAL SRAD(TROWS)
      REAL SREJ
      REAL ARREJ1(MSKY)
      REAL ARREJ2(MSKY)

      REAL TOTO(N3)
      REAL TOTE(N3)
      REAL HEFFI(N3)
      REAL HEFFQ(N3)
      REAL HEFFU(N3)

      REAL STOI(TROWS)
      REAL STOIER(TROWS)
      REAL STOQ(TROWS)
      REAL STOQER(TROWS)
      REAL STOU(TROWS)
      REAL STOUER(TROWS)
      REAL LP(TROWS)
      REAL LPER(TROWS)
      REAL TH(TROWS)
      REAL THER(TROWS)

      DOUBLE PRECISION X(N3)
      DOUBLE PRECISION Y(N3)
      DOUBLE PRECISION DY(N3)
      DOUBLE PRECISION YF(N3)
      DOUBLE PRECISION WT(N3)
C
C     Local variables
C
      INTEGER I,J
      INTEGER STAT
      INTEGER INDX(3)
      INTEGER IFAIL
      INTEGER NIFAIL

      REAL INSTPO
      REAL INSTPA
      REAL LPLIM
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

      NIFAIL=0
C
C     Set instrumental polarization and position angle.
C     Need to correct position angle of instrumental 
C     polarization to absolute value on sky
C
      INSTPO=INSPO(1)/100.0 ! Fractional
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
C     For each object and sky aperture form the object-sky 
C     signal for the N3 input images and the error and compute 
C     the Stokes I, Q and U, linear polarization and position 
C     angle and errors
C
c      WRITE(TEXT,'(A)') '   I    J   TOTI    TOTIERR     Q    Q ERR'//
c     :'     U    U ERR    P(%)   PERR(%)  TH(DEG) THERR(DEG)'
c      CALL UMSPUT(TEXT,1,0,STAT)

      DO I=1,TROWS,1
C
C       Form the object sigal, the sky signal, the object-sky signal
C       and errors for each polarizer image and return the object-sky 
C       signal and error for each 
C
        DO J=1,N3,1
          TOTO(J)=0.0
          TOTE(J)=0.0
C
C           Form the total object-sky and error in the aperture
C           for the N3 input polarizer images. For the sky
C           perform NREJ iterations clipping points gretaer
C           than SREJ*rms on the mean
C
            CALL APEROS(N1,N2,N3,OBJCUB,ERRCUB,J,OPOSX(I),OPOSY(I),
     :                  ORAD(I),OSARAD(I),SRAD(I),NREJ,SREJ,MSKY,
     :                  ARREJ1,ARREJ2,TOTO(J),TOTE(J))

c            write(text,883) J,toto(J),tote(J)
c883         format('K,TOTO,TOTE ',I5,2(E12.5))
c            CALL UMSPUT(TEXT,1,0,STAT)

        ENDDO
C
C       Form the linear polarization and position angle and
C       errors for this bin
C
        IFAIL=0
        LPLIM=100.0 ! Huge value for maximum poln error 
        CALL POLCALF(N3,TOTO,TOTE,ANG,EFFI,EFFQ,EFFU,
     :               INSTPO,INSTPA,LPLIM,ARRI,COVI,INDX,
     :               HEFFI,HEFFQ,HEFFU,DQ,X,Y,DY,YF,WT,
     :               TOTI,TOTIER,TOTQ,TOTQER,TOTU,TOTUER,
     :               LPOL,LPOLER,LPA,LPAER,IFAIL)
        IF (IFAIL.NE.0) THEN
          NIFAIL=NIFAIL+1
        ENDIF
C
C       Write output values to result arrays
C
        STOI(I)=TOTI
        STOIER(I)=TOTIER
        STOQ(I)=TOTQ
        STOQER(I)=TOTQER
        STOU(I)=TOTU
        STOUER(I)=TOTUER
        LP(I)=LPOL
        LPER(I)=LPOLER
C
C       Set position angle, add zero offset and check for
C       negative values only for points with valid output data
C
        IF (LP(I).EQ.0.0.AND.LPER(I).EQ.0.0) THEN
          TH(I)=0.0 ! Not valid output
        ENDIF
        IF (LP(I).NE.0.0.AND.LPER(I).EQ.0.0) THEN
          TH(I)=LPA + PAZERO ! Valid output without errors
        ENDIF
        IF (LPER(I).NE.0.0) THEN
          TH(I)=LPA + PAZERO
        ENDIF
        IF (TH(I).LT.0.0) THEN
          TH(I)=180.0 + TH(I)
        ENDIF
        IF (TH(I).GT.180.0) THEN
          TH(I)=TH(I) - 180.0
        ENDIF
        THER(I)=LPAER
c        IF (LPOL.NE.0.0) THEN
c          WRITE(TEXT,77) TOTI,TOTIER,TOTQ,
c     :TOTQER,TOTU,TOTUER,LPOL,LPOLER,LPA,LPAER
c77        FORMAT(1X,E10.3,1X,F8.1,1X,4(1X,F6.3),1X,
c     :2(1X,F6.2),2X,F7.2,1X,F7.2)
c          CALL UMSPUT(TEXT,1,0,STAT)
c        ENDIF
      ENDDO
C
C     Report number of failures to calulate I, pol and theta for
C     least squares case
C
      IF (NIFAIL.GT.0) THEN
        WRITE(TEXT,990) NIFAIL
990     FORMAT(' Warning: ',I6,' failures occurred in least', 
     :' sqaures computation')
        CALL UMSPUT(TEXT,1,0,STAT)
      ENDIF

999   END

      SUBROUTINE APEROS(N1,N2,N3,OBJCUB,ERRCUB,NK,POSX,POSY,
     :                  ORAD,GAPRAD,SRAD,NREJ,SREJ,M1,
     :                  ARREJS,ARREJE,TOTO,TOTE)
C
C     This subroutine sums the counts in the NK'th plane of
C     the 3-D array OBJCUB inside the circular aperture with
C     centre POX,POY and radius ORAD (all in pixels). The
C     sky is summed in the annulus ORAD+GAPRAD to 
C     ORAD+GAPRAD+SRAD. If NREJ is greater than zero then 
C     NREJ sigma rejection iterations are performed excluding
C     points SREJ*rms on the mean from the value for the mean 
C     sky. The object - sky count is formed and the errors
C     propagated from the NK'th plane of the error aray ERRCUB.
C     The object-sky count and error are returned as TOTO and 
C     TOTE
C
      IMPLICIT NONE
      INTEGER N1 ! 1st dimension of input CUB arrays
      INTEGER N2 ! 2nd dimension of input CUB arrays
      INTEGER N3 ! 3rd dimension of input CUB arrays
      INTEGER NK ! Plane (3rd dimension) on which to determine aperture values
      INTEGER NREJ ! No. of sigma clip iterations
      INTEGER M1 ! Dimension of arrays ARREJS and ARREJE

      REAL OBJCUB(N1,N2,N3) ! Input 3 dimension array of input values
      REAL ERRCUB(N1,N2,N3) ! Input 3 dimension array of input errors
      REAL POSX ! X coordinate of object aperture centre
      REAL POSY ! Y coordinate of object aperture centre
      REAL ORAD ! Radius of object aperture 
      REAL GAPRAD ! Width of object-sky gap annulus
      REAL SRAD ! Width of sky annulus
      REAL SREJ ! Rejection factor (* rms) for rejection from mean sky
      REAL ARREJS(M1) ! Holding array for sky values in sigma rejection iteration
      REAL ARREJE(M1) ! Holding array for sky errors in sigma rejection iteration
      REAL TOTO ! Output total signal - mean sky in object aperture 
      REAL TOTE ! Output error on signal - mean sky in object aperture
C
C     Local variables
C
      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER L
      INTEGER LEFT
      INTEGER RIGHT
      INTEGER BOTT
      INTEGER TOP
      INTEGER NOB
      INTEGER NSK

      REAL OSUM
      REAL ESUM
      REAL RAD1
      REAL MEANS
      REAL ERROS

C
C     Form limits of region for object and sky
C
      LEFT=INT(POSX-ORAD-GAPRAD-SRAD) -1
      IF (LEFT.LT.0) THEN
        LEFT=1
      ENDIF
      RIGHT=NINT(POSX+ORAD+GAPRAD+SRAD) +1      
      IF (RIGHT.GT.N1) THEN
        RIGHT=N1
      ENDIF

      BOTT=INT(POSY-ORAD-GAPRAD-SRAD) -1
      IF (BOTT.LT.0) THEN
        BOTT=1
      ENDIF
      TOP=NINT(POSY+ORAD+GAPRAD+SRAD) +1      
      IF (TOP.GT.N2) THEN
        TOP=N2
      ENDIF
C
C     Step through this area summing the object counts, the 
C     erro in quadrature and copying the sky values to ARREJS 
C     and sky errors to ARREJE
C
      K=1
      L=1
      OSUM=0.0
      ESUM=0.0
      DO I=LEFT,RIGHT,1
        DO J=BOTT,TOP,1
          RAD1=SQRT( (REAL(I)-POSX)**2. + (REAL(J)-POSY)**2.)
          IF (RAD1.LE.ORAD) THEN
            OSUM=OSUM+OBJCUB(I,J,NK)
            ESUM=ESUM+(ERRCUB(I,J,NK)*ERRCUB(I,J,NK))
            L=L+1
          ENDIF
          IF (SRAD.GT.0.0) THEN
            IF (RAD1.LE.(ORAD+GAPRAD+SRAD).AND.
     :          RAD1.GT.(ORAD+GAPRAD)) THEN
              ARREJS(K)=OBJCUB(I,J,NK) 
              ARREJE(K)=ERRCUB(I,J,NK)*ERRCUB(I,J,NK)
              K=K+1
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      NOB=L-1
      NSK=K-1
C
C     Perform NREJ sigma rejection iterations for the sky mean value
C
      IF (NSK.GT.0) THEN
        CALL SSIGIT(NSK,ARREJS,ARREJE,NREJ,SREJ,MEANS,ERROS)
      ELSE
        MEANS=0.0
        ERROS=0.0
      ENDIF
C
C     Calculate the object-mean sky count and the propagated error
C 
      TOTO=OSUM - MEANS*REAL(NOB)
      TOTE=SQRT( ESUM + REAL(NOB)*ERROS )

99    END

      SUBROUTINE SSIGIT(N,ARRS,ARRE,NREJ,SIGREJ,MEANS,MEANE)
C
C     This subroutine computes the mean count (MEANS) in the 
C     array ARRS and the mean error**2 (MEANE) in the array 
C     ARRE with NREJ rejection cycles of points differing by 
C     more than SIGREJ*rms on the mean
C
      IMPLICIT NONE
      INTEGER N ! No. of values in input arrays ARRS and ARRE
      INTEGER NREJ ! No. of sigma clip iterations

      REAL ARRS(N) ! Input array of sky values
      REAL ARRE(N) ! Input array of sky error**2 values
      REAL SIGREJ ! Clip factor (* rms) for rejection from mean sky
      REAL MEANS ! Output mean sky value
      REAL MEANE ! Output variance on mean sky value
C
C     Local variables
C
      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER NUS
      INTEGER STAT

      REAL RMS

      CHARACTER*72 OUTEXT
C
C     Compute simple mean, mean variance and rms on mean
C
      CALL MEANER(N,ARRS,ARRE,MEANS,MEANE,RMS)

      IF (NREJ.EQ.0) THEN
        GO TO 90
      ENDIF

      NUS=N
      DO J=1,NREJ,1
        K=1
        DO I=1,NUS,1
          IF (ABS(ARRS(I)-MEANS).LT.(SIGREJ*RMS)) THEN
            ARRS(K)=ARRS(I)
            ARRE(K)=ARRE(I)
            K=K+1
          ENDIF
        ENDDO
        NUS=K-1
        CALL MEANER(NUS,ARRS,ARRE,MEANS,MEANE,RMS)
      ENDDO
     
90    IF (RMS.EQ.0.0.AND.N.GT.1) THEN
        WRITE(OUTEXT,95)
95      FORMAT(' WARNING: Rms on mean sky is zero')
        CALL UMSPUT(OUTEXT,1,0,STAT)
      ENDIF

99    END

      SUBROUTINE MEANER(N,ARRV,ARRE,MEANV,MEANE,RMS)
C
C     This subroutine returns the mean value (MEANV),
C     the variance (MEANE) and the simple rms on the 
C     mean for an array of values ARRS and variances 
C     ARRE
C
      IMPLICIT NONE
      INTEGER N ! No. of values in input arrays ARRV and ARRE
     
      REAL ARRV(N) ! Input array of values for mean
      REAL ARRE(N) ! Input array of variances (squared errors)
      REAL MEANV ! Output value of mean
      REAL MEANE ! Output value of variance
      REAL RMS ! Root mean square on MEANS
C
C     Local variables
C
      INTEGER I
      INTEGER ITOT

      REAL STOT
      REAL ETOT
      REAL RTOT
C
C     Compute simple mean and mean variance 
C
      STOT=0.0
      ETOT=0.0
      ITOT=1
      DO I=1,N,1
        STOT=STOT+ARRV(I)
        ETOT=ETOT+ARRE(I)
        ITOT=ITOT+1
      ENDDO
      IF ((ITOT-1).GT.0) THEN
        MEANV=STOT/REAL(ITOT-1)
        MEANE=ETOT/(REAL(ITOT-1)*REAL(ITOT-1))
      ELSE
        MEANV=0.0
        MEANE=0.0
        RMS=0.0
        GO TO 99
      ENDIF
C
C     Compute simple rms
C
      RTOT=0.0
      DO I=1,N,1
        RTOT=RTOT + (ARRV(I)-MEANV)**2.
      ENDDO
      RMS=SQRT(RTOT/REAL(ITOT-1))

99    END
