      SUBROUTINE NPLUCY
C
C     This FORTRAN program extracts any number of 1-D spectra 
C     of point sources from a 2-D longslit spectrum image and 
C     its statistical error and data quality images using an 
C     iterative scheme developed by Leon Lucy which restores 
C     simultaneously the spectra of the point sources and the 
C     spatial profile of the background. Each wavelength
C     increment is treated independently. The smoothness 
C     criterion for the background spatial restoration is given 
C     by a Gaussian. 
C     The PSF longslit spectrum image is required.
C
C     The 1-D spectrum and statistical error + data quality 
C     of each extracted spectrum are output as a table. Each 
C     separate source has its own table specified by a root
C     name with an extension _n. The output background 
C     spectrum, with or without the point source spectrum 
C     removed, together with error and data quality image, 
C     is also output. The errors on the extracted spectrum 
C     and fitted background can be formed by Monte Carlo 
C     trials. Data quality is handled through input and output 
C     for all files except the PSF image.
C
C     Written by:  J. R. Walsh, ST-ECF, ESO (jwalsh@eso.org) 
C                   First version: May 2001
C
      IMPLICIT NONE
      INTEGER I,J

      INTEGER STAT
      INTEGER STAT1
      INTEGER STAT2
      INTEGER STAT3
      INTEGER STAT4

      INTEGER DTYPE
      INTEGER NAXIS1
      INTEGER NAXIS2
      INTEGER NAXIS3
      INTEGER NAXIS4
      INTEGER N1
      INTEGER N2
      INTEGER E1
      INTEGER E2
      INTEGER D1
      INTEGER D2
      INTEGER P1
      INTEGER P2
      INTEGER NELEM

      INTEGER MEMI(1)

      INTEGER IMDSCR1
      INTEGER IMDSCR2
      INTEGER IMDSCR3
      INTEGER IMDSCR4
      INTEGER IMDSCR5
      INTEGER IMDSCR6
      INTEGER IMDSCR7
      INTEGER TDSCR
      INTEGER PDSCR

      INTEGER NROWS
      INTEGER NCOLS
      INTEGER NPO
      INTEGER NST
      INTEGER NST1
      INTEGER BY
      INTEGER COLID1
      INTEGER COLID2
      INTEGER COLID3
      INTEGER COLID4
      INTEGER COLID5
      INTEGER ILEN

      INTEGER DIMEN(7)
      INTEGER DATYP(7)
      INTEGER CDENT(7)
      INTEGER ODIMEN(7)

      INTEGER ICSUM
      INTEGER ICSTEP
      INTEGER NITER
      INTEGER INTYPE
      INTEGER NMONTE
      INTEGER ISEED
      INTEGER DQLIM

      INTEGER IMINP
      INTEGER ERRINP
      INTEGER DQINP
      INTEGER PSFINP
      INTEGER WI1
      INTEGER CENCX
      INTEGER CENCY
      INTEGER CENP1
      INTEGER CENP2
      INTEGER CENP3
      INTEGER CENT
      INTEGER SUBPX

      REAL MEMR(1)
      REAL PIST
      REAL LAMST
      REAL DELLAM
      REAL RCENT
      REAL RESKER
      REAL RVAR

      DOUBLE PRECISION EPSP
      DOUBLE PRECISION EPSB

      INTEGER POSPEC
      INTEGER WAV
      INTEGER POERR
      INTEGER PODQ
      INTEGER POISPEC
      INTEGER POIERR
      INTEGER POIDQ
      INTEGER IMBACK
      INTEGER IMBERR
      INTEGER IMBDQ
      INTEGER WR1
      INTEGER WR2

      DOUBLE PRECISION MEMD(1)

      INTEGER WD1
      INTEGER XYCEN
      INTEGER XFIT
      INTEGER YFIT
      INTEGER WD2
      INTEGER WD3
      INTEGER WD4
      INTEGER WD5
      INTEGER WD6
      INTEGER WD7
      INTEGER WD8
      INTEGER WD9
      INTEGER WD10
      INTEGER WD11
      INTEGER WD12
      INTEGER WD13
      INTEGER WD13A
      INTEGER WD14
      INTEGER WD15
      INTEGER WD16
      INTEGER WD17
      INTEGER WD18
      INTEGER WD19
      INTEGER WD20
      INTEGER WD21
      INTEGER WD22
      INTEGER WD23
      INTEGER WD24
      INTEGER WD25
      INTEGER WD26
      INTEGER WD27
      INTEGER WD28
      INTEGER WD29
      INTEGER WD30
      INTEGER WD31
      INTEGER WD32
      INTEGER WD33
      INTEGER WD34
      INTEGER WD35
      INTEGER WD36
      INTEGER WD37
      INTEGER WD38
      INTEGER WD39
      INTEGER WD40
      INTEGER WD41

      CHARACTER*1 PSMETH 
      CHARACTER*1 POMETH
      CHARACTER*24 IMANAM
      CHARACTER*24 ERRNAM
      CHARACTER*24 DQNAM
      CHARACTER*24 BAKIMA
      CHARACTER*24 BAKERR
      CHARACTER*24 BAKDQ
      CHARACTER*24 PSFNAM
      CHARACTER*24 INTERP
      CHARACTER*24 POITAB
      CHARACTER*24 ROONAM
      CHARACTER*24 TABNAM
      CHARACTER*20 COLNAM(4)
      CHARACTER*20 COLUNIT(4)
      CHARACTER*20 COLFMT(4)
      CHARACTER*132 OUTEXT

      LOGICAL MEMB(1)
      LOGICAL LEXIST
      LOGICAL NFLAG
      LOGICAL LNOERR
      LOGICAL LNODQ
      LOGICAL LMETH
      LOGICAL LPOBA
      LOGICAL LVERB
      LOGICAL TEXIST

      COMMON/MEM/MEMD
      EQUIVALENCE (MEMI,MEMR,MEMD,MEMB)
C
C     Get the name of the input 2-D spectral image to be restored
C
      CALL UCLGST('inpima',IMANAM,STAT)
C
C     Open the file and get the dimensions
C
10    CALL UIMOPN(IMANAM,1,IMDSCR1,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UIMGID(IMDSCR1,DTYPE,NAXIS1,DIMEN,STAT2)
        IF (NAXIS1.NE.2) THEN
          WRITE(OUTEXT,11) 
11        FORMAT(' Input image must be 2-dimensional')
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
        N1=DIMEN(1)
        N2=DIMEN(2)
C
C       Check that the second dimension is EVEN, if not abort
C
        IF (MOD(N2,2).NE.0) THEN
          CALL UMSPUT('! Image Y dimension must be even',
     :               1,0,STAT)
          GO TO 990
        ENDIF
C
C       Allocate dynamic memory for the 2-D input image
C       of dimensions N1 by N2 (real number)
C
        NELEM=N1*N2
        CALL UDMGET(NELEM,6,IMINP,STAT3)
        IF (STAT3.NE.0) THEN
          WRITE(OUTEXT,12) 
12       FORMAT(' Unable to assign memory for internal 2-D array')
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
        IF (STAT3.EQ.0) THEN
          CALL UIGS2R(IMDSCR1,1,N1,1,N2,MEMR(IMINP),STAT4)
          IF (STAT4.NE.0) THEN
            WRITE(OUTEXT,13) IMANAM
13          FORMAT(' Error reading data from file ',A24)
            CALL UMSPUT(OUTEXT,1,0,STAT)
            GO TO 990
          ENDIF
        ELSE
          WRITE(OUTEXT,14) IMANAM
14        FORMAT(' Error reading file ',A24)
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
      ELSE
        WRITE(OUTEXT,15) IMANAM
15      FORMAT(' Failed to open file ',A24)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Attempt to read the header parameters for the reference pixel,
C     wavelength start and increment. If succesful create a wavelength 
C     array. If no wavelength array created, use pixels numbers.
C
      CALL UHDGSR(IMDSCR1,'CRPIX1',PIST,STAT2)
      CALL UHDGSR(IMDSCR1,'CRVAL1',LAMST,STAT2)
      CALL UHDGSR(IMDSCR1,'CD1_1',DELLAM,STAT3)
C
C     Allocate dynamic memory for the 1-D wavelength array
C     of length N1 (double precision)
C
      CALL UDMGET(N1,7,WAV,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,16) 
16      FORMAT(' Unable to assign memory for internal 1-D array')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF

      CALL WAVCRE(N1,PIST,LAMST,DELLAM,MEMD(WAV))
C
C     Get the name of the input 2-D error image
C
20    CALL UCLGST('errima',ERRNAM,STAT)
      IF (ERRNAM(:1).EQ.' ') THEN
        LNOERR=.TRUE.
        E1=1
        E2=1
        GO TO 30
      ENDIF
      LNOERR=.FALSE.
C
C     Open the file and get the dimensions
C
      CALL UIMOPN(ERRNAM,1,IMDSCR2,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UIMGID(IMDSCR2,DTYPE,NAXIS2,DIMEN,STAT2)
        IF (NAXIS2.NE.2) THEN
          WRITE(OUTEXT,11) 
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
        E1=DIMEN(1)
        E2=DIMEN(2)
        IF (E1.NE.N1.OR.E2.NE.N2) THEN
          WRITE(OUTEXT,21) ERRNAM
21        FORMAT(' Error image not same size as data image ',A24)
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
C
C       Allocate dynamic memory for the 2-D input error image
C       of dimensions E1 by E2 (real number)
C
        NELEM=E1*E2
        CALL UDMGET(NELEM,6,ERRINP,STAT3)
        IF (STAT3.NE.0) THEN
          WRITE(OUTEXT,12) 
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
        IF (STAT3.EQ.0) THEN
          CALL UIGS2R(IMDSCR2,1,E1,1,E2,MEMR(ERRINP),STAT4)
          IF (STAT4.NE.0) THEN
            WRITE(OUTEXT,13) ERRNAM
            CALL UMSPUT(OUTEXT,1,0,STAT)
            GO TO 990
          ENDIF
        ELSE
          WRITE(OUTEXT,14) ERRNAM
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
      ELSE
        WRITE(OUTEXT,15) ERRNAM
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the name of the input 2-D data quality image
C
30    CALL UCLGST('dqima',DQNAM,STAT)
      IF (DQNAM(:1).EQ.' ') THEN
        LNODQ=.TRUE.
        D1=1
        D2=1
        DQLIM=2
        GO TO 50
      ENDIF
      LNODQ=.FALSE.
C
C     Open the file and get the dimensions
C
      CALL UIMOPN(DQNAM,1,IMDSCR3,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UIMGID(IMDSCR3,DTYPE,NAXIS3,DIMEN,STAT2)
        IF (NAXIS3.NE.2) THEN
          WRITE(OUTEXT,11) 
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
        D1=DIMEN(1)
        D2=DIMEN(2)
        IF (D1.NE.N1.OR.D2.NE.N2) THEN
          WRITE(OUTEXT,31) DQNAM
31        FORMAT(' Data quality image not same size as data image ',
     :A24)
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
C
C       Allocate dynamic memory for the 2-D input data quality image
C       of dimensions D1 by D2 (integer number)
C
        NELEM=D1*D2
        CALL UDMGET(NELEM,4,DQINP,STAT3)
        IF (STAT3.NE.0) THEN
          WRITE(OUTEXT,12) 
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
        IF (STAT3.EQ.0) THEN
          CALL UIGS2I(IMDSCR3,1,D1,1,D2,MEMI(DQINP),STAT4)
          IF (STAT4.NE.0) THEN
            WRITE(OUTEXT,13) DQNAM
            CALL UMSPUT(OUTEXT,1,0,STAT)
            GO TO 990
          ENDIF
        ELSE
          WRITE(OUTEXT,14) DQNAM
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
      ELSE
        WRITE(OUTEXT,15) DQNAM
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF

C     Get the data quality limit such that values with data quality less
C     than this value will be considered GOOD. This value is also the
C     fill value for the point sources table
C
50    CALL UCLGSI('dqlim',DQLIM,STAT)
C
C     Get the name of the input 2-D PSF image to match the PSF of
C     the spectrum image. 2nd dimension can be less than spectrum
C     image
C
      CALL UCLGST('psfima',PSFNAM,STAT)
C
C     Open the file and get the dimensions
C
      CALL UIMOPN(PSFNAM,1,IMDSCR4,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UIMGID(IMDSCR4,DTYPE,NAXIS4,DIMEN,STAT2)
        IF (NAXIS4.NE.2) THEN
          WRITE(OUTEXT,11) 
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
        P1=DIMEN(1)
        P2=DIMEN(2)
C
C       Check PSF image size is equal to X size of input data image
C       and less than or equal to Y size input data image
C
        IF (P1.NE.N1) THEN
          WRITE(OUTEXT,51) PSFNAM
51        FORMAT(' PSF spectrum X image size does not match ',
     :           'data image ',A24)
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
        IF (P2.GT.N2) THEN
          WRITE(OUTEXT,53) PSFNAM
53        FORMAT(' PSF spectrum Y image size not compatible with ',
     :           'data image ',A24) 
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
C
C       Allocate dynamic memory for the 2-D input PSF image
C       of dimensions P1 by P2 (real number)
C
        NELEM=P1*P2
        CALL UDMGET(NELEM,6,PSFINP,STAT3)
        IF (STAT3.NE.0) THEN
          WRITE(OUTEXT,12) 
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
        IF (STAT3.EQ.0) THEN
          CALL UIGS2R(IMDSCR4,1,P1,1,P2,MEMR(PSFINP),STAT4)
          IF (STAT4.NE.0) THEN
            WRITE(OUTEXT,13) PSFNAM
            CALL UMSPUT(OUTEXT,1,0,STAT)
            GO TO 990
          ENDIF
        ELSE
          WRITE(OUTEXT,14) PSFNAM
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
      ELSE
        WRITE(OUTEXT,15) PSFNAM
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMCLO(IMDSCR4,STAT)
C
C     Get the name of the table file listing the reference X
C     and Y positions, the first, second and third order 
C     polynomial fit terms to compute the Y positions as a 
C     function of the X position of the point sources whose 
C     spectra are required
C
      CALL UCLGST('poitab',POITAB,STAT)
      CALL UTTACC(POITAB,LEXIST,STAT)
      IF (.NOT.LEXIST) THEN
        WRITE(OUTEXT,212) POITAB
212     FORMAT(' Table file ',A24,' does not exist')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Open the table file, get the number of rows and 
C     columns
C
      CALL UTTOPN(POITAB,1,PDSCR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,211) POITAB
211     FORMAT(' Failed to open table file ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UTPGTI(PDSCR,22,NCOLS,STAT1) ! No. of columns
      CALL UTPGTI(PDSCR,21,NROWS,STAT1) ! No. of rows
C
C     Check that number of columns is at least five
C     (reference X, Y, 1st, 2nd and 3rd order terms) else 
C     exit
C
      IF (NCOLS.LT.5) THEN
        WRITE(OUTEXT,213) POITAB
213     FORMAT(' Less than 5 columns in table file ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the 1-D arrays
C     of point source reference X, reference Y, 1st, 2nd
C     and 3rd order terms with respect to X and calculated 
C     Y centre (all real) 
C
      CALL UDMGET(NROWS,6,CENCX,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,16) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,CENCY,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,16) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,CENP1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,16) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,CENP2,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,16) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,CENP3,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,16) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NROWS,6,CENT,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,16) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCNUM(PDSCR,1,COLID1,STAT1) ! Column ID for column 1
      CALL UTCNUM(PDSCR,2,COLID2,STAT1) ! Column ID for column 2
      CALL UTCNUM(PDSCR,3,COLID3,STAT1) ! Column ID for column 3
      CALL UTCNUM(PDSCR,4,COLID4,STAT1) ! Column ID for column 4
      CALL UTCNUM(PDSCR,5,COLID5,STAT1) ! Column ID for column 5
C
C     Read each row value of first column into array CENCX
C
      J=1
      DO I=1,NROWS,1
        CALL UTRGTR(PDSCR,COLID1,1,I,RCENT,NFLAG,STAT1)
        IF (STAT1.EQ.0) THEN
          IF (.NOT.NFLAG) THEN
            CALL TCOPUT(RCENT,NROWS,J,MEMR(CENCX))
            J=J+1
          ENDIF
        ENDIF
      ENDDO
      NPO=J-1
C
C     Read each row value of second column into array CENCY
C
      J=1
      DO I=1,NROWS,1
        CALL UTRGTR(PDSCR,COLID2,1,I,RCENT,NFLAG,STAT1)
        IF (STAT1.EQ.0) THEN
          IF (.NOT.NFLAG) THEN
            CALL TCOPUT(RCENT,NROWS,J,MEMR(CENCY))
            J=J+1
          ENDIF
        ENDIF
      ENDDO
C
C     Read each row value of third column into array CENP1
C
      J=1
      DO I=1,NROWS,1
        CALL UTRGTR(PDSCR,COLID3,1,I,RCENT,NFLAG,STAT1)
        IF (STAT1.EQ.0) THEN
          IF (.NOT.NFLAG) THEN
            CALL TCOPUT(RCENT,NROWS,J,MEMR(CENP1))
            J=J+1
          ENDIF
        ENDIF
      ENDDO
C
C     Read each row value of fourth column into array CENP2
C
      J=1
      DO I=1,NROWS,1
        CALL UTRGTR(PDSCR,COLID4,1,I,RCENT,NFLAG,STAT1)
        IF (STAT1.EQ.0) THEN
          IF (.NOT.NFLAG) THEN
            CALL TCOPUT(RCENT,NROWS,J,MEMR(CENP2))
            J=J+1
          ENDIF
        ENDIF
      ENDDO
C
C     Read each row value of fifth column into array CENP3
C
      J=1
      DO I=1,NROWS,1
        CALL UTRGTR(PDSCR,COLID5,1,I,RCENT,NFLAG,STAT1)
        IF (STAT1.EQ.0) THEN
          IF (.NOT.NFLAG) THEN
            CALL TCOPUT(RCENT,NROWS,J,MEMR(CENP3))
            J=J+1
          ENDIF
        ENDIF
      ENDDO
      CALL UTTCLO(PDSCR,STAT)
C
C     Determine the number of point source spectra in the Y 
C     range of the input spectral image, NST, and the first 
C     source fully above the 2nd row, NST1
C
      CALL POILIM(NPO,MEMR(CENCX),MEMR(CENCY),MEMR(CENP1),
     :            MEMR(CENP2),MEMR(CENP3),MEMR(CENT),N1,N2,
     :            NST,NST1)
      IF (NST.EQ.0) THEN
        WRITE(OUTEXT,221) 
221     FORMAT(' No point source spectra in range of input image')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the method for fitting the PSF to determine its
C     centre position
C     C for centroid; G for Gaussian
C
      CALL UCLGST('psfmeth',PSMETH,STAT)
C
C     Check that value of PSFMETH is C or G
C
      LMETH=.FALSE.
      IF (PSMETH.EQ.'C'.OR.PSMETH.EQ.'c') THEN
        LMETH=.TRUE.
      ENDIF
      IF (PSMETH.EQ.'G'.OR.PSMETH.EQ.'g') THEN
        LMETH=.TRUE.
      ENDIF
      IF (.NOT.LMETH) THEN
        CALL UMSPUT('! Invalid value for psfmeth ',1,0,STAT)
        GO TO 990
      ENDIF
C
C     Determine whether the centre positions of the point sources
C     are to be determined by cross correlation or given exactly by
C     the values in the table POIFIT (C or E)
C
      CALL UCLGST('posmeth',POMETH,STAT)
C
C     Check that value of POMETH is C or E
C
      LMETH=.FALSE.
      IF (POMETH.EQ.'C'.OR.POMETH.EQ.'c') THEN
        LMETH=.TRUE.
      ENDIF
      IF (POMETH.EQ.'E'.OR.POMETH.EQ.'e') THEN
        LMETH=.TRUE.
      ENDIF
      IF (.NOT.LMETH) THEN
        CALL UMSPUT('! Invalid value for posmeth ',1,0,STAT)
        GO TO 990
      ENDIF
C
C     Enter the number of columns to sum for the data array for 
C     computing the cross correlation with the PSF
C 
      CALL UCLGSI('icsum',ICSUM,STAT)
      ICSTEP=ICSUM
C
C     Get the sigma of the resolution kernel for the restoration
C     of the background channel. 
C
      CALL UCLGSR('skernel',RESKER,STAT)
C
C     Enter the number of iterations for the 
C     spatial restoration of point sources and background
C 
      CALL UCLGSI('niter',NITER,STAT)
C
C     Enter the fractional increment per iteration 
C     for stopping restoration of the point source 
C     spectra
C
      CALL UCLGSR('epspoi',RVAR,STAT)
      EPSP=DBLE(RVAR)
C
C     Enter the fractional increment per iteration 
C     for stopping restoration of the background 
C     spatial profile
C
      CALL UCLGSR('epsbac',RVAR,STAT)
      EPSB=DBLE(RVAR)
C
C     Get the interpolation type for the shifted PSF's
C
      CALL UCLGST('interpol',INTERP,STAT)
      IF (INTERP.EQ.'nearest') THEN
        INTYPE=1
      ELSE IF (INTERP.EQ.'linear') THEN
        INTYPE=2
      ELSE IF (INTERP.EQ.'poly3') THEN
        INTYPE=3
      ELSE IF (INTERP.EQ.'poly5') THEN
        INTYPE=4
      ELSE IF (INTERP.EQ.'spline3') THEN
        INTYPE=5
      ELSE
        CALL UMSPUT('! Invalid interpolant specified',
     :               1,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the sub-sampling factor for the PSF image 
C
      CALL UCLGSI('psfb',BY,STAT)
C
C     Verify PSF subsampling factor is sensible
C
      IF (BY.LT.1) THEN
        CALL UMSPUT('! Invalid PSF blocking factor',1,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the sub-sampling factor for interpolation
C     in transforming the spectra from the observed
C     to the rectified grid
C
c      CALL UCLGSI('subpix',SUBPX,STAT)
      SUBPX=1
C
C     Get the number of Monte Carlo trials to perform for
C     error estimation
C
      CALL UCLGSI('ntrial',NMONTE,STAT)
      IF (NMONTE.LE.1) THEN
        NMONTE=1
        LNOERR=.TRUE. ! Even if errors available        
      ENDIF
      IF (LNOERR.AND.(NMONTE.GT.1)) THEN
        WRITE(OUTEXT,225)
225     FORMAT('! Multiple trials not possible without error array')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the seed for the random number generator
C
      CALL UCLGSI('seed',ISEED,STAT)
C
C     Get the switch to determine if the background is output 
C     or the background + point source (TRUE=point sources +
C     background; FALSE=background only)
C
      CALL UCLGSB('poi_back',LPOBA,STAT)
C
C     Get the reporting level for the running of the 
C     two channel restoration
C
      CALL UCLGSB('verbose',LVERB,STAT)
C
C     Allocate dynamic memory for the arrays 
C     required for storage and workspace for the 
C     two channel Lucy spectral-spatial restoration
C
      CALL UDMGET(NST,4,WI1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,231) 
231     FORMAT(' Unable to assign memory for internal 1-D ',
     :         'integer array')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Now real arrays
C
      CALL UDMGET(P2,6,WR1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,231) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=P1*P2*NST
      CALL UDMGET(NELEM,6,WR2,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,233) 
233     FORMAT(' Unable to assign memory for internal 3-D ',
     :        'real array')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Now double precision arrays
C
      CALL UDMGET(N2,7,WD1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
235     FORMAT(' Unable to assign memory for internal 1-D ',
     :         'double precision array')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*N2
      CALL UDMGET(NELEM,7,XYCEN,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,237) 
237     FORMAT(' Unable to assign memory for internal 2-D ',
     :         'double precision array')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*N2
      CALL UDMGET(NELEM,7,XFIT,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,237) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*N2
      CALL UDMGET(NELEM,7,YFIT,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,237) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*N2
      CALL UDMGET(NELEM,7,WD2,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,237) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=P1*P2
      CALL UDMGET(NELEM,7,WD3,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,237) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(P1,7,WD4,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(P1,7,WD5,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(P1,7,WD6,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(P1,7,WD7,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(P1,7,WD8,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N2,7,WD9,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*NST
      CALL UDMGET(NELEM,7,WD10,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,237) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*N2
      CALL UDMGET(NELEM,7,WD11,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,237) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N2,7,WD12,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*NST
      CALL UDMGET(NELEM,7,WD13,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*NST
      CALL UDMGET(NELEM,7,WD13A,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*NST
      CALL UDMGET(NELEM,7,WD14,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N1,7,WD15,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*N2
      CALL UDMGET(NELEM,7,WD16,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*N2
      CALL UDMGET(NELEM,7,WD17,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,237) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N2,7,WD18,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,237) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N2,7,WD19,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N2,7,WD20,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N2*NST
      CALL UDMGET(NELEM,7,WD21,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N2,7,WD22,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,237) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N2,7,WD23,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N2*2
      CALL UDMGET(NELEM,7,WD24,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*N2*2
      CALL UDMGET(NELEM,7,WD25,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*N2
      CALL UDMGET(NELEM,7,WD26,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N2,7,WD27,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N2,7,WD28,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NST,7,WD29,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*N2
      CALL UDMGET(NELEM,7,WD30,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*NST
      CALL UDMGET(NELEM,7,WD31,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N2,7,WD32,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N2,7,WD33,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N2*2
      CALL UDMGET(NELEM,7,WD34,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,7,WD35,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,7,WD36,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,7,WD37,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,7,WD38,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,7,WD39,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,235) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*NST
      CALL UDMGET(NELEM,7,WD40,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,237) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      NELEM=N1*N2
      CALL UDMGET(NELEM,7,WD41,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,237) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the output 2-D arrays
C     of point source flux, error and data quality
C
250   NELEM=N1*NST
      CALL UDMGET(NELEM,6,POSPEC,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,251) 
251     FORMAT(' Unable to assign memory for output 2-D ',
     :         'real array')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,POERR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,251) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,4,PODQ,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,252) 
252     FORMAT(' Unable to assign memory for internal 2-D ',
     :         'integer array')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the output 2-D images
C     of background flux, error and data quality
C
260   NELEM=N1*N2
      CALL UDMGET(NELEM,6,IMBACK,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,261) 
261     FORMAT(' Unable to assign memory for output 2-D ',
     :         'real array')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,IMBERR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,261) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,4,IMBDQ,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,262) 
262     FORMAT(' Unable to assign memory for output 2-D ',
     :         'integer array')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Call the subroutine to perform the restoration of the 
C     input point source spectra and the background. The 
C     output image consists of the fitted background or the 
C     fitted background plus the point source(s).
C
      CALL POPNPL(N1,N2,MEMD(WAV),MEMR(IMINP),E1,E2,MEMR(ERRINP),
     :     D1,D2,MEMI(DQINP),LNOERR,LNODQ,P1,P2,MEMR(PSFINP),
     :     BY,MEMD(XYCEN),MEMD(XFIT),MEMD(YFIT),NPO,NST,NST1,
     :     MEMR(CENCX),MEMR(CENCY),MEMR(CENP1),MEMR(CENP2),
     :     MEMR(CENP3),MEMR(CENT),PSMETH,POMETH,ICSTEP,RESKER,
     :     NITER,EPSP,EPSB,INTYPE,SUBPX,NMONTE,ISEED,DQLIM,
     :     MEMR(WR1),MEMD(WD1),MEMD(WD2),MEMD(WD3),
     :     MEMD(WD4),MEMD(WD5),MEMD(WD6),MEMD(WD7),
     :     MEMD(WD8),MEMD(WD9),MEMD(WD10),MEMR(WR2),MEMI(WI1),
     :     MEMD(WD11),MEMD(WD12),MEMD(WD13),MEMD(WD13A),
     :     MEMD(WD14),MEMD(WD15),MEMD(WD16),MEMD(WD17),
     :     MEMD(WD18),MEMD(WD19),MEMD(WD20),MEMD(WD21),
     :     MEMD(WD22),MEMD(WD23),MEMD(WD24),MEMD(WD25),
     :     MEMD(WD26),MEMD(WD27),MEMD(WD28),
     :     MEMD(WD29),MEMD(WD30),MEMD(WD31),MEMD(WD32),
     :     MEMD(WD33),MEMD(WD34),MEMD(WD35),MEMD(WD36),
     :     MEMD(WD37),MEMD(WD38),MEMD(WD39),MEMD(WD40),
     :     MEMD(WD41),LVERB,MEMR(POSPEC),
     :     MEMR(POERR),MEMI(PODQ),LPOBA,MEMR(IMBACK),
     :     MEMR(IMBERR),MEMI(IMBDQ))
C
C     Free up some of the allocated dynamic memory
C
      CALL UDMFRE(IMINP,6,STAT)
      CALL UDMFRE(ERRINP,6,STAT)
      IF (.NOT.LNODQ) THEN
        CALL UDMFRE(DQINP,4,STAT)
      ENDIF
      CALL UDMFRE(PSFINP,6,STAT)
      CALL UDMFRE(CENCX,6,STAT)
      CALL UDMFRE(CENCY,6,STAT)
      CALL UDMFRE(CENP1,6,STAT)
      CALL UDMFRE(CENP2,6,STAT)
      CALL UDMFRE(CENP3,6,STAT)
      CALL UDMFRE(CENT,6,STAT)
      CALL UDMFRE(WI1,4,STAT)
      CALL UDMFRE(WR1,6,STAT)
      CALL UDMFRE(XYCEN,7,STAT)
      CALL UDMFRE(XFIT,7,STAT)
      CALL UDMFRE(YFIT,7,STAT)
      CALL UDMFRE(WD1,7,STAT)
      CALL UDMFRE(WD2,7,STAT)
      CALL UDMFRE(WD3,7,STAT)
      CALL UDMFRE(WD4,7,STAT)
      CALL UDMFRE(WD5,7,STAT)
      CALL UDMFRE(WD6,7,STAT)
      CALL UDMFRE(WD7,7,STAT)
      CALL UDMFRE(WD8,7,STAT)
      CALL UDMFRE(WD9,7,STAT)
      CALL UDMFRE(WD10,7,STAT)
      CALL UDMFRE(WR2,6,STAT)
      CALL UDMFRE(WD11,7,STAT)
      CALL UDMFRE(WD12,7,STAT)
      CALL UDMFRE(WD13,7,STAT)
      CALL UDMFRE(WD13A,7,STAT)
      CALL UDMFRE(WD14,7,STAT)
      CALL UDMFRE(WD15,7,STAT)
      CALL UDMFRE(WD16,7,STAT)
      CALL UDMFRE(WD17,7,STAT)
      CALL UDMFRE(WD18,7,STAT)
      CALL UDMFRE(WD19,7,STAT)
      CALL UDMFRE(WD19,7,STAT)
      CALL UDMFRE(WD20,7,STAT)
      CALL UDMFRE(WD21,7,STAT)
      CALL UDMFRE(WD22,7,STAT)
      CALL UDMFRE(WD23,7,STAT)
      CALL UDMFRE(WD24,7,STAT)
      CALL UDMFRE(WD25,7,STAT)
      CALL UDMFRE(WD26,7,STAT)
      CALL UDMFRE(WD27,7,STAT)
      CALL UDMFRE(WD28,7,STAT)
      CALL UDMFRE(WD29,7,STAT)
      CALL UDMFRE(WD30,7,STAT)
      CALL UDMFRE(WD31,7,STAT)
      CALL UDMFRE(WD32,7,STAT)
      CALL UDMFRE(WD33,7,STAT)
      CALL UDMFRE(WD34,7,STAT)
      CALL UDMFRE(WD35,7,STAT)
      CALL UDMFRE(WD36,7,STAT)
      CALL UDMFRE(WD37,7,STAT)
      CALL UDMFRE(WD38,7,STAT)
      CALL UDMFRE(WD39,7,STAT)
      CALL UDMFRE(WD40,7,STAT)
      CALL UDMFRE(WD41,7,STAT)
C
C     Set the arrays for the output table column names
C
300   COLNAM(1)='WAVELENGTH'
      COLNAM(2)='FLUX'
      COLNAM(3)='STATISTICAL_ERROR'
      COLNAM(4)='DATA_QUALITY'
      COLUNIT(1)='Angstroms'
      COLUNIT(2)='Erg/s/cm**2/Angstrom'
      COLUNIT(3)='Erg/s/cm**2/Angstrom'
      COLUNIT(4)='   '
      COLFMT(1)='25.16g'
      COLFMT(2)='15.7g'
      COLFMT(3)='15.7g'
      COLFMT(4)='I4'
      DATYP(1)=7 ! Double Precision Wavelength
      DATYP(2)=6 ! Real flux
      DATYP(3)=6 ! Real flux error
      DATYP(4)=4 ! Integer data quality
C
C     Allocate dynamic memory for the three 1-D arrays
C     for the point source flux, error and data quality
C     for writing out to the output table columns
C
      CALL UDMGET(N1,6,POISPEC,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,303) 
303       FORMAT(' Unable to assign memory for internal 1-D ',
     :           'real array')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N1,6,POIERR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,303) 
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(N1,4,POIDQ,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,305) 
305     FORMAT(' Unable to assign memory for internal 1-D ',
     :           'integer array')
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     For each point source spectrum write out a table file of
C     the wavelengths, the total flux in the point source, the 
C     error and the data quality. The running number affix in the
C     name reflects the number of the point source in the 
C     input table
C
      CALL UCLGST('Rootab',ROONAM,STAT)
      DO I=1,NST,1
        CALL LISNAM(ROONAM,I+NST1-1,TABNAM)
C     
C       Open a table file to list the wavelengths, the total flux
C       in the point source, the error and the data quality
C
        ILEN=INDEX(TABNAM,'   ') -1
        CALL UTTACC(TABNAM,TEXIST,STAT)
        IF (TEXIST) THEN
          WRITE(OUTEXT,307) TABNAM(:ILEN)
307       FORMAT(' Error writing table. ',A,' already exists')
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 330
        ENDIF
        CALL UTTINN(TABNAM,TDSCR,STAT1)
        CALL UTPPTI(TDSCR,3,N1,STAT2)
        CALL UTCDEF(TDSCR,COLNAM,COLUNIT,COLFMT,DATYP,4,CDENT,STAT3)
        IF (STAT1.EQ.0.AND.STAT2.EQ.0.AND.STAT3.EQ.0) THEN
          CALL UTTCRE(TDSCR,STAT)
        ELSE
          WRITE(OUTEXT,311) TABNAM
311       FORMAT(' Failed to open table file ',A24)
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 330
        ENDIF
C
C       Copy the relevant section of the 2-D arrays of point 
C       source spectrum, error and data quality to 1-D holding
C       arrays for writing out
C
        CALL OUT2T1(N1,NST,I,MEMR(POSPEC),MEMR(POISPEC),
     :              MEMR(POERR),MEMR(POIERR),MEMI(PODQ),
     :              MEMI(POIDQ))
C
C       Put the wavelength, flux, statistical error and data quality
C       values for the extracted point source into the columns of the 
C       table        
C     
        CALL UTCPTD(TDSCR,CDENT(1),1,N1,MEMD(WAV),STAT1)
        IF (STAT1.NE.0) THEN
          WRITE(OUTEXT,312) TABNAM
312       FORMAT(' Failed to copy wavelengths to table file ',A24)
          CALL UMSPUT(OUTEXT,1,0,STAT)
        ENDIF 
        CALL UTCPTR(TDSCR,CDENT(2),1,N1,MEMR(POISPEC),STAT1)
        IF (STAT1.NE.0) THEN
          WRITE(OUTEXT,313) TABNAM
313       FORMAT(' Failed to copy fluxes to table file ',A24)
          CALL UMSPUT(OUTEXT,1,0,STAT)
        ENDIF 
        CALL UTCPTR(TDSCR,CDENT(3),1,N1,MEMR(POIERR),STAT1)
        IF (STAT1.NE.0) THEN
          WRITE(OUTEXT,314) TABNAM
314       FORMAT(' Failed to copy errors to table file ',A24)
          CALL UMSPUT(OUTEXT,1,0,STAT)
        ENDIF 
        CALL UTCPTI(TDSCR,CDENT(4),1,N1,MEMI(POIDQ),STAT1)
        IF (STAT1.NE.0) THEN
          WRITE(OUTEXT,315) TABNAM
315       FORMAT(' Failed to copy data quality to table file ',A24)
          CALL UMSPUT(OUTEXT,1,0,STAT)
        ENDIF 
        CALL UTTCLO(TDSCR,STAT)        
330     CONTINUE
      ENDDO
C
C     Open a new file for the fitted background or background
C     + point sources image and write IMBACK
C     into it
C
400   CALL UCLGST('bakima',BAKIMA,STAT)
      ODIMEN(1)=N1
      ODIMEN(2)=N2
      CALL UIMCRE(BAKIMA,6,2,ODIMEN,IMDSCR5,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCR1,IMDSCR5,STAT2)
        CALL UIPS2R(IMDSCR5,1,N1,1,N2,MEMR(IMBACK),STAT3)
        IF (STAT3.NE.0) THEN
          WRITE(OUTEXT,411) BAKIMA
411       FORMAT(' Error writing data to file ',A24)
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCR5,STAT4)
      ELSE
        WRITE(OUTEXT,412) BAKIMA
412     FORMAT(' Failed to open data file ',A24)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Open a new file for the fitted background or background
C     + point sources image error map and write the array 
C     IMBERR into it
C
430   IF (LNOERR) THEN
        GO TO 460
      ENDIF
      CALL UCLGST('bakerr',BAKERR,STAT)
      CALL UIMCRE(BAKERR,6,2,ODIMEN,IMDSCR6,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCR2,IMDSCR6,STAT2)
        CALL UIPS2R(IMDSCR6,1,N1,1,N2,MEMR(IMBERR),STAT3)
        IF (STAT3.NE.0) THEN
          WRITE(OUTEXT,431) BAKERR
431       FORMAT(' Error writing error data to file ',A24)
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCR6,STAT4)
      ELSE
        WRITE(OUTEXT,432) BAKERR
432     FORMAT(' Failed to open error file ',A24)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Open a new file for the fitted background or background
C     + point sources image data quality map and write the array 
C     IMBDQ into it
C
460   IF (LNODQ) THEN
        GO TO 900
      ENDIF
      CALL UCLGST('bakdq',BAKDQ,STAT)
      CALL UIMCRE(BAKDQ,6,2,ODIMEN,IMDSCR7,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCR3,IMDSCR7,STAT2)
        CALL UIPS2I(IMDSCR7,1,N1,1,N2,MEMI(IMBDQ),STAT3)
        IF (STAT3.NE.0) THEN
          WRITE(OUTEXT,461) BAKDQ
461       FORMAT(' Error writing data quality to file ',A24)
          CALL UMSPUT(OUTEXT,1,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCR7,STAT4)
      ELSE
        WRITE(OUTEXT,462) BAKDQ
462     FORMAT(' Failed to open data quality file ',A24)
        CALL UMSPUT(OUTEXT,1,0,STAT)
        GO TO 990
      ENDIF
C
C     Close the files left open for copying of headers
C
900   CALL UIMCLO(IMDSCR1,STAT)
      IF (.NOT.LNOERR) THEN
        CALL UIMCLO(IMDSCR2,STAT)
      ENDIF
      IF (.NOT.LNODQ) THEN
        CALL UIMCLO(IMDSCR3,STAT)
      ENDIF
C
C     Free up the allocated dynamic memory
C
      CALL UDMFRE(POSPEC,6,STAT)
      CALL UDMFRE(POERR,6,STAT)
      CALL UDMFRE(PODQ,4,STAT)
      CALL UDMFRE(POISPEC,6,STAT)
      CALL UDMFRE(POIERR,6,STAT)
      CALL UDMFRE(POIDQ,4,STAT)
      CALL UDMFRE(IMBACK,6,STAT)
      CALL UDMFRE(IMBERR,6,STAT)
      CALL UDMFRE(IMBDQ,4,STAT)
      GO TO 999

990   OUTEXT=' Program failed. No output written'
      CALL UMSPUT(OUTEXT,1,0,STAT)

999   END

      SUBROUTINE POPNPL(N1,N2,XIN,IMAG,E1,E2,ERRIM,D1,D2,DQIM,
     :                  LNOERR,LNODQ,P1,P2,PSFIM,BY,XYCEN,
     :                  XFIT,YFIT,NPO,NS,NS1,CENCX,CENCY,CENP1,
     :                  CENP2,CENP3,RCENT,PSFMETH,POSMETH,ICSTEP,
     :                  RESIG,NITER,EPSP,EPSB,INTERP,SUBPX,NTRIAL,
     :                  IDUM,DQLIM,RWK1,Y,Z,PSF,PSFPOS,PSFPK,
     :                  PSFWID,PCEN,PFWHM,PSFR,CENT,PSFNS,
     :                  ISHPSF,PSFBS,YS,FLUXI,FLUXO,FLUXVAR,
     :                  BCKMN,PSB,PSBVAR,DWK1,DWK2,DWK3,WK1,WK2,
     :                  WK3,WK4,WK5,WK6,WK7,WK8,WK9,WK10,WK11,
     :                  WK12,WK13,WK14,WK15,WKCOR1,WKCOR2,
     :                  WKCOR3,WKCOR4,FLUXMT,PSBMT,VERBOSE,
     :                  SPEC,SPECERR,SPECDQ,LPOBA,BACK,BACKER,
     :                  BACKDQ)
C
C     This subroutine sets up the PSF and calls NPLUCY,
C     to restore and extract the spectra of NS point 
C     sources from the restored background. The background 
C     spatial variation controlled by a resolution kernel. 
C
C     This subroutine prepares the various input arrays for the
C     Lucy point+background spatial restoration in subroutine NLUCY.
C     The array of the real values of the centres of the pixels 
C     edges are set, the PSF is zeroised and scaled to 1.0 each 
C     Y section by section. The centre of the PSF is determined
C     either as a centroid (PSFMETH=C) or by a Gaussian fit 
C     (PSFMETH=G) and the PSF is shifted to pixel N2/2 and the 
C     PSF is block averaged by the factor BY. The 1-D array of 
C     the smoothing kernel of sigma RESIG is set up. The 
C     position of the point source(s) are either specified as 
C     exact (POSMETH=E) or are determined by cross-correlation 
C     with the PSF X-section by section. A cube of the NS PSF's, 
C     each centred at the Y centre of the PSF array but with
C     the correct fractional pixel shift to match the position
C     of the point sources in the input image, is produced. The 
C     integer pixel shifts are stored in a separate array
C
C     If multiple trials are required the input data is amended 
C     by the random noise taken from a Gaussian distrubtion.
C     The point source flux is returned as a vector SPEC 
C     together with the error, which may be calculated from 
C     multiple trials, and the data quality.
C     The background + point source(s) image (LPOBA=TRUE) or
C     background only image (LPOBA=FALSE) are set with errors
C     set by multiple trials if used, together with the data 
C     quality.
C
      IMPLICIT NONE
      INTEGER N1 ! X dimension of input signal image 
      INTEGER N2 ! Y dimension of input signal image
      INTEGER E1 ! X dimension of input error image 
      INTEGER E2 ! Y dimension of input error image
      INTEGER D1 ! X dimension of input data quality image 
      INTEGER D2 ! Y dimension of input data quality image
      INTEGER P1 ! X dimension of input PSF image
      INTEGER P2 ! Y dimension of input PSF image
      INTEGER BY ! Subsampling factor for input PSF image
      INTEGER ICSTEP ! Number of X-sections to sum for cross-correlation analysis
      INTEGER NITER ! No. of iterations of point source/background restoration
      INTEGER NPO ! Number of point sources in reference coordinate files (>=NS)
      INTEGER NS ! Number of point source spectra to restore
      INTEGER NS1 ! Index number of first point source in input position array to restore
      INTEGER NTRIAL ! No. of Monte Carlo trials of restoration
      INTEGER IDUM ! Value of seed for random number generator
      INTEGER DQIM(D1,D2) ! Data quality array of input image IMAG
      INTEGER DQLIM ! Limiting data quality value for `good' data
      INTEGER SUBPX ! No. of subpixels per pixels for interpolation in TRANS
      INTEGER INTERP ! Key to method for interpolating shifted PSF
      INTEGER ISHPSF(NS) ! Array for the fiducial pixel values for the NS PSF's in PSFNS
      INTEGER SPECDQ(N1,NS) ! Data quality array of NS output spectra SPEC
      INTEGER BACKDQ(N1,N2) ! Data quality array of output background image BACK

      REAL IMAG(N1,N2) ! Array of input spectrum image to be restored
      REAL ERRIM(E1,E2) ! Error array on input array IMAG
      REAL PSFIM(P1,P2) ! Array of PSF for restoration
      REAL CENCX(NPO) ! X reference coordinates of centre of point source
      REAL CENCY(NPO) ! X reference coordinates of centre of point source
      REAL CENP1(NPO) ! 1st term of 3rd order polynomial (*X) specifying centre of point source
      REAL CENP2(NPO) ! 2nd term of 3rd order polynomial (*X*X) specifying centre of point source
      REAL CENP3(NPO) ! 3rd term of 3rd order polynomial (*X*X*X) specifying centre of point source
      REAL RCENT(NPO) ! Array of the Y values of the centres of the point sources to be restored
      REAL RESIG ! Sigma of resolution kernel for restoration of spatial background
      REAL RWK1(P2) ! Holding array for interpolation in PSFBLKY and PSFMANY
      REAL SPEC(N1,NS) ! Output array of the restored NS point source spectra 
      REAL SPECERR(N1,NS) ! Output error array of the restored NS spectra   
      REAL BACK(N1,N2) ! Output array of the restored background
      REAL BACKER(N1,N2) ! Output error array of the restored background

      REAL PSFNS(P1,P2,NS) ! Cube of NS PSF's shifted to centre of point source

      DOUBLE PRECISION XIN(N1) ! Array of the X (wavelength) values of the pixels
      DOUBLE PRECISION XYCEN(N1,N2) ! Array of Y values of X mid-points of pixels
      DOUBLE PRECISION XFIT(N1,N2) ! Array of some fitting function for X centres of all pixels
      DOUBLE PRECISION YFIT(N1,N2) ! Array of some fitting function for Y centres of all pixels
      DOUBLE PRECISION EPSP ! Convergence criterion for limit in change of fractional point source flux
      DOUBLE PRECISION EPSB ! Convergence criterion for limit in change of fractional background per iteration
      DOUBLE PRECISION Y(N2) ! Array of float values of centres of Y pixels
      DOUBLE PRECISION Z(N1,N2) ! Input image in counts to be restored
      DOUBLE PRECISION PSF(P1,P2) ! Holding array for PSF image
      DOUBLE PRECISION PSFPOS(P1) ! Array of rough position of peak of (P1) PSF profiles
      DOUBLE PRECISION PSFPK(P1) ! Array of rough peak value of PSF profiles
      DOUBLE PRECISION PSFWID(P1) ! Array of rough position of FWHM of (P1) PSF profiles
      DOUBLE PRECISION PCEN(P1) ! Array of accurate position of peak of (P1) PSF profiles
      DOUBLE PRECISION PFWHM(P1) ! Array of accurate FWHM of peak of (P1) PSF profiles
      DOUBLE PRECISION PSFR(N2) ! Array of the background resolution Gaussian kernel
      DOUBLE PRECISION CENT(N1,NS) ! Array of (N1) initial/accurate positions of (NS) point sources
      DOUBLE PRECISION PSFBS(N1,N2) ! Block summed version of input PSF array (PSF centred at N2/2)
      DOUBLE PRECISION YS(N2) ! Holding array for 1-D x-sections of PSF
      DOUBLE PRECISION FLUXI(N1,NS) ! Holding array for estimate of flux spectrum of the NS point sources
      DOUBLE PRECISION FLUXO(N1,NS) ! Holding array for flux spectrum of the NS point sources
      DOUBLE PRECISION FLUXVAR(N1,NS) ! Holding array for fluxes of NS point sources from Monte Carlo trials  
      DOUBLE PRECISION BCKMN(N1) ! Estimated mean background level per pixel as function of X pixel
      DOUBLE PRECISION PSB(N1,N2) ! Holding array for restored background 
      DOUBLE PRECISION PSBVAR(N1,N2) ! Holding array for restored background from Monte Carlo trials

      DOUBLE PRECISION DWK1(N2) ! Holding array for CH convolved with effective PSF
      DOUBLE PRECISION DWK2(N2) ! Holding array for the summmed contribution of all point sources      
      DOUBLE PRECISION DWK3(N2) ! Holding array for the auxilliary function

      DOUBLE PRECISION WK1(N2,NS) ! Work array for NLUCY
      DOUBLE PRECISION WK2(N2) ! Work array for NLUCY
      DOUBLE PRECISION WK3(N2) ! Work array for NLUCY
      DOUBLE PRECISION WK4(N2*2) ! Work array for NLUCY
      DOUBLE PRECISION WK5(N1,N2*2) ! Work array for NLUCY
      DOUBLE PRECISION WK6(N1,N2) ! Work array for NLUCY
      DOUBLE PRECISION WK7(N2) ! Work array for NLUCY
      DOUBLE PRECISION WK8(N2) ! Work array for NLUCY
      DOUBLE PRECISION WK9(NS) ! Work array for NLUCY
      DOUBLE PRECISION WK10(N1,N2) ! Work array for NLUCY
      DOUBLE PRECISION WK11(N1,NS) ! Work array for NLUCY
      DOUBLE PRECISION WK12(N2) ! Work array for NLUCY
      DOUBLE PRECISION WK13(N2) ! Work array for NLUCY
      DOUBLE PRECISION WK14(N2*2) ! Work array for NLUCY
      DOUBLE PRECISION WK15(N2*2) ! Work array for NLUCY

      DOUBLE PRECISION WKCOR1(N2*2) ! Work array for complex value of data
      DOUBLE PRECISION WKCOR2(N2*2) ! Work array for complex value of PSF
      DOUBLE PRECISION WKCOR3(N2*2) ! Work array for complex value of cross-correlation
      DOUBLE PRECISION WKCOR4(N2*2) ! Work array for complex value for FFT

      DOUBLE PRECISION FLUXMT(N1,NS) ! Output flux spectrum of the NS point sources 
      DOUBLE PRECISION PSBMT(N1,N2) ! Restored spectrum of background with poi

      CHARACTER*1 PSFMETH ! Method for determining centre of PSF (Centroid or Gaussian)
      CHARACTER*1 POSMETH ! Method for determining position of point source (Exact or by Cross-Correlation) 

      LOGICAL LNOERR ! If TRUE then error image not used
      LOGICAL LNODQ ! If TRUE then data quality is not used
      LOGICAL VERBOSE  ! If TRUE then write out status/error reports 
      LOGICAL LPOBA ! If TRUE copy background + point sources to output PH array; else copy background only
C
C     Local variables
C
      INTEGER I,J,K,L
      INTEGER NNEG
      INTEGER BX
      INTEGER N11
      INTEGER N1C
      INTEGER ICPSF
      INTEGER ISTAT
      INTEGER IRL1
      INTEGER IRL2
      INTEGER NTR

      PARAMETER (N11=1)

      REAL VAL
      REAL RAN1
      REAL NORDEV

      DOUBLE PRECISION VMIN
      DOUBLE PRECISION OVERFL
      DOUBLE PRECISION SUM
      DOUBLE PRECISION CPSF
      
      CHARACTER*128 TEXT

      LOGICAL LZERO
      LOGICAL LOVER

      OVERFL=1.0D30 ! If FLUXMT larger than this then multiple trial results not summed

C
C     Form the array of the channel positions of the
C     Y values (using IRAF pixel convention)
C
      DO J=1,N2,1
        Y(J)=DBLE(J)
      ENDDO
C
C     Form the array XYCEN giving the real Y position of the
C     Y edges of the pixels at the position of the X pixel centre
C
      CALL MKXYCEN(N1,N2,XIN,Y,XFIT,YFIT,XYCEN)
C
C     Prepare the PSF image. Convert to double precision.
C     Check for zero's. Determine position of centre of
C     profile. Rebin to the observed pixel size and shift 
C     each X section so that the PSF is centred at channel 
C     P2/2 as required for the FFT's and consistency
C
      DO J=1,P2,1
        DO I=1,P1,1
          PSF(I,J)=DBLE(PSFIM(I,J))
        ENDDO
      ENDDO
C                
C     Check for negative values, if there are any set 
C     them to zero. Check that for each X-section of the 
C     PSF, the total count is normalised to 1.0. If not 
C     normalise.  If any X-section of the PSF image has
C     a sum <=0, then flag LZERO is set FALSE. In this case
C     zero the output arrays and exit
C
      CALL ZAPTOT(P1,P2,PSF,DWK1,VERBOSE,LZERO)
      IF (.NOT.LZERO) THEN
        WRITE(TEXT,'(''! Warning. PSF image has columns with''
     :        '' zero signal. No restoration possible '')')
        CALL UMSPUT(TEXT,1,0,ISTAT)
        DO K=1,NS,1
          DO I=1,N1,1
            FLUXO(I,K)=0.0D0
          ENDDO
        ENDDO
        DO I=1,N1,1
          DO J=1,N2,1
            PSB(I,J)=0.0D0
          ENDDO
        ENDDO
        GO TO 999
      ENDIF
C
C     For each X section of the PSF determine the rough position
C     of the peak, peak value and approximate FWHM of the PSF
C
      CALL PSFPARN(P1,P2,PSF,N2,Y,YS,PSFPOS,PSFPK,PSFWID)

c      do i=1,P1,1
c        write(text,501) I,PSFPOS(I),PSFPK(I),PSFWID(I)
c501     format(' I,PSF-POS,PK,WID',I5,3(1x,E11.5))
c        call umsput(text,1,0,istat)
c      enddo

C
C     Determine the exact position of the PSF centre for each
C     P1 X-sections using either simple centroid (PSFMETH=C) or
C     Gaussian fit (PSFMETH=G)
C
      CALL PSFCENTN(P1,P2,PSF,N2,Y,PSFPOS,PSFPK,PSFWID,PSFMETH,
     :              YS,DWK1,DWK2,PCEN,PFWHM)

c      do i=1,256,1
c        write(text,502) I,PCEN(I),PFWHM(I)
c502     format(' I,PSF-CEN,FWHM',I5,2(1x,E11.5))
c        call umsput(text,3,0,istat)
c      enddo

C
C     Block sum the subsampled PSF to the same pixel size as 
C     the data and centre the PSF at channel N2/2
C
      BX=1
      CALL PSFBLKY(P1,P2,PSF,BX,BY,PCEN,INTERP,
     :             RWK1,N2,DWK1,N1,N2,PSFBS)

c      do i=1,n2,1
c        if (psfbs(128,i).gt.0.0d0) then
c          write(text,29) i,psfbs(128,i)
c29        format(' I,PSFBS(128,I) ',I4,1x,F12.5)
c          call umsput(text,1,0,istat)
c        endif
c      enddo

C
C     Check for negative values, if there are any set 
C     them to zero. Check that for each X section of the 
C     PSF, the total count is normalised to 1.0. If not 
C     normalise. 
C
      CALL ZAPTOT(N1,N2,PSFBS,DWK1,.FALSE.,LZERO)
C
C     Form the Gaussian of the resolution kernel centred at
C     central channel and the channel limits corresponding to  
C     +&- 10 sigma about the peak position (IRL1 and IRL2)
C
      N1C=N1/2 ! Central X channel
      ICPSF=(N2/2) - 1 ! Required centre of image for Fourrier routines
      CPSF=DBLE(ICPSF)
      CALL PSFKER(N2,Y,CPSF,DBLE(RESIG),PSFR,IRL1,IRL2)
C
C     Set the Y centres of the NS profiles within the Y range 
C     2 to N2-2 for all X channels in the array CENT. The number
C     of point sources in range is NS and the first is NS1
C
      CALL POICENN(NPO,CENCX,CENCY,CENP1,CENP2,CENP3,RCENT,
     :             N1,NS,NS1,CENT)
C      
C     If performing multiple trials initialize random
C     number generator
C
      IF (NTRIAL.GT.1) THEN
        VAL=RAN1(-IDUM)
      ENDIF
C
C     Initialize the variance arrays
C
      IF (.NOT.LNOERR) THEN
        DO I=1,N1,1
          DO K=1,NS,1
            FLUXVAR(I,K)=0.0D0
          ENDDO
        ENDDO
        DO I=1,N1,1
          DO J=1,N2,1
            PSBVAR(I,J)=0.0D0
          ENDDO
        ENDDO
      ENDIF
C
C     Perform restoration over number of trials
C
      NTR=1
      DO L=1,NTRIAL,1
        DO I=1,N1,1
          DO J=1,N2,1
            IF (L.EQ.1) THEN
              Z(I,J)=DBLE(IMAG(I,J))
            ELSE
              IF (.NOT.LNOERR.AND.L.GT.1) THEN
                Z(I,J)=DBLE(NORDEV(IDUM,IMAG(I,J),ERRIM(I,J)))
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C                
C       Check for the presence of negative values, if there are 
C       any set them to zero
C
        CALL ZAPNEG(Z,N1,N2,NNEG,VMIN,0.0D0)
        IF (NNEG.GT.0) THEN
          IF (VERBOSE) THEN
            WRITE(TEXT,
     :         '(''! Warning, data image '',
     :         '' had '',I7,'' negative values, set'',
     :         '' to zero.'')') NNEG
            CALL UMSPUT(TEXT,1,0,ISTAT)
          ENDIF
        ENDIF
C
C       Add up the total in the data image and check greater
C       than zero
C
        CALL TOTAL(Z,N1,N2,SUM)
        IF (SUM.LE.0.0D0) THEN
          WRITE(TEXT,'(''! Warning, sum of data image'',
     :          '' zero. No restoration '')')
          DO K=1,NS,1
            DO I=1,N1,1
              FLUXO(I,K)=0.0D0
            ENDDO
          ENDDO
          DO I=1,N1,1
            DO J=1,N2,1
              PSB(I,J)=0.0D0
            ENDDO
          ENDDO
          GO TO 999
        ENDIF
C
C       Determine the position of the point sources by cross-correlation
C       with the PSF (POSMETH=C) if not exactly set (POSMETH=E)
C       
        IF (POSMETH.EQ.'C'.OR.POSMETH.EQ.'c') THEN
          CALL POSCORN(N1,N2,Z,P1,P2,PSF,Y,NS,CENT,ICSTEP,PCEN,
     :                 PFWHM,YS,WK14,DWK1,DWK2,DWK3,N2*2,WKCOR1,
     :                 WKCOR2,WKCOR3,WKCOR4)
        ENDIF

c        do i=1,ns,1
c          write(21,390) i,cent(128,i)
c390       format(' N,CENT(128,N) ',I5,1x,F12.5)
c          call umsput(text,1,0,istat)
c        enddo

C
C       Set the initial estimate of the point source flux for
C       each source for all (N1) X-sections
C
        CALL FLUESTN(N1,N2,Z,NS,CENT,PFWHM,BY,YS,WK5,FLUXI)

c        do i=1,n1,1
c          do j=1,ns,1
c            write(text,507) I,J,FLUXI(I,J)
c507         format(' I,NS,FLUX',I5,I5,1x,E12.5)
c            call umsput(text,1,0,istat)
c          enddo
c        enddo

        IF (VERBOSE) THEN
          WRITE(TEXT,'(''                             '')')
          CALL UMSPUT(TEXT,1,0,ISTAT)
          DO K=1,NS,1
            WRITE(TEXT,'(''!-- Spectrum No.'',I4,'' at Y pix = '',F7.2,
     :      '' flux '',E11.4E2,'' (mean per pix at X= '',I5,'')'')') 
     :      K+NS1-1,CENT(N1C,K),FLUXI(N1C,K),N1C
            CALL UMSPUT(TEXT,1,0,ISTAT)
          ENDDO
        ENDIF
C
C       Set the initial estimate of the mean background level per
C       pixel excluding the NS point sources for all (N1) X-sections
C
        CALL BACKMN(N1,N2,Z,NS,FLUXI,BCKMN)

        IF (VERBOSE) THEN
          WRITE(TEXT,'(''!-- Mean background '',E11.4E2
     :    '' (mean per pix at X= '',I5,'')'')') BCKMN(N1C),N1C
          CALL UMSPUT(TEXT,1,0,ISTAT)
        ENDIF
C
C       Form a cube of the NS PSF images, each PSF image
C       with the PSF shifted to the position of the point
C       source and block summed to the same pixel size as
C       the data
C
        CALL PSFMANY(P1,P2,PSF,PCEN,BY,NS,CENT,INTERP,RWK1,
     :               N2,DWK1,PSFNS,ISHPSF) 

c        do k=1,ns,1
c          do i=128,128,1
c            do j=1,p2,1
c              if (psfns(i,j,k).gt.0.0) then
c                write(text,508) I,J,K,PSFNS(I,J,K),ISHPSF(k)
c508             format(' I,J,K,PSFNS,ISHPSF ',I5,I5,I5,1x,E12.6,1x,I5)
c                call umsput(text,1,0,istat)
c              endif
c            enddo
c          enddo
c        enddo

C
C       Perform the iterative restoration of the point sources
C       and the background in the spatial direction 
C   
        CALL NLUCY(N1,N2,NS,Z,LNODQ,D1,D2,DQIM,CENT,FLUXI,
     :             BCKMN,PSFBS,P1,P2,PSFNS,ISHPSF,PSFR,
     :             XYCEN,CENT,SUBPX,DQLIM,NS1,NITER,EPSP,
     :             EPSB,WK1,WK2,WK3,WK4,WK5,WK6,WK7,WK8,
     :             WK9,WK10,WK11,WK12,WK13,WK14,WK15,
     :             FLUXMT,PSBMT,LPOBA,VERBOSE)
C
C       If not performing multiple trials or first of multiple
C       trials copy point source flux and background holding 
C       arrays to output arrays. Else form variance on point 
C       source flux and background
C
        IF (L.EQ.1) THEN
          DO K=1,NS,1
            DO I=1,N1,1
              FLUXO(I,K)=FLUXMT(I,K)
            ENDDO
          ENDDO
          DO I=1,N1,1
            DO J=1,N2,1
              PSB(I,J)=PSBMT(I,J)
            ENDDO
          ENDDO
          NTR=NTR+1
        ELSE
C
C         Check all values of FLUXMT aren't overflowed. If all
C         values are below OVERFL then sum the variance on the 
C         point source fluxes and the background array
C
          CALL FMTCHKA(N1,NS,FLUXMT,OVERFL,LOVER)
          IF (LOVER) THEN
            DO K=1,NS,1
              DO I=1,N1,1
                FLUXVAR(I,K)=FLUXVAR(I,K) + 
     :                       (FLUXMT(I,K)-FLUXO(I,K))**2
              ENDDO
            ENDDO
            DO I=1,N1,1
              DO J=1,N2,1
                PSBVAR(I,J)=PSBVAR(I,J) + (PSBMT(I,J)-PSB(I,J))**2
              ENDDO
            ENDDO
            NTR=NTR+1
          ENDIF
        ENDIF
C
C       Copy the flux of the restored point source to the output
C       spectrum and the restored background to the output 
C       background array. Copy the input errors and data 
C       quality to the output background arrays if no multiple
C       trials, else copy the rms on the mean of the multiple 
C       trials to the errors arrays of the point source flux and 
C       the background
C
        DO K=1,NS,1
          DO I=1,N1,1
            SPEC(I,K)=REAL(FLUXO(I,K))
            IF (FLUXO(I,K).LT.0.0D0) THEN
              SPECERR(I,K)=0.0
              SPECDQ(I,K)=DQLIM
            ELSE
              IF (NTR.EQ.2) THEN
                SPECERR(I,K)=0.0
                SPECDQ(I,K)=0
              ELSE
                IF (FLUXVAR(I,K).LT.0.0D0) THEN
                  SPECERR(I,K)=0.0
                  SPECDQ(I,K)=DQLIM
                ELSE
                  SPECERR(I,K)=REAL(SQRT(FLUXVAR(I,K)/DBLE(NTR-1)))
                  SPECDQ(I,K)=0
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO

        DO I=1,N1,1
          DO J=1,N2,1
            BACK(I,J)=REAL(PSB(I,J))
            IF (.NOT.LNOERR) THEN
              BACKER(I,J)=REAL(SQRT(PSBVAR(I,J)/DBLE(NTR-1)))
            ENDIF
            IF (.NOT.LNODQ) THEN
              BACKDQ(I,J)=DQIM(I,J)
            ENDIF
          ENDDO
        ENDDO

      ENDDO

999   END

      SUBROUTINE NLUCY(N1,N2,NS,IMA,LNODQ,D1,D2,DQ,PCENT,
     :                 FLUXI,BCKMN,PSF,P1,P2,PSFNS,
     :                 ISHPSF,PSFR,XYCEN,CENT,SUBPX,DQLIM,
     :                 NS1,NITER,EPSP,EPSB,WK1,WK2,WK3,WK4,
     :                 WK5,WK6,WK7,WK8,WK9,WK10,WK11,WK12,
     :                 WK13,WK14,WK15,FLUX,PH,LPOBA,VERBOSE)
C
C     This subroutine performs NITER iterations of the spatial
C     restoration routine ITERLAM for the point source and 
C     background iterative restoration on the input image IMA. 
C     The input image has wavelength direction as the 1st (X) 
C     dimension and the spatial direction, the 2nd (Y) 
C     dimension, is the one that is restored. The PSF of the 
C     point sources is given by PSFNS, which has each PSF 
C     shifted to the position of the point source (given by 
C     PCENT). The PSF of the smoothing kernel for the 
C     background is PSFR.
C     The restoration of the point sources + background 
C     is iterated NITER times in ITERLAM; the background
C     estimate is smoothed by a Gaussian PSFR.
C     The restoration is considered converged when the fractional 
C     change per iteration in restored point sources is less than
C     EPSP and for the background is less than EPSB.
C     The output products are the integrated flux in the NS point
C     sources (FLUX) and the 2-D image PH of the point 
C     sources+background (if LPOBA=TRUE) or 2-D background only
C     (LPOBA=FALSE) 
C
C     Lucy routine: flrj
C
      IMPLICIT NONE
      INTEGER N1 ! X dimension of input image IMA
      INTEGER N2 ! Y dimension of input image IMA
      INTEGER D1 ! X dimension of input data quality image DQ
      INTEGER D2 ! Y dimension of input data quality image DQ
      INTEGER NS ! Number of point source spectra to restore
      INTEGER DQ(D1,D2) ! Data quality array of input array IMA
      INTEGER DQLIM ! Limiting data quality value for `good' data
      INTEGER P1 ! X dimension of input array PSFNS
      INTEGER P2 ! Y dimension of input array PSFNS
      INTEGER ISHPSF(NS) ! Array of integer fiducial positions of PSF's in PSFNS
      INTEGER SUBPX ! No. of subpixels per pixel for interpolation
      INTEGER NS1 ! Pointer to first valid point source in original input tables
      INTEGER NITER ! No. of iterations of point source and background restoration

      REAL PSFNS(P1,P2,NS) ! Cube of NS PSF's shifted to centres of point sources

      DOUBLE PRECISION IMA(N1,N2) ! Input image in counts to be restored
      DOUBLE PRECISION PCENT(N1,NS) ! Array of accurate positions of NS point sources for N1 X-sections
      DOUBLE PRECISION FLUXI(N1,NS) ! Initial estimate of the integrated flux spectra of point sources
      DOUBLE PRECISION BCKMN(N1) ! Estimated mean background level per pixel as function of X pixel
      DOUBLE PRECISION PSF(N1,N2) ! Input PSF image with PSF centred at channel N2/2
      DOUBLE PRECISION PSFR(N2) ! Array of the background resolution Gaussian kernel
      DOUBLE PRECISION XYCEN(N1,N2) ! Array of Y values of X mid-points of pixels
      DOUBLE PRECISION CENT(N1,NS) ! Array of (N1) positions of (NS) point sources
      DOUBLE PRECISION EPSP ! Convergence criterion for limit in change of fractional point sources
      DOUBLE PRECISION EPSB ! Convergence criterion for limit in change of fractional background flux

      DOUBLE PRECISION WK1(N2,NS) ! Work array for ITERLAM
      DOUBLE PRECISION WK2(N2) ! Work array for ITERLAM
      DOUBLE PRECISION WK3(N2) ! Work array for ITERLAM
      DOUBLE PRECISION WK4(N2*2) ! Work array for ITERLAM
      DOUBLE PRECISION WK5(N1,N2*2) ! Work array for ITERLAM
      DOUBLE PRECISION WK6(N1,N2) ! Work array for ITERLAM
      DOUBLE PRECISION WK7(N2) ! Work array for ITERLAM
      DOUBLE PRECISION WK8(N2) ! Work array for ITERLAM
      DOUBLE PRECISION WK9(NS) ! Work array for ITERLAM
      DOUBLE PRECISION WK10(N1,N2) ! Work array for ITERLAM
      DOUBLE PRECISION WK11(N1,NS) ! Work array for ITERLAM
      DOUBLE PRECISION WK12(N2) ! Work array for ITERLAM
      DOUBLE PRECISION WK13(N2) ! Work array for ITERLAM
      DOUBLE PRECISION WK14(N2*2) ! Work array for ITERLAM
      DOUBLE PRECISION WK15(N2*2) ! Work array for ITERLAM

      DOUBLE PRECISION FLUX(N1,NS) ! Output flux spectrum of the NS point sources 
      DOUBLE PRECISION PH(N1,N2) ! Restored spectrum of background with point sources 

      LOGICAL LNODQ ! FALSE if data quality DQ not set
      LOGICAL LPOBA ! If TRUE then point sources+background in output image; if FALSE background only
C     (LPOBA=FALSE)  
      LOGICAL VERBOSE
C
C     Local variables
C
      INTEGER I,K

C
C     Set the initial estimate of the point source fluxes to the
C     guessed values
C
      DO I=1,N1,1
        DO K=1,NS,1
          FLUX(I,K)=FLUXI(I,K)
        ENDDO
      ENDDO
C
C     Call ITERLAM to restore the point sources and background for
C     each wavelength element independently
C
      CALL ITERLAM(N1,N2,NS,IMA,LNODQ,D1,D2,DQ,PCENT,
     :             BCKMN,PSF,P1,P2,PSFNS,ISHPSF,XYCEN,CENT,
     :             SUBPX,PSFR,DQLIM,NS1,NITER,EPSP,EPSB,WK1,
     :             WK2,WK3,WK4,WK5,WK6,WK7,WK8,WK9,WK10,
     :             WK11,WK12,WK13,WK14,WK15,LPOBA,VERBOSE,
     :             FLUX,PH)

99    END

      SUBROUTINE ITERLAM(N1,N2,NP,PHT,LNODQ,D1,D2,DQ,PCENT,
     :                   BCKMN,PSF,P1,P2,PSFFI,ISHPSF,
     :                   XYCEN,CENT,SUBPX,PSFR,DQLIM,NS1,ITER,
     :                   EPSP,EPSB,PSFJK,PSFL,PSFE,PSFEFT,
     :                   PSFIFT,CHI,CH,CCH,CF,CHIP,FVP,PHI,
     :                   RT,WK1,WK2,LBGC,VERBOSE,FP,PH)
C
C     This subroutine performs ITER iterations for the NP point
C     source fluxes and the 2D background in the input image PHT  
C     at each N1 spectral (wavelength) channel using a new 
C     application of the Lucy-Hook code to long-slit spectra.
C     No restriction on the spatial variation of the background
C     spectrum is required.  
C     The spatial direction is the 2nd dimension. 
C
C     The array of the spectra of the NP point sources is 
C     returned as FP and the restored 2-D background is PH, with 
C     point sources (if LBGC is TRUE) included, else the background 
C     only
C
C     Lucy routine: ITERATE
C        Revised version with spectral and spatial loops within
C        the iteration scheme
C 
      IMPLICIT NONE
      INTEGER N1 ! X dimension of input image PHT
      INTEGER N2 ! Y dimension of input image PHT
      INTEGER NP ! No. of point source spectra to restore
      INTEGER D1 ! X dimension of input data quality image DQ
      INTEGER D2 ! Y dimension of input data quality image DQ
      INTEGER SUBPX ! No. of subpixels per pixels for interpolation in TRANS
      INTEGER DQ(D1,D2) ! Data quality array of input image PHT
      INTEGER DQLIM ! Limiting data quality value for `good' data
      INTEGER P1 ! X dimension of input array PSFFI
      INTEGER P2 ! Y dimension of inout array PSFFI
      INTEGER ISHPSF(NP) ! Array of the fiducial positions of the NP PSF's in PSFFI
      INTEGER NS1 ! Pointer to first valid point source in original input tables
      INTEGER ITER ! No. of iterations for spectra of point sources and background

      REAL PSFFI(P1,P2,NP) ! Cube of NP PSF's fractionally shifted from P2/2+1 for each point source
 
      DOUBLE PRECISION PHT(N1,N2) ! Input array of observational counts
      DOUBLE PRECISION PCENT(N1,NP) ! Input Y positions of the point sources as a function of X position pixel 
      DOUBLE PRECISION BCKMN(N1) ! Estimated mean background level per pixel as function of X pixel
      DOUBLE PRECISION PSF(N1,N2) ! Input PSF image with PSF centred at channel N2/2+1
      DOUBLE PRECISION XYCEN(N1,N2) ! Array of Y values of X mid-points of pixels
      DOUBLE PRECISION CENT(N1,NP) ! Array of (N1) positions of (NP) point sources
      DOUBLE PRECISION PSFR(N2) ! Array of the background resolution smoothing kernel
      DOUBLE PRECISION EPSP ! Convergence criterion for limit in change of fractional point source flux
      DOUBLE PRECISION EPSB ! Convergence criterion for limit in change of fractional background flux
      DOUBLE PRECISION PSFJK(N2,NP) ! Holding array for Y,Z sections of 3D PSFFI
      DOUBLE PRECISION PSFL(N2) ! Holding array for 1D PSF array 
      DOUBLE PRECISION PSFE(N2) ! Holding array for the effective PSF for the background
      DOUBLE PRECISION PSFEFT(N2*2) ! Work array for FFT of PSFE
      DOUBLE PRECISION PSFIFT(N1,N2*2) ! Holding array for the FFT of the effective PSF for the background for each spectral channel
      DOUBLE PRECISION CHI(N1,N2) ! Holding array for the N1 by 1D auxilliary function for the background
      DOUBLE PRECISION CH(N2) ! Holding array for the 1D auxilliary function for the background
      DOUBLE PRECISION CCH(N2) ! Holding array for the 1D auxilliary function for the background (previous iterate)
      DOUBLE PRECISION CF(NP) ! Holding array for the correction factor for the point sources
      DOUBLE PRECISION CHIP(N1,N2) ! Holding array for the correction factor for the background
      DOUBLE PRECISION FVP(N1,NP) ! Holding array for the point source fluxes (previous iterate)
      DOUBLE PRECISION PHI(N2) ! Holding array for background convolved with effective PSF
      DOUBLE PRECISION RT(N2) ! Holding array for correction factor to background 
      DOUBLE PRECISION WK1(N2*2) ! Work array for 1-D PSF in FFT routines
      DOUBLE PRECISION WK2(N2*2) ! Work array for 1-D PSF in FFT routines

      DOUBLE PRECISION FP(N1,NP) ! Array of integrated spectra of NP point sources
      DOUBLE PRECISION PH(N1,N2) ! Restored spectrum of background with/without point sources 

      LOGICAL LNODQ ! TRUE if data quality DQ not set 
      LOGICAL VERBOSE ! If TRUE then write out status/error reports 
      LOGICAL LBGC ! If TRUE copy background + point sources to output PH array; else copy background only
C
C     Local variables
C
      INTEGER I,J,JJ,K,L
      INTEGER NY
      INTEGER DIMS(2)
      INTEGER IDZ1
      INTEGER ITSUM
      INTEGER NOZ
      INTEGER STAT
 
      PARAMETER (NY=1)

      DOUBLE PRECISION CHLAST
      DOUBLE PRECISION SUMDFLX
      DOUBLE PRECISION SUMFLX
      DOUBLE PRECISION POIDIF
      DOUBLE PRECISION MAXDEL
      DOUBLE PRECISION SUMCH
      DOUBLE PRECISION SUMDIF
      DOUBLE PRECISION BACKDIF

      CHARACTER*128 TEXT

      LOGICAL LCOPOI
      LOGICAL LCOBCK
      LOGICAL LCONP
      LOGICAL LCONB
      LOGICAL ZFLAG

      DIMS(1)=N2
      DIMS(2)=NY
      IDZ1=0
C
C     Initialize the estimates of the spectra of the NP point 
C     sources for each spectral channel FP and the auxiliiary 
C     function, CHI, for the background estimate at each spectral 
C     channel
C
      DO I=1,N1,1
        DO L=1,NP,1
          FP(I,L)=1.0D0 ! FLUXI(I,L) 
        ENDDO
        DO J=1,N2,1
          CHI(I,J)=1.0D0 ! BCKMN(I)
        ENDDO
      ENDDO
      CHLAST=1.0D0 ! Initial fudge value for pixels flagged bad
C
C     For each spectral channel convolve the resolution kernel 
C     with the PSF and then store the FFT of the effective PSF
C     (the spatial PSF convolved with the Gaussian kernel) for
C     convolution with the auxilliary function for the background 
C     CH
C
      DO I=1,N1,1
        DO J=1,N2,1
C
C         Get the PSF of the X channel I for the
C         background restoration
C
          PSFL(J)=PSF(I,J)
        ENDDO
C
C       Prepare for the Fourier transform of PSFL and perform
C       the FFT
C
        CALL DFILL(PSFR,N2,NY,WK1)
        CALL DFOURT(WK1,DIMS,1,-1,0,WK2)
C
C       Convolve PSFL with the resolution kernal PSFR to
C       give the effective PSF for the background PSFE
C
        CALL DCONV(PSFL,N2,NY,WK2,WK1,PSFE,+1)

c        Do J=1,n2,1
c          if (i.eq.128) then
c              write(text,194) j,psfl(j),psfr(j),psfe(j)
c194            format(' J PSFL PSFR PSFE',1x,I4,3(1x,E12.5))
c              call umsput(text,1,0,istat)
c            endif
c        enddo

C
C       Prepare for the Fourier transform of PSFE and perform
C       the FFT
C
        CALL DFILL(PSFE,N2,NY,PSFEFT)
        CALL DFOURT(PSFEFT,DIMS,1,-1,0,WK2)
        DO J=1,N2*2,1
          PSFIFT(I,J)=PSFEFT(J)
        ENDDO
      ENDDO
C
C     Iterate the restoration for the background and NP point 
C     sources for all X-channels 
C
      LCOPOI=.FALSE.
      LCOBCK=.FALSE.
      IDZ1=0
      ITSUM=0
      DO K=1,ITER,1
C
C       Store previous estimates of the spectra of the 
C       NP point sources FVP and the auxilliary function for 
C       the background
C
        DO I=1,N1,1
          DO L=1,NP,1
            FVP(I,L)=FP(I,L)
          ENDDO
          DO J=1,N2,1
            CHIP(I,J)=CHI(I,J)
          ENDDO
        ENDDO
C
C       Write banner for this iteration
C
        IF (VERBOSE) THEN
          WRITE(TEXT,18) 
18        FORMAT('!----------------------------------------------',
     :    '-----------------------------------')
          CALL UMSPUT(TEXT,1,0,STAT)
          WRITE(TEXT,11) K
11        FORMAT('!-- Starting iteration no. ',I4)
          CALL UMSPUT(TEXT,1,0,STAT)
        ENDIF
        MAXDEL=0.0D0
C
C       Loop over each spectral channel
C
        DO I=1,N1,1
C
C         Initialize the PSF YZ section holding array for the
C         PSF's of all points sources at this X-channel
C
          DO L=1,NP,1
            DO J=1,N2,1
              PSFJK(J,L)=0.0D0
            ENDDO
C
C           Copy the NP PSF's of the point sources for X-channel I
C           to a 2D holding array
C
            DO J=1,P2,1
              JJ=J + ISHPSF(L)  
              IF (JJ.GE.1.AND.JJ.LE.N2) THEN
                PSFJK(JJ,L)=DBLE(PSFFI(I,J,L))
              ENDIF
            ENDDO
          ENDDO
C
C         Copy the I'th X-section of the FFT of the effective
C         PSF and the auxilliary function to 1D arrays
C
          DO J=1,N2*2,1
            PSFEFT(J)=PSFIFT(I,J)
          ENDDO
          DO J=1,N2,1
            CH(J)=CHI(I,J)
          ENDDO
C
C         Convolve the current estimate of CH with the effective
C         PSF to get model of background PHI
C
          CALL DCONV(CH,N2,NY,WK2,PSFEFT,PHI,+1)

c         do j=115,142,1
c           If (i.eq.128) then
c             write(text,19) I,j,PHI(j)
c19           format(' J,PHI',I4,1x,I4,1x,E12.5)
c             call umsput(text,1,0,istat)
c           endif
c         enddo

C
C         Add the estimate of the contribution of the point 
C         sources to the background estimate
C
          DO J=1,N2,1
            DO L=1,NP,1
              PHI(J)=PHI(J) + FP(I,L)*PSFJK(J,L)
            ENDDO
          ENDDO

c         do j=115,142,1
c           If (i.eq.128) then
c             write(text,198) I,j,PHI(j)
c198          format(' I,J,PHI',I4,1x,I4,1x,E12.5)
c             call umsput(text,1,0,istat)
c           endif
c         enddo

C
C         Compute the ratio RT of the input data to the sum of
C         the estimated point sources and background
C 
          CALL DIVIDA(N1,N2,PHT,PHI,DQ,DQLIM,LNODQ,I,RT,ZFLAG,NOZ)
          IF (ZFLAG) THEN
C
C           Correlate the correction factor RT with the effective
C           PSF to CCH
C
            CALL DCONV(RT,N2,NY,WK2,PSFEFT,CCH,-1)
          ELSE
            IDZ1=IDZ1+NOZ
          ENDIF
C
C         Form the correction factor for the point sources
C         First initialize the point source estimate
C
          DO L=1,NP,1
            CF(L)=0.0D0
            DO J=1,N2,1
              IF (.NOT.LNODQ) THEN
                IF (DQ(I,J).LT.DQLIM) THEN
                  CF(L)=CF(L) + PSFJK(J,L)*RT(J)
                ENDIF
              ELSE
                CF(L)=CF(L) + PSFJK(J,L)*RT(J)
              ENDIF
            ENDDO
          ENDDO 
C
C         Store the current value of the correction factor
C         for the background and update the correction factor.
C         Take account of the data quality if appropriate
C
          DO J=1,N2,1
            IF (.NOT.LNODQ) THEN
              IF (DQ(I,J).LT.DQLIM) THEN
                CH(J)=CCH(J)*CH(J)
                CHI(I,J)=CH(J)
                CHLAST=CHI(I,J)
              ELSE
                CHI(I,J)=CHLAST
              ENDIF
            ELSE
              CH(J)=CCH(J)*CH(J)
              CHI(I,J)=CH(J)
            ENDIF
          ENDDO
C
C         Store the current value of the corrrection factor for
C         the point sources and update the correction factor
C         for this iteration
C
          DO L=1,NP,1
            FP(I,L)=CF(L)*FP(I,L)
          ENDDO
C
C         If point sources and background required as output
C         copy to output array PH
C
          IF (LBGC) THEN
C
C           Copy sum of background and point sources to output array
C
            DO J=1,N2,1
              PH(I,J)=PHI(J)
            ENDDO
          ELSE
C
C           If only background copied to output array (without point sources)
C
            CALL DCONV(CH,N2,NY,WK2,PSFEFT,PHI,+1) ! Background result of last iteration
            DO J=1,N2,1
              PH(I,J)=PHI(J)
            ENDDO
          ENDIF
        ENDDO ! Completed N1 spectral channels
C
C       Form mean ABS difference per pixel between current 
C       iteration estimate for background and last
C
        SUMDIF=0.0D0
        SUMCH=0.0D0
        DO I=1,N1,1
          DO J=1,N2,1
            IF (.NOT.LNODQ) THEN
              IF (DQ(I,J).LT.DQLIM) THEN
                SUMDIF=SUMDIF + DABS(CHI(I,J)-CHIP(I,J))
                SUMCH=SUMCH +  CHI(I,J)
              ENDIF
            ELSE
              SUMDIF=SUMDIF + DABS(CHI(I,J)-CHIP(I,J))
              SUMCH=SUMCH +  CHI(I,J)
            ENDIF
          ENDDO
        ENDDO
        IF (SUMCH.NE.0.0D0) THEN
          BACKDIF=SUMDIF/SUMCH
        ELSE
          BACKDIF=-999.0D0
        ENDIF
C
C       Test convergence of background
C
        IF (BACKDIF.LE.EPSB.AND.BACKDIF.NE.-999.0D0) THEN
          LCONB=.TRUE.
        ELSE
          LCONB=.FALSE.
        ENDIF
C
C       Form mean ABS difference per point source between the
C       current iteration estimate of the flux and the last. 
C       The point sources are considered converged only when the 
C       fractional difference is less than EPSP for ALL point 
C       sources
C
        LCONP=.FALSE.
        DO L=1,NP,1
          SUMDFLX=0.0D0
          SUMFLX=0.0D0
          DO I=1,N1,1
            IF (FVP(I,L).NE.0.0D0) THEN
              SUMDFLX=SUMDFLX + DABS(FP(I,L)-FVP(I,L))
              SUMFLX=SUMFLX +  FP(I,L)
            ENDIF
          ENDDO
C
C         Test convergence for this point source
C
          IF (SUMFLX.NE.0.0D0) THEN
            POIDIF=SUMDFLX/SUMFLX
          ELSE
            POIDIF=-999.0D0
          ENDIF
          IF (POIDIF.LE.EPSP.AND.POIDIF.NE.-999.0D0) THEN
            LCONP=.TRUE.
          ELSE
            LCONP=.FALSE.
          ENDIF
        ENDDO
C      
C       Test for convergence on background and point source
C       estimates between last and current iterations
C
        IF (VERBOSE) THEN
          IF (LCONB.AND.LCONP) THEN
C
C           All point sources converged
C
            IF (NP.EQ.1) THEN
              WRITE(TEXT,12) 
12            FORMAT('!-- Point source and background spectra',
     :        ' converged')
              CALL UMSPUT(TEXT,1,0,STAT)
            ELSE
              WRITE(TEXT,13) 
13            FORMAT('!-- Point sources and background spectra',
     :        ' converged')
              CALL UMSPUT(TEXT,1,0,STAT)
            ENDIF
            GO TO 100
          ELSE
            IF (NP.EQ.1) THEN
              WRITE(TEXT,14) 
14            FORMAT('!-- Point source and background spectra NOT',
     :        ' converged')
              CALL UMSPUT(TEXT,1,0,STAT)
            ELSE
              WRITE(TEXT,15) 
15            FORMAT('!-- Point sources and background spectra NOT',
     :        ' converged')
              CALL UMSPUT(TEXT,1,0,STAT)
            ENDIF
C
C           Report if either point sources or background already 
C           converged
C
            IF (VERBOSE) THEN
              IF (LCONP.AND.(.NOT.LCOPOI)) THEN
                WRITE(TEXT,'(''!-- ONLY point sources converged '',
     :          '' after '',I5,'' iterations'')') ITSUM
                CALL UMSPUT(TEXT,1,0,STAT)
                LCOPOI=.TRUE.
              ENDIF
              IF (LCONB.AND.(.NOT.LCOBCK)) THEN
                WRITE(TEXT,'(''!-- ONLY background converged '',
     :          '' after '',I5,'' iterations'')') ITSUM
                CALL UMSPUT(TEXT,1,0,STAT)
                LCOBCK=.TRUE.
              ENDIF
            ENDIF
C
C           Form mean fractional absolute difference in flux for all 
C           point sources between current iteration estimate and last
C           to report progress of iterations
C
            DO L=1,NP,1
              SUMDFLX=0.0D0
              SUMFLX=0.0D0
              DO I=1,N1,1
                SUMDFLX=SUMDFLX + DABS((FP(I,L)-FVP(I,L)))
                SUMFLX=SUMFLX + FP(I,L)
              ENDDO
              IF (SUMFLX.NE.0.0D0) THEN
                POIDIF=SUMDFLX/SUMFLX
                WRITE(TEXT,16) L+NS1-1,POIDIF*100.0D0
16              FORMAT('!-- Point source no. ',I4,'; % flux',
     :          ' change ',F10.5) 
                CALL UMSPUT(TEXT,1,0,STAT)
                IF (POIDIF.GT.MAXDEL) THEN
                  MAXDEL=POIDIF
                ENDIF
              ELSE
                WRITE(TEXT,17) L+NS1-1
17              FORMAT('!-- Point source no. ',I4,' flux change',
     :          ' not set')
                CALL UMSPUT(TEXT,1,0,STAT)
              ENDIF
            ENDDO
            IF (NP.GT.2.AND.K.GT.1) THEN
              WRITE(TEXT,19) MAXDEL*100.0D0
19            FORMAT('!-- All point sources: Maximum % flux change ',
     :        F10.5) 
              CALL UMSPUT(TEXT,1,0,STAT)
            ENDIF
            IF (BACKDIF.NE.-999.0D0) THEN
              WRITE(TEXT,20) BACKDIF*100.0D0
20            FORMAT('!-- Background spectrum: % total flux change ',
     :        F10.5) 
              CALL UMSPUT(TEXT,1,0,STAT)
            ENDIF
          ENDIF
C
C         Report the point source flux and the mean background
C         at three X channel numbers (N1/4, N1/2, 3*N1/4
C
          WRITE(TEXT,27) N1/4,N1/2,3*N1/4
27        FORMAT(' Star #   Flux (#',I4,') / Back       Flux (#',
     :    I4,') / Back       Flux (#',I4,') / Back ')  
          CALL UMSPUT(TEXT,1,0,STAT)
          DO L=1,NP,1
C
C           Only report source and background flux if point
C           source within range of Y data values
C
            IF (CENT(N1/4,L).GE.1.AND.CENT(N1/4,L).LE.N2.
     :        AND.CENT(N1/2,L).GE.1.AND.CENT(N1/2,L).LE.N2. 
     :        AND.CENT(3*N1/4,L).GE.1.AND.CENT(3*N1/4,L).LE.N2)
     :        THEN
              WRITE(TEXT,29) L+NS1-1,    
     :        FP(N1/4,L), CHI(N1/4,INT(CENT(N1/4,L))),
     :        FP(N1/2,L),CHI(N1/2,INT(CENT(N1/2,L))),
     :        FP(3*N1/4,L),CHI(3*N1/4,INT(CENT(3*N1/4,L)))
29            FORMAT(1X,I4,2X,3(1X,G12.5E2,1X,G12.5E2))
              CALL UMSPUT(TEXT,1,0,STAT)
            ENDIF
          ENDDO
C
C         Report iteration completed
C
          WRITE(TEXT,31) K 
31        FORMAT('!-- Completed combined iteration no. ',I4)
          CALL UMSPUT(TEXT,1,0,STAT)
        ENDIF
        ITSUM=ITSUM+1
      ENDDO ! End of iterations
C
C     Report information on overall convergence 
C
100   IF (VERBOSE) THEN
        IF (ITSUM.LT.ITER) THEN
          WRITE(TEXT,'(''!-- Point+back restoration: '',
     :    ''converged after '',I5,''/'',I5,'' iterations'')') ITSUM,ITER
          CALL UMSPUT(TEXT,1,0,STAT)
        ELSE
          WRITE(TEXT,'(''!-- Point+back restoration: '',
     :    '' not converged after '',I5,'' iterations'')') ITER
          CALL UMSPUT(TEXT,1,0,STAT)
        ENDIF
      ENDIF
C
C     Report frequency of any divide by zeroes for all X-sections
C 
      IF (VERBOSE) THEN
        IF (IDZ1.GT.0) THEN
          WRITE(TEXT,'(''!-- No. of divide by zeroes for RT '',
     :    ''per X-section '',I5)') IDZ1/N1
          CALL UMSPUT(TEXT,1,0,STAT)
        ENDIF
      ENDIF

99    END


