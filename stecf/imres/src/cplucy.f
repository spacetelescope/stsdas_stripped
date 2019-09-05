      SUBROUTINE CPLUCY   !I
CM    PROGRAM MCLUCY      !M
C++
C
C CPLUCY.F Version 0.22
C
C Lucy point-sources+smooth extended objects restoration.
C Experimental version for shifted, multiple PSFs. .                          
C
C Implementation Details:
C
C This version uses the F77/VOS interface to IRAF parameters and
C data structures.
C
C Apart from the F77/VOS standard libraries the only other external
C dependencies are:
C
C fftetc.f - a set of FFT support routines including several
C            FFT codes.
C
C x_cplucy.x - the one line of SPP needed for linking purposes.
C
C timtem.x - routines needed for image list handling from F77/VOS
C
C Author:
C
C Richard Hook, ST-ECF, e-mail: rhook@eso.org 
C (Based on ideas and code by Leon Lucy)
C
C History:
C
C V0.1 first try, January 1997
C V0.2 adding acceleration and subsampled PSFs, April 1997
C V0.21 preliminary release for Alan Stockton, November 1997
C V0.22 modified to also write out the stars image, if requested, April 1998
C
C Note:
C
C This version may also be compiled and linked to run under MIDAS
C as well as IRAF. Essentially all that is required is for the
C lines ending with !I to be commented out and the comment
C characters removed from lines starting CM. The result should
C then be linked with the additional library 'midint.f' which
C emulates (partially) the F77/VOS routines using MIDAS.
C
C--
      IMPLICIT NONE
                   
C Iraf global storage
      DOUBLE PRECISION MEMD(1) !I
      REAL MEMR(1) !I
      COMMON /MEM/MEMD         !I
      EQUIVALENCE (MEMR,MEMD)  !I
                                 
C MIDAS global storage
CM    INTEGER MEMD(1)          !M
CM    COMMON /VMR/MEMD         !M
                                 
C Parameters
      INTEGER MAXSTR
      PARAMETER (MAXSTR=100000)
      INTEGER USEOF
      PARAMETER (USEOF=-2)

C Local variables
      INTEGER DATTYP,NDIMS,NPSF
      INTEGER PHT,PSFPNT,PINPNT
      INTEGER ISTAT,NX,NY,N,NITER,DIMS(7),NPX,NPY
      INTEGER PSFFFT,WORK,PSB,PSFR,PHSM
      INTEGER CH,NNEG,CF,PHS,RU,RP,CFP,PSFP,FPFFT,PHB,PSFS
      INTEGER SD,DPSB,TEMP
      INTEGER IDD,IDP,IDPS,IDSI
      INTEGER PX,PY,PTYP,PDIMS(7),PNDIMS
      INTEGER BX,BY
      DOUBLE PRECISION PT,SCRATCH(40960),CS(MAXSTR),SMB
      DOUBLE PRECISION FAC(MAXSTR)
      DOUBLE PRECISION RMSRES,RESMAX,SUM,MAGZERO,SMS,TT,XMU
      DOUBLE PRECISION XMU1,XMU2
      DOUBLE PRECISION GW,HH,SS,STARS(5,MAXSTR),QQ,AG
      INTEGER IRMAX,JRMAX,NSTARS
      INTEGER I,INTYPE
      REAL DUMMY
      DOUBLE PRECISION VMIN,LV,XL
      CHARACTER*80 DATA,PSF,FILE,OUTTAB
      CHARACTER*80 BACK,STARIM
      CHARACTER*80 CHARS
      CHARACTER*8 INTERP
      LOGICAL VERBOSE
      LOGICAL ACCEL 
      LOGICAL PRF
C++
   
C Start of code
               
CM    CALL STSPRO('MCLUCY') !M
                              
C First get the boolean control logicals
      CALL UCLGSB('verbose',VERBOSE,ISTAT)
      CALL UCLGSB('prf',PRF,ISTAT)

C Initialise the maximised quantity (obj func.) to something v. small
      LV=-1.0E20

C We have no acceleration at the moment
      ACCEL=.FALSE.

C Announce the version
      IF(VERBOSE) CALL UMSPUT('+ CPLUCY Version 0.22 (Apr 98)', !I
     :            1,0,ISTAT)                                      !I
CM    IF(VERBOSE) CALL UMSPUT('+ MCLUCY Version 0.22 (Apr 98)', !M
CM   :            1,0,ISTAT)                                      !M
                            
C Get the magnitude zero point
      CALL UCLGSR('magzero',DUMMY,ISTAT)
      MAGZERO=DBLE(DUMMY)

C Before we start also get the interpolation type and convert
C to a numerical code, this is used by the code which creates
C the PSFS by shifting
      CALL UCLGST('interpol',INTERP,ISTAT)
      IF(INTERP.EQ.'nearest') THEN
         INTYPE=1
      ELSE IF(INTERP.EQ.'linear') THEN
         INTYPE=2
      ELSE IF(INTERP.EQ.'poly3') THEN
         INTYPE=3
      ELSE IF(INTERP.EQ.'poly5') THEN
         INTYPE=4
      ELSE IF(INTERP.EQ.'spline3') THEN
         INTYPE=5
      ELSE
         CALL UMSPUT('! Invalid interpolant specified',
     :               1,0,ISTAT)
         GO TO 99
      ENDIF 

C Get the name of the input data image
      CALL UCLGST('data',DATA,ISTAT)
      CALL UIMOPN(DATA,1,IDD,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT('! Unable to open data image',
     :                    1,0,ISTAT)
         GO TO 99
      ELSE
         IF(VERBOSE) CALL UMSPUT('-Opening data file: '//DATA,
     :                   1,0,ISTAT)
      ENDIF
         
C Get the name of the PSF image
      CALL UCLGST('psf',PSF,ISTAT)

      CALL UIMOPN(PSF,1,IDP,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT('! Unable to open PSF image',
     :                 1,0,ISTAT)
         GO TO 99
      ELSE
         IF(VERBOSE) 
     :      CALL UMSPUT('-Opening PSF file: '//PSF,
     :               1,0,ISTAT)
      ENDIF

C Get the fractional contribution of the entropy term
      CALL UCLGSR('fracent',DUMMY,ISTAT)
      AG=DBLE(DUMMY)

C Get the sub-sampling factors of the PSF image supplied
      CALL UCLGSI('psfxb',BX,ISTAT)
      CALL UCLGSI('psfyb',BY,ISTAT)

C Verify they are sensible
      IF(BX.LT.1 .OR. BY.LT.1) THEN
         CALL UMSPUT('! Invalid PSF blocking factors',
     :               1,0,ISTAT)
         GO TO 99
      ENDIF

C Do some checks on PSF size etc
C Get the shapes and sizes of the images
      CALL UIMGID(IDD,DATTYP,NDIMS,DIMS,ISTAT)
                                                    
      IF(NDIMS.EQ.1) THEN
         DIMS(2)=1
      ENDIF
           
      NX=DIMS(1)
      NY=DIMS(2)

C Check that the dimensions are EVEN, if not abort
      IF(MOD(NX,2).NE.0 .OR.
     :   (MOD(NY,2).NE.0 .AND. NY.NE.1)) THEN
         CALL UMSPUT('! Image dimensions must be even',
     :               1,0,ISTAT)
         GO TO 99
      ENDIF

C Same for the PSF - which is a different size
      CALL UIMGID(IDP,PTYP,PNDIMS,PDIMS,ISTAT)
                                                      
      IF(PNDIMS.EQ.1) THEN
         PDIMS(2)=1
      ENDIF
              
      PX=PDIMS(1)
      PY=PDIMS(2)

C Check they are OK
      IF(PX/BX.GT.NX .OR. PY/BY.GT.NY) THEN
         CALL UMSPUT('! PSF image is larger than data',
     :               1,0,ISTAT)
         GO TO 99
      ENDIF

C We have a floating prior so get the smoothing kernel sigma
C and create an image for it. Then take the FFT and remove the
C original image
      CALL UCLGSR('skernel',DUMMY,ISTAT)
      GW=DBLE(DUMMY)

      CALL UDMGET(NX*NY,7,PSFP,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :        '! Unable to allocate memory for PSFP array',
     :                 1,0,ISTAT)
         GO TO 99
      ENDIF

      CALL UDMGET(NX*NY*2,7,FPFFT,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :        '! Unable to allocate memory for FPFFT array',
     :                 1,0,ISTAT)
         GO TO 99
      ENDIF

C Put a central, circular, normalised Gaussian into the image
      CALL FGAUSS(MEMD(PSFP),NX,NY,GW)

      CALL DFILL(MEMD(PSFP),NX,NY,MEMD(FPFFT))

      CALL DFOURT(MEMD(FPFFT),DIMS,2,-1,0,SCRATCH)

      CALL UDMFRE(PSFP,7,ISTAT)

C We also get space for the other images we will need
      CALL UDMGET(NX*NY,7,RP,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :        '! Unable to allocate memory for RP array',
     :                 1,0,ISTAT)
         GO TO 99
      ENDIF

      CALL UDMGET(NX*NY,7,CH,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :        '! Unable to allocate memory for CH array',
     :                 1,0,ISTAT)
         GO TO 99
      ENDIF

      CALL UDMGET(NX*NY,7,CFP,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :           '! Unable to allocate memory for CFP array',
     :                    1,0,ISTAT)
         GO TO 99
      ENDIF

      CALL UDMGET(NX*NY,7,SD,ISTAT)
      IF(ISTAT.NE.0) THEN
        CALL UMSPUT(
     :        '! Unable to allocate memory for SD array',
     :                 1,0,ISTAT)
         GO TO 99
      ENDIF

      CALL UDMGET(NX*NY,7,PHB,ISTAT)
      IF(ISTAT.NE.0) THEN
        CALL UMSPUT(
     :        '! Unable to allocate memory for PHB array',
     :                 1,0,ISTAT)
         GO TO 99
      ENDIF

      CALL UDMGET(NX*NY,7,DPSB,ISTAT)
      IF(ISTAT.NE.0) THEN
        CALL UMSPUT(
     :        '! Unable to allocate memory for DPSB array',
     :                 1,0,ISTAT)
         GO TO 99
      ENDIF

      CALL UDMGET(NX*NY,7,RU,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :      '! Unable to allocate memory for RU array',
     :            1,0,ISTAT)
         GO TO 99
      ENDIF
      CALL FILCON(MEMD(RU),NX,NY,0.0D0)

C Get the name of the stars list
      CALL UCLGST('starlist',FILE,ISTAT)

      IF(FILE.NE.' ') THEN
         CALL GETSTR(FILE,STARS,NX,NY,1,
     :               MAXSTR,MAGZERO,
     :               NSTARS,ISTAT)
         IF(ISTAT.NE.0) GO TO 99

C Report the file name of the number of stars
         IF(VERBOSE) THEN
            WRITE(CHARS,'(''-Reading star list file: '',
     :                    A20,'' ('',I5,'' stars).'')')
     :      FILE,NSTARS
            CALL UMSPUT(CHARS,1,0,ISTAT)
         ENDIF

C Also get the name for the output text file
         CALL UCLGST('outtab',OUTTAB,ISTAT)

         IF(OUTTAB.NE.' ') THEN
            OPEN(8,FILE=OUTTAB,STATUS='NEW',IOSTAT=ISTAT)
            IF(ISTAT.NE.0) THEN
               CALL UMSPUT(
     :      '! Unable to open output text list file',
     :                     1,0,ISTAT)
               GO TO 99
            ENDIF
         ENDIF
      ELSE
         NSTARS=0
      ENDIF

C Now we have the star list we can prepare the PSFs array - this 
C is a set of small, shifted versions of the PSF. First we find
C how big these are to be
      IF(NSTARS.GT.0) THEN
         CALL UCLGSI('psfsnx',NPX,ISTAT)
         CALL UCLGSI('psfsny',NPY,ISTAT)

C Now we allocate space for the 3d PSFs 
         CALL UDMGET(NPX*NPY*NSTARS,7,PSFS,ISTAT)
         IF(ISTAT.NE.0) THEN
           CALL UMSPUT(
     :        '! Unable to allocate memory for sub PSFs array',
     :                 1,0,ISTAT)
           GO TO 99
         ENDIF

C We also get a temporary single image
         CALL UDMGET(NPX*NPY,7,TEMP,ISTAT)
         IF(ISTAT.NE.0) THEN
           CALL UMSPUT(
     :        '! Unable to allocate memory for sub TEMP array',
     :                 1,0,ISTAT)
           GO TO 99
         ENDIF
      ENDIF

C Allocate space for the data
      CALL UDMGET(NX*NY,7,PHT,ISTAT)
      IF(ISTAT.NE.0) THEN
        CALL UMSPUT(
     :        '! Unable to allocate memory for data array',
     :                 1,0,ISTAT)
        GO TO 99
      ENDIF
             
C Read the data into the memory
      CALL UIGS2D(IDD,1,NX,1,NY,MEMD(PHT),ISTAT)
      IF(ISTAT.NE.0) THEN
        CALL UMSPUT(
     :        '! Unable to read in data input image',
     :                 1,0,ISTAT)
        GO TO 99
      ENDIF
             
C Check that the data is non-negative
      CALL ZAPNEG(MEMD(PHT),NX,NY,NNEG,VMIN,0.0D0)
      IF(NNEG.GT.0) THEN
          WRITE(CHARS,
     :    '(''! Warning, data image'',
     :    '' contains'',I7,'' negative values, these'',
     :    '' have been zeroed.'')') NNEG
         CALL UMSPUT(CHARS,1,0,ISTAT)
         WRITE(CHARS,'(''--Minimum value: '',d20.10)') VMIN
         CALL UMSPUT(CHARS,1,0,ISTAT)
      ENDIF
             
C Allocate space for the PSF - first the big one (same size as
C the data)
      CALL UDMGET(NX*NY,7,PSFPNT,ISTAT)
      IF(ISTAT.NE.0) THEN
        CALL UMSPUT(
     :  '! Unable to allocate memory for PSF array',
     :           1,0,ISTAT)
        GO TO 99
      ENDIF
             
C and now the actual (sub-sampled) one we are to read in
      CALL UDMGET(PX*PY,7,PINPNT,ISTAT)
      IF(ISTAT.NE.0) THEN
        CALL UMSPUT(
     :  '! Unable to allocate memory for PSF input array',
     :           1,0,ISTAT)
        GO TO 99
      ENDIF
             
C Read the PSF into the memory
      CALL UIGS2D(IDP,1,PX,1,PY,MEMD(PINPNT),ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :   '! Unable to read in PSF input image',
     :            1,0,ISTAT)
         GO TO 99
      ENDIF

C Renormalise the input PSF before continuing
      CALL TOTAL(MEMD(PINPNT),PX,PY,PT)
      IF(DABS(PT-1.0D0).GT.1.0E-5) THEN
         WRITE(CHARS,
     :    '(''! Warning, input PSF totals '',
     :     F15.7,'' - renormalising.'')') PT
         CALL UMSPUT(CHARS,1,0,ISTAT)
         CALL MULC(MEMD(PINPNT),NX,NY,1.0D0/PT,
     :                MEMD(PINPNT))
      ENDIF

C Fill the PSF internal version (for the background convolutions)
C with a block-summed version of the input one
      CALL PSFBLK(MEMD(PINPNT),PX,PY,MEMD(PSFPNT),NX,NY,BX,BY)

C We use the PSF in two ways - first to generate the smaller
C shifted versions and secondly as a standard PSF image for
C convolutions. For the first we need a real version of
C the PSF image
      IF(NSTARS.GT.0) THEN
         CALL UDMGET(PX*PY,6,PSFR,ISTAT)
         IF(ISTAT.NE.0) THEN
            CALL UMSPUT(
     :  '! Unable to allocate memory for real PSF array',
     :           1,0,ISTAT)
            GO TO 99
         ELSE
            CALL COREAL(MEMD(PINPNT),MEMR(PSFR),PX,PY)
         ENDIF

         CALL EXPSFS(MEMR(PSFR),PX,PY,BX,BY,MEMD(PSFS),MEMD(TEMP),
     :            NPX,NPY,NSTARS,STARS,NSTARS,
     :                  INTYPE,FAC,PRF)

         CALL UDMFRE(PSFR,6,ISTAT)
      ENDIF
             
C Get working space arrays for the FFT
      CALL UDMGET(NX*NY*2,7,PSFFFT,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :   '! Unable to allocate memory for PSF FFT array',
     :            1,0,ISTAT)
         GO TO 99
      ENDIF
           
C Create the output data arrays and optionally the stars image
      CALL UCLGST('back',BACK,ISTAT)
      CALL UCLGST('starim',STARIM,ISTAT)

      IF(BACK.NE.' ') THEN
         CALL UIMCRE(BACK,7,2,DIMS,IDPS,ISTAT)

         IF(ISTAT.NE.0) THEN
            CALL UMSPUT(
     :     '! Unable to create output background image',
     :                 1,0,ISTAT)
            GO TO 99
         ENDIF
      ENDIF

      IF(STARIM.NE.' ' .AND. NSTARS.GT.0) THEN
         CALL UIMCRE(STARIM,7,2,DIMS,IDSI,ISTAT)

         IF(ISTAT.NE.0) THEN
            CALL UMSPUT(
     :     '! Unable to create output point-source image',
     :                 1,0,ISTAT)
            GO TO 99
         ENDIF
      ENDIF

C Add up the total in the data image
      CALL TOTAL(MEMD(PHT),NX,NY,SUM)

C Prepare the arrays for the FFT routine
C and do the FFTs of the PSF and rotated PSF
C
C At this point we also check that the PSF is normalised to
C a total of 1 - if it isn't we warn the user but continue
                
C First check for negative values, if there are any set them
C to zero
      CALL ZAPNEG(MEMD(PSFPNT),NX,NY,NNEG,VMIN,0.0D0)
      IF(NNEG.GT.0) THEN
         WRITE(CHARS,
     :       '(''! Warning, PSF '',
     :       '' contained '',I7,'' negative values, these'',
     :       '' have been set to zero.'')') NNEG
          CALL UMSPUT(CHARS,1,0,ISTAT)
      ENDIF
              
      CALL TOTAL(MEMD(PSFPNT),NX,NY,PT)
      IF(DABS(PT-1.0D0).GT.1.0E-5) THEN
         WRITE(CHARS,
     :    '(''! Warning, PSF for background totals '',
     :     F15.7,'' - renormalising.'')') PT
         CALL UMSPUT(CHARS,1,0,ISTAT)
         CALL MULC(MEMD(PSFPNT),NX,NY,1.0D0/PT,
     :                MEMD(PSFPNT))
      ENDIF
              
      CALL DFILL(MEMD(PSFPNT),NX,NY,MEMD(PSFFFT))
      CALL DFOURT(MEMD(PSFFFT),DIMS,2,-1,0,SCRATCH)
                                                         
C At this point we have finished with the PSF so it can be closed
C and the memory freed.
      CALL UIMCLO(IDP,ISTAT)
      CALL UDMFRE(PSFPNT,7,ISTAT)
           
C Create other arrays needed, workspace etc
      CALL UDMGET(NX*NY*2,7,WORK,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :      '! Unable to allocate memory for workspace array',
     :               1,0,ISTAT)
         GO TO 99
      ENDIF
             
C PSB is the background image 
      CALL UDMGET(NX*NY,7,PSB,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :      '! Unable to allocate memory for background array',
     :               1,0,ISTAT)
         GO TO 99
      ENDIF

C CH is the 'entropy' array
      CALL UDMGET(NX*NY,7,CH,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :      '! Unable to allocate memory for CH array',
     :               1,0,ISTAT)
         GO TO 99
      ENDIF

C PHS 
      CALL UDMGET(NX*NY,7,PHS,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :      '! Unable to allocate memory for PHS array',
     :              1,0,ISTAT)
        GO TO 99
      ENDIF

C PHSM
      CALL UDMGET(NX*NY,7,PHSM,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT(
     :      '! Unable to allocate memory for PHSM array',
     :              1,0,ISTAT)
        GO TO 99
      ENDIF
              
C Get space for and initialise the correction factor array
      CALL UDMGET(NX*NY,7,CF,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT('! Unable to create corr. fac. image',
     :                    1,0,ISTAT)
         GO TO 99
      ENDIF
            
C Prepare the first estimate - just a flat image with 
C half of the flux in the stars and half in the background.
C 
C If there are no specified star positions
C we can just fill the array with a constant
      IF(NSTARS.GT.0) THEN

C Write out the initial background value and check that it is
C positive
         IF(VERBOSE) THEN
          WRITE(CHARS,'(
     :     ''-Setting background in first estimate to: '',
     :                 F12.3)') 0.5*SUM/DBLE(NX*NY)
          CALL UMSPUT(CHARS,1,0,ISTAT)
         ENDIF

         CALL FILCON(MEMD(PSB),NX,NY,0.5*SUM/DFLOAT(NX*NY))

C Now put in the stars
         DO I=1,NSTARS
            STARS(5,I)=0.5*SUM/DBLE(NSTARS)
         ENDDO
      ELSE
         CALL FILCON(MEMD(PSB),NX,NY,SUM/DFLOAT(NX*NY))
      ENDIF
           
C Now we can start the iterative procedure
      CALL UCLGSI('niter',NITER,ISTAT)
                                      
      DO N=1,NITER
                  
         IF(VERBOSE) THEN
          CALL UMSPUT(' ',1,0,ISTAT)
            WRITE(CHARS,'(''# Starting iteration '',I5,
     :            ''/'',I5,''-----------'')') N,NITER
          CALL UMSPUT(CHARS,1,0,ISTAT)
         ENDIF

C Check for non-negativity
         CALL ZAPNEG(MEMD(PSB),NX,NY,NNEG,VMIN,0.0D0)
         IF(NNEG.GT.0) THEN
            WRITE(CHARS,'(''! Background image '',
     :         ''contains '',I7,'' negative values'')') NNEG
            CALL UMSPUT(CHARS,1,0,ISTAT)
            WRITE(CHARS,'(''--Minimum value: '',d20.10)') VMIN
            CALL UMSPUT(CHARS,1,0,ISTAT)
         ENDIF

C Check that everything is correctly normalised
         CALL TOTAL(MEMD(PSB),NX,NY,TT)

         WRITE(CHARS,'(''--Flux distribution, Stars: '',
     :    F6.2,''% Background: '',F6.2,''%.'')')
     :    100.0D0*(1.0-TT/SUM),100.0D0*(TT/SUM)
         CALL UMSPUT(CHARS,1,0,ISTAT)

         IF(DABS(TT/SUM-1.0D0).GT.1.0D-10) THEN
            WRITE(CHARS,'(
     :     ''- Sum of background image is '',D20.10)') TT
            CALL UMSPUT(CHARS,1,0,ISTAT)
         ENDIF

C Convolve the background image using FFTs
         CALL DCONV(MEMD(PSB),NX,NY,MEMD(WORK),
     :                 MEMD(PSFFFT),MEMD(PHB),1)

C If there is a stars list create the convolutions directly
         IF(NSTARS.GT.0) THEN
            CALL CONVLS(MEMD(PSFS),NPX,NPY,NSTARS,STARS,CS,
     :                  MEMD(PHS),NX,NY,+1)
         ENDIF
              
C Add the background and stars components
         CALL ADDIM(MEMD(PHB),MEMD(PHS),NX,NY,MEMD(PHSM))

C We now convolve the background with the smoothing
C kernel to get the default solution
         CALL DCONV(MEMD(PSB),NX,NY,MEMD(WORK),
     :                 MEMD(FPFFT),MEMD(CH),1)

C Calculate and display the residual information to assist closeness
C of fit assessment
         IF(VERBOSE) THEN
            CALL RESINF(MEMD(PHS),MEMD(PHT),
     :                    NX,NY,RMSRES,RESMAX,IRMAX,JRMAX)
               
            WRITE(CHARS,'(''--RMS residual: '',D10.4,
     :                 '' Max residual of '',D10.4,
     :                 '' at ('',I4,'','',I4,'')'')')
     :             RMSRES,RESMAX,IRMAX,JRMAX
            CALL UMSPUT(CHARS,1,0,ISTAT)
         ENDIF
              
C Calculate the sum of the continuum component
         CALL TOTAL(MEMD(PSB),NX,NY,SMB)

C and the stars
         SMS=0.0
         DO I=1,NSTARS
            SMS=SMS+STARS(5,I)
         ENDDO

C Divide the data by this convolution
         CALL DIVIDE(MEMD(PHT),MEMD(PHSM),NX,NY,MEMD(RU))

C If there is a floating prior we need to calculate the RP
C array
         CALL DIVIDE(MEMD(PSB),MEMD(CH),NX,NY,MEMD(RP))

C We now do the correlations to get the correction factors
C First the FFT one
         CALL DCONV(MEMD(RU),NX,NY,MEMD(WORK),
     :                 MEMD(PSFFFT),MEMD(CF),-1)

C then the default background image
         CALL DCONV(MEMD(RP),NX,NY,MEMD(WORK),
     :                 MEMD(FPFFT),MEMD(CFP),-1)

C and now the direct one using the small PSFs
         CALL CONVLS(MEMD(PSFS),NPX,NPY,NSTARS,STARS,CS,
     :                  MEMD(RU),NX,NY,-1)

C Calculate the entropy and likelihood and fill derivatives
C array
         CALL GETOF(MEMD(PSB),MEMD(PHSM),MEMD(PHT),MEMD(CH),
     :            NX,NY,MEMD(SD),MEMD(CFP),SS,HH)

C Calculate the objective function
         QQ=HH+AG*SS

C Some diagnostics
         IF(VERBOSE) THEN
            WRITE(CHARS,'(''--Log.Lik: '',
     :         E15.6,'' Ent: '',E15.6,
     :              '' Obj.Func: '',E20.12)')
     :       HH,SS,QQ
            CALL UMSPUT(CHARS,1,0,ISTAT)
         ENDIF

         IF(QQ.LT.LV) THEN
            WRITE(CHARS,
     :  '(''! Warning, objective function fell by '',D15.7)')
     :      LV-QQ
            CALL UMSPUT(CHARS,1,0,ISTAT)
         ELSE
            IF(VERBOSE.AND.N.GT.1) THEN
               WRITE(CHARS,
     :  '(''--Objective function increased by '',D15.7)')
     :         QQ-LV
               CALL UMSPUT(CHARS,1,0,ISTAT)
            ENDIF
         ENDIF
         LV=QQ

C Lagrange multiplier
         XMU1=-SUM/(SMB+SMS)
         XMU2=-AG*SS/(SMB+SMS)
         XMU=XMU1+XMU2

C Calculate the correction for the background (unaccelerated)
         CALL GTDPSB(MEMD(PSB),MEMD(CF),MEMD(SD),MEMD(DPSB),
     :               NX,NY,AG,XMU)

         XL=1.0

C Now apply the correction to the background image
         CALL BCORR(MEMD(PSB),MEMD(DPSB),NX,NY,XL)

C and finally to the stars
         DO I=1,NSTARS
            STARS(5,I)=STARS(5,I)*(1.0+XL*(CS(I)+XMU))
            WRITE(CHARS,
     :  '('' Star # '',I5,'' Flux: '',G12.4,'' Input: '',
     :       G12.4)')
     :       I,STARS(5,I)/FAC(I),STARS(3,I)
            IF(I.LT.6) THEN
               CALL UMSPUT(CHARS,1,0,ISTAT)
            ELSE IF(I.EQ.6) THEN
                 CALL UMSPUT('...',1,0,ISTAT)
            ENDIF
         ENDDO
      ENDDO

C Then the background image if requested
      IF(BACK.NE.' ') THEN
         IF(VERBOSE)
     :     CALL UMSPUT(
     :       '-Writing output smooth background image: '//BACK
     :                 ,1,0,ISTAT)
                               
         CALL UIPS2D(IDPS,1,NX,1,NY,MEMD(PSB),ISTAT)
         CALL UIMCLO(IDPS,ISTAT)
      ENDIF

C and the stars image if requested
      IF(STARIM.NE.' ' .AND. NSTARS.GT.0) THEN
         IF(VERBOSE)
     :     CALL UMSPUT(
     :       '-Writing output point-sources image: '//STARIM
     :                 ,1,0,ISTAT)

         CALL UIPS2D(IDSI,1,NX,1,NY,MEMD(PHS),ISTAT)
         CALL UIMCLO(IDSI,ISTAT)
      ENDIF

C And now the text list
      IF(OUTTAB.NE.' '.AND.NSTARS.GT.0) THEN

C First we write in some information which may be of interest - mainly
C to record how the file was produced
         WRITE(8,'(''# Created by CPLUCY V0.21'')')
         WRITE(8,'(''#'')')
         WRITE(8,'(''# Input data image: '',A30)') DATA
         WRITE(8,'(''# Input PSF image: '',A50)') PSF
         WRITE(8,'(''# Fractional entropy strength: '',F10.4)') AG
         WRITE(8,'(''# Number of interations: '',I6)') NITER
         WRITE(8,'(''# Output background image: '',A50)') BACK 
         WRITE(8,'(''# Input star list: '',A50)') FILE
         WRITE(8,'(''# Sigma of smoothing kernel: '',F10.3)') GW

         IF(ACCEL) THEN
            WRITE(8,'(''# Acceleration was used.'')')
         ELSE
            WRITE(8,'(''# Acceleration was not used.'')')
         ENDIF

         WRITE(8,'(''# Magnitude zero point: '',F10.3)') MAGZERO

         WRITE(8,
     : '(''# Number  X        Y       InputFlux'',
     "   '' MeasuredFlux MeasuredMag'')')

         CALL WRITAB(STARS,NSTARS,MAGZERO,NPSF,FAC)
         IF(VERBOSE)
     :     CALL UMSPUT(
     :       '-Writing output photometry list: '//OUTTAB
     :                 ,1,0,ISTAT)
      ENDIF

99    CONTINUE
               
C Close the text file
      CLOSE(8)

C Close all the images
      CALL UIMCLO(IDD,ISTAT)
           
C Free all the dynamic arrays
      CALL UDMFRE(PHT,7,ISTAT)
      CALL UDMFRE(PSFFFT,7,ISTAT)
      CALL UDMFRE(PHS,7,ISTAT)
      CALL UDMFRE(PSB,7,ISTAT)
      CALL UDMFRE(CF,7,ISTAT)
      CALL UDMFRE(WORK,7,ISTAT)
      CALL UDMFRE(RU,7,ISTAT)
      CALL UDMFRE(FPFFT,7,ISTAT)
      CALL UDMFRE(RP,7,ISTAT)
      CALL UDMFRE(CFP,7,ISTAT)

C Close down MIDAS
CM    CALL STSEPI    !M
                       
      END
         
      SUBROUTINE MULC(IN1,NX,NY,F,OUT)
C
C Just multiply one array by a constant
C
      IMPLICIT NONE
                   
      INTEGER NX,NY,I,J
      DOUBLE PRECISION IN1(NX,NY),OUT(NX,NY),F
                                              
      DO J=1,NY
         DO I=1,NX
            OUT(I,J)=IN1(I,J)*F
         ENDDO
      ENDDO
           
      RETURN
      END
         
      SUBROUTINE DIVIDE(IN1,IN2,NX,NY,OUT)
C
C Just divide one array by another of the same size
C If the denominator is zero the result is set to zero also.
C
      IMPLICIT NONE
                   
      INTEGER NX,NY,I,J
      DOUBLE PRECISION IN1(NX,NY),IN2(NX,NY),OUT(NX,NY)
      INTEGER NZERO,ISTAT
      CHARACTER*80 CHARS

      NZERO=0
                                                       
      DO J=1,NY
         DO I=1,NX
            IF(IN2(I,J).EQ.0.0D0) THEN
               OUT(I,J)=0.0D0
               NZERO=NZERO+1
            ELSE
               OUT(I,J)=IN1(I,J)/IN2(I,J)
            ENDIF
         ENDDO
      ENDDO
           
      IF(NZERO.GT.0) THEN
         WRITE(CHARS,
     : '(''! '',i10,'' attempts to divide by zero.'')') NZERO
         CALL UMSPUT(CHARS,1,0,ISTAT)
      ENDIF

      RETURN
      END
         
      SUBROUTINE TOTAL(DATA,NX,NY,TOT)
C
C Add up all elements in an image
C
      IMPLICIT NONE
                   
      INTEGER NX,NY,I,J
      DOUBLE PRECISION DATA(NX,NY),TOT
                                      
      TOT=0.0D0
      DO J=1,NY
         DO I=1,NX
             TOT=TOT+DATA(I,J)
         ENDDO
      ENDDO
           
      RETURN
      END
         
      SUBROUTINE WRITAB(STARS,NSTARS,MAGZERO,NPSF,FAC)
C
C Write out the final results as an ASCII table
C It is assumed that this file has already been opened
C on FORTRAN unit 8.
C
      IMPLICIT NONE

      INTEGER NSTARS
      DOUBLE PRECISION STARS(5,NSTARS),MAGZERO
      DOUBLE PRECISION FAC(NSTARS)

      INTEGER I,IX,IY,K,NPSF

      DO I=1,NSTARS
         IX=NINT(STARS(1,I))
         IY=NINT(STARS(2,I))

         IF(STARS(3,I).NE.0.0D0 .AND. STARS(5,I).GT.0.0D0) THEN
          IF(NPSF.GT.1) THEN
           WRITE(8,'(I6,1X,F7.2,1X,F7.2,1X,
     :         F12.2,1X,F12.2,1X,F7.3,1X,I5)')
     :         I,(STARS(K,I),K=1,3),STARS(5,I)/FAC(I),
     :         MAGZERO-2.5*DLOG10(STARS(5,I)/FAC(I)),
     :         NINT(STARS(4,I))
          ELSE
           WRITE(8,'(I6,1X,F7.2,1X,F7.2,1X,
     :         F12.2,1X,F12.2,1X,F7.3)')
     :         I,(STARS(K,I),K=1,3),STARS(5,I)/FAC(I),
     :         MAGZERO-2.5*DLOG10(STARS(5,I)/FAC(I))
          ENDIF
         ELSE
          IF(NPSF.GT.1) THEN
           WRITE(8,'(I6,1X,F7.2,1X,F7.2,1X,
     :         F12.2,1X,F12.2,1X,F7.3,1X,I5)')
     :         I,(STARS(K,I),K=1,3),0.0D0,
     :         -100.0,NINT(STARS(4,I))
          ELSE
           WRITE(8,'(I6,1X,F7.2,1X,F7.2,1X,
     :         F12.2,1X,F12.2,1X,F7.3)')
     :         I,(STARS(K,I),K=1,3),0.0D0,-100.0
          ENDIF
         ENDIF

      ENDDO

      END

      SUBROUTINE MULIM(A,B,NX,NY,OUT)
C
C Multiply an image by another
C
      IMPLICIT NONE

      INTEGER NX,NY,I,J
      DOUBLE PRECISION A(NX,NY),B(NX,NY),OUT(NX,NY)

      DO J=1,NY
         DO I=1,NX
            OUT(I,J)=A(I,J)*B(I,J)
         ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE ADDIM(A,B,NX,NY,OUT)
C
C Add an image to another
C
      IMPLICIT NONE

      INTEGER NX,NY,I,J
      DOUBLE PRECISION A(NX,NY),B(NX,NY),OUT(NX,NY)

      DO J=1,NY
         DO I=1,NX
            OUT(I,J)=A(I,J)+B(I,J)
         ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE RESINF(IM1,IM2,NX,NY,RMSRES,RESMAX,
     :                   IRMAX,JRMAX)
C
C Calculate the RMS residual between two images and the
C largest residual.
C
      IMPLICIT NONE
                   
      INTEGER NX,NY,IRMAX,JRMAX
      DOUBLE PRECISION RMSRES,RESMAX,IM1(NX,NY),IM2(NX,NY)
                                                          
      INTEGER I,J
      DOUBLE PRECISION RES,T
                            
      T=0.0D0
      RESMAX=0.0D0
                  
      DO J=1,NY
         DO I=1,NX
            RES=IM1(I,J)-IM2(I,J)
            T=T+RES*RES
            IF(DABS(RES).GT.DABS(RESMAX)) THEN
               RESMAX=RES
               IRMAX=I
               JRMAX=J
            ENDIF
         ENDDO
      ENDDO
           
      RMSRES=DSQRT(T/DBLE(NX*NY))
                                 
      RETURN
      END
         
      SUBROUTINE ZAPNEG(DATA,NX,NY,NNEG,VMIN,VZAP)
C
C Find negative points in an array and set them to VZAP
C Also return the number of negative points and the smallest
C value.
C
      IMPLICIT NONE
                   
      INTEGER NX,NY,NNEG
      DOUBLE PRECISION DATA(NX,NY),VMIN,VZAP
      INTEGER I,J
                 
      NNEG=0
      VMIN=1.0D20
            
      DO J=1,NY
         DO I=1,NX
            IF(DATA(I,J).LT.0.0D0) THEN
               NNEG=NNEG+1
               IF(DATA(I,J).LT.VMIN) VMIN=DATA(I,J)
               DATA(I,J)=VZAP
            ENDIF
         ENDDO
      ENDDO
           
      RETURN
      END

      SUBROUTINE GETSTR(FILE,STARS,NX,NY,NPSF,
     :                  MAXSTR,MAGZERO,NSTARS,ISTAT)
C
C Read in a list of free format star positions (in pixels)
C
C Each line has four entries:
C  - the X position
C  - the Y position
C  - some sort of relative brightness expressed as a magnitude
C    relative to MAGZERO
C  - the number of the matching PSF (only if NPSF>1)
C
C Comments may be put in a line after a ! or #, blank lines
C are ignored.
C
      IMPLICIT NONE

      INTEGER NX,NY
      INTEGER MAXSTR,NSTARS,ISTAT,I,NPSF
      DOUBLE PRECISION STARS(5,MAXSTR),MAGZERO
      CHARACTER*80 FILE,CHARS,LINE

      INTEGER N,IOS,NN

      ISTAT=0
      NSTARS=0

C First open the file
      OPEN(12,FILE=FILE,STATUS='OLD',IOSTAT=IOS)
      IF(IOS.NE.0) THEN
         CALL UMSPUT('! Unable to open star positions list file',
     :               1,0,ISTAT)
         ISTAT=1
         RETURN
      ENDIF

      N=0

C Read in the values until there are no more
      DO NN=1,MAXSTR+1000
         IF(N.EQ.MAXSTR+1) THEN
            CALL UMSPUT('! Too many stars!',1,0,ISTAT)
            ISTAT=1
            GO TO 99 
         ENDIF

C Read a line from the file
         READ(12,'(A80)',END=99) LINE 

C Remove comments
         DO I=1,80
            IF(LINE(I:I).EQ.'#' .OR.
     :         LINE(I:I).EQ.'!') THEN
               LINE(I:80)=' '
            ENDIF
         ENDDO

         IF(LINE.NE.' ') THEN
            N=N+1

C If there is only one PSF we don't try to get the PSF
C number, we just default it to 1
            IF(NPSF.GT.1) THEN
             READ(LINE,*,IOSTAT=IOS) (STARS(I,N),I=1,4)
            ELSE
             READ(LINE,*,IOSTAT=IOS) (STARS(I,N),I=1,3)
             STARS(4,N)=1.0D0
            ENDIF
 
            IF(IOS.NE.0) THEN
               CALL UMSPUT('! Error reading StarList file',
     :                     1,0,ISTAT)
               GO TO 99
            ENDIF

C Convert from magnitudes to fluxes
            STARS(3,N)=10.0**(-0.4*(STARS(3,N)-MAGZERO))

C Check for out of bounds stars
            IF(NINT(STARS(1,N)).LT.1 .OR.
     :         NINT(STARS(1,N)).GT.NX .OR.
     :         NINT(STARS(2,N)).LT.1 .OR.
     :         NINT(STARS(2,N)).GT.NY) THEN
               STARS(3,N)=0.0D0
               STARS(4,N)=0.0D0
               WRITE(CHARS,'(''! Warning, star '',I5,
     :            '' at '',2F7.2,
     :      '' is outside image, ignored.'')') 
     :         STARS(1,N),STARS(2,N),N
               CALL UMSPUT(CHARS,1,0,ISTAT)
            ENDIF

            IF(NINT(STARS(4,N)).LT.0 .OR.
     :         NINT(STARS(4,N)).GT.NPSF) THEN
               STARS(3,N)=0.0D0
               STARS(4,N)=0.0D0
               WRITE(CHARS,'(''! Warning, star '',I5,
     :            '' has invalid PSF number, ignored.'')') N
               CALL UMSPUT(CHARS,1,0,ISTAT)
            ENDIF

         ENDIF

         NSTARS=N
      ENDDO

99    CONTINUE
      CLOSE(12)

      RETURN
      END

      SUBROUTINE FGAUSS(IMAGE,NX,NY,GW)
C
C Fill the centre of an empty image with a 2d circular
C gaussian of sigma=GW. We go out as far as 5 sigma.
C
      IMPLICIT NONE

      INTEGER NX,NY
      DOUBLE PRECISION IMAGE(NX,NY),GW

C Local Variables etc
      DOUBLE PRECISION R2,S5S,T,DY,DX
      INTEGER I,J

      S5S=25.0D0*GW*GW
      T=0.0D0

      DO J=1,NY
         DY=DBLE(J-NY/2)
         DO I=1,NX
            DX=DBLE(I-NX/2)
            R2=DX*DX+DY*DY
            IF(R2.LT.S5S) THEN
               IMAGE(I,J)=EXP(-R2/(2.0D0*GW*GW))
               T=T+IMAGE(I,J)
            ELSE
               IMAGE(I,J)=0.0D0
            ENDIF
         ENDDO
      ENDDO

      DO J=1,NY
         DO I=1,NX
            IMAGE(I,J)=IMAGE(I,J)/T
         ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE COPYIM(IN,NX,NY,OUT)
C
C Copy an image to another of the same size
C
      IMPLICIT NONE

      INTEGER NX,NY
      DOUBLE PRECISION IN(NX,NY),OUT(NX,NY)

      INTEGER I,J

      DO J=1,NY
         DO I=1,NX
            OUT(I,J)=IN(I,J)
         ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE SMOOTH(IN,OUT,NX,NY,SIG)
C
C Simple smoothing using direct convolution and a box size
C of 7 by 7 - ie only usable for small SIG
C
      IMPLICIT NONE

      INTEGER NX,NY,I,J
      DOUBLE PRECISION IN(NX,NY),OUT(NX,NY),SIG,BOX(7,7)
      DOUBLE PRECISION X,Y,T

C First fill the box
      T=0.0D0
      DO J=1,7
         Y=DBLE(J-3)
         DO I=1,7
            X=DBLE(I-3)
            BOX(I,J)=EXP(-(X*X+Y*Y)/(2.0D0*SIG*SIG))
            T=T+BOX(I,J)
         ENDDO
      ENDDO

C Normalise
      DO J=1,7
         DO I=1,7
            BOX(I,J)=BOX(I,J)/T
         ENDDO
      ENDDO

C Convolve
      CALL DIRCON(IN,NX,NY,BOX,7,7,OUT,1)

      RETURN
      END

      SUBROUTINE CONVLS(PSFS,NPX,NPY,NP,STARS,CS,
     :                  IM,NX,NY,FLAG)
C
C Convolve or correlate a set of stars held as a position
C intensity list.
C
C First try as part of the PLUCY upgrade work, January 1997
C
      IMPLICIT NONE

      INTEGER NPX,NPY,NP,NX,NY,FLAG
      DOUBLE PRECISION PSFS(NPX,NPY,NP),IM(NX,NY)
      DOUBLE PRECISION STARS(5,NP),CS(NP)

C Local variables
      INTEGER I,J,N,II,JJ,IXS,IYS
      DOUBLE PRECISION S

C Convolution 
      IF(FLAG.EQ.1) THEN

C Initialise the output array
         DO J=1,NY
            DO I=1,NX
               IM(I,J)=0.0
            ENDDO
         ENDDO

C Add in the scaled stars
         DO N=1,NP
            IXS=NINT(STARS(1,N))-NPX/2-1
            IYS=NINT(STARS(2,N))-NPY/2-1
            S=STARS(5,N)

            DO J=1,NPY
               JJ=J+IYS
               IF(JJ.GE.1 .AND. JJ.LE.NY) THEN
                DO I=1,NPX
                  II=I+IXS
                  IF(II.GE.0 .AND. II.LE.NX) THEN
                     IM(II,JJ)=IM(II,JJ)+PSFS(I,J,N)*S
                  ENDIF
                ENDDO
               ENDIF
            ENDDO
         ENDDO
      ELSE

C Now the case of correlating, this is a little less obvious
         DO N=1,NP   
            CS(N)=0.0

C Sum the product of the stars and the image supplied
            IXS=NINT(STARS(1,N))-NPX/2-1
            IYS=NINT(STARS(2,N))-NPY/2-1

            DO J=1,NPY
               JJ=J+IYS
               IF(JJ.GE.1 .AND. JJ.LE.NY) THEN
                DO I=1,NPX
                  II=I+IXS
                  IF(II.GE.0 .AND. II.LE.NX) THEN
                     CS(N)=CS(N)+IM(II,JJ)*PSFS(I,J,N)
                  ENDIF
                ENDDO
               ENDIF
            ENDDO
         ENDDO
      ENDIF 

      RETURN
      END

      SUBROUTINE EXPSFS(PSF,NX,NY,BX,BY,
     :                  PSFS,TEMP,PX,PY,NP,STARS,NSTARS,
     :                  INTYPE,FAC,PRF)
C
C Extract and interpolate the small star PSFs from the big
C image. The interpolation is done using the standard IRAF
C routine MRIEVL which can handle several interpolation schemes,
C defined by the value of the INTYPE parameter.
C
C Added optional WFPC pixel-response function, October 1997
C
      IMPLICIT NONE

      INTEGER NX,NY,PX,PY,NP,NSTARS,INTYPE,BX,BY
      REAL PSF(NX,NY),X,Y,MRIEVL,AI,AJ,OFFI,OFFJ
      DOUBLE PRECISION PSFS(PX,PY,NP),TEMP(PX,PY)
      DOUBLE PRECISION STARS(5,NSTARS),FAC(NSTARS)

      INTEGER I,J,N
      DOUBLE PRECISION T
      LOGICAL PRF

C First we calculate some tricky offsets so that the big pixels
C are sampled uniformly and not shifted
      OFFI=(REAL(BX)-1.0)/2.0/REAL(BX)
      OFFJ=(REAL(BY)-1.0)/2.0/REAL(BY)

      DO N=1,NSTARS
         DO J=1,PY
            DO I=1,PX
               PSFS(I,J,N)=0.0
            ENDDO
         ENDDO

         DO AJ=1.0-OFFJ,REAL(PY)+OFFJ,1.0/REAL(BY)
            Y=REAL(BY)*(AJ-REAL(PY/2+1))+REAL(NY/2)-
     :      REAL(BY)*(REAL(STARS(2,N))-REAL(NINT(STARS(2,N))))
            DO AI=1.0-OFFI,REAL(PX)+OFFI,1.0/REAL(BX)
               X=REAL(BX)*(AI-REAL(PX/2+1))+REAL(NX/2)-
     :         REAL(BX)*(REAL(STARS(1,N))-REAL(NINT(STARS(1,N))))

               I=NINT(AI)
               J=NINT(AJ)
               PSFS(I,J,N)=PSFS(I,J,N)+
     :            DBLE(MRIEVL(X,Y,PSF,NX,NY,NX,INTYPE))
            ENDDO
         ENDDO

         T=0.0
         DO J=1,PY
            DO I=1,PX
               T=T+PSFS(I,J,N)
            ENDDO
         ENDDO
       DO J=1,PY
          DO I=1,PX
             PSFS(I,J,N)=PSFS(I,J,N)/T
          ENDDO
       ENDDO

C If we want to do the WFPC PRF convolution we do it here,
C explicitly as the PSFs and the kernel are both small
       IF(PRF) THEN
        DO J=1,PY
          DO I=1,PX
             TEMP(I,J)=PSFS(I,J,N)
          ENDDO
        ENDDO

        CALL PRFCON(TEMP,PX,PY,PSFS(1,1,N))
       ENDIF

       FAC(N)=T
      ENDDO

      RETURN
      END

      SUBROUTINE COREAL(IN,OUT,NX,NY)
C
C Convert an image from double to real
C
      IMPLICIT NONE
      INTEGER NX,NY,I,J
      REAL OUT(NX,NY)
      DOUBLE PRECISION IN(NX,NY)
   
      DO J=1,NY
         DO I=1,NX
            OUT(I,J)=REAL(IN(I,J))
         ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE GETOF(PSB,PHSM,PHT,CH,
     :            NX,NY,SD,CFP,SS,HH)
C
C Compute the entropy and likelihood terms as well as
C filling the partial derivatives array
C
      IMPLICIT NONE

      INTEGER NX,NY
      DOUBLE PRECISION PSB(NX,NY),PHSM(NX,NY)
      DOUBLE PRECISION PHT(NX,NY),CH(NX,NY)
      DOUBLE PRECISION SD(NX,NY),CFP(NX,NY),SS,HH,ARG

      INTEGER I,J,ISTAT

      SS=0.0
      HH=0.0

      DO J=1,NY
         DO I=1,NX
            SD(I,J)=-1.0+CFP(I,J)
            IF(PSB(I,J).LE.0.0 .OR. CH(I,J).LE.0.0) GO TO 11
            ARG=PSB(I,J)/CH(I,J)
            IF(ARG.LE.0.0) CALL UMSPUT('! ARG negative...',1,0,ISTAT)
            SS=SS-PSB(I,J)*DLOG(PSB(I,J)/CH(I,J))
            SD(I,J)=SD(I,J)-DLOG(PSB(I,J)/CH(I,J))
 11         CONTINUE

            IF(PHT(I,J).EQ.0.0) GO TO 22
            IF(PHSM(I,J).LE.0.0) CALL UMSPUT(
     :          '! PHSM negative...',1,0,ISTAT)
            HH=HH+PHT(I,J)*DLOG(PHSM(I,J))
 22         CONTINUE
         ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE BCORR(PSB,DPSB,NX,NY,XL)
C
C Apply the correction to the background image
C 
C XL is the acceleration factor
C
      IMPLICIT NONE

      INTEGER NX,NY
      DOUBLE PRECISION PSB(NX,NY),DPSB(NX,NY)
      DOUBLE PRECISION XL

      INTEGER I,J

      DO J=1,NY
         DO I=1,NX
            PSB(I,J)=PSB(I,J)+XL*DPSB(I,J)
         ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE CTEST(DPSB,DPSBH,DPSBS,NX,NY,
     :                 DPSS,DPSSH,DPSSS,NSTARS,
     :                 TB,TS)
C
C Test convergence by assessing "anti-parallelism"
C Based on Leon Lucy routine of the same name,
C February 1997.
C
      IMPLICIT NONE

      INTEGER NX,NY,NSTARS

      DOUBLE PRECISION DPSB(NX,NY),DPSBH(NX,NY),DPSBS(NX,NY)
      DOUBLE PRECISION DPSS(NSTARS),DPSSH(NSTARS),DPSSS(NSTARS)
      DOUBLE PRECISION SM1,SM2,SM3,TB,TS
      INTEGER I,J

      SM1=0.0
      SM2=0.0
      SM3=0.0

C First for the background
      DO J=1,NY
         DO I=1,NX
            SM1=SM1+DPSB(I,J)**2
            SM2=SM2+DPSBH(I,J)**2
            SM3=SM3+DPSBS(I,J)**2
         ENDDO
      ENDDO

      TB=DSQRT(SM1)/(DSQRT(SM2)+DSQRT(SM3))

C and then for the stars
      DO I=1,NSTARS
         SM1=SM1+DPSS(I)**2
         SM2=SM2+DPSSH(I)**2
         SM3=SM3+DPSSS(I)**2
      ENDDO

      TS=DSQRT(SM1)/(DSQRT(SM2)+DSQRT(SM3))

      RETURN
      END

      SUBROUTINE PLIM(PSB,DPSB,NX,NY,PSS,DPSS,NSTARS,XLM)
C
C Find the maximum possible acceleration factor which is
C consistent with non-negativity.
C
      IMPLICIT NONE
  
      INTEGER NX,NY,NSTARS

      DOUBLE PRECISION PSB(NX,NY),DPSB(NX,NY)
      DOUBLE PRECISION PSS(NSTARS),DPSS(NSTARS),XLM
      DOUBLE PRECISION XLJ,XLN

      INTEGER I,J

      XLM=1.0D30

C First the background
      DO J=1,NY
         DO I=1,NX
            IF(DPSB(I,J).LT.0.0) THEN
               XLJ=-PSB(I,J)/DPSB(I,J)
               XLM=DMIN1(XLJ,XLM)
            ENDIF
         ENDDO
      ENDDO
            
C and then the stars
      DO I=1,NSTARS
         IF(DPSS(I).LT.0.0) THEN
            XLN=-PSS(I)/DPSS(I)
            XLM=DMIN1(XLN,XLM)
         ENDIF
      ENDDO

      RETURN
      END

      SUBROUTINE GTDPSB(PSB,CF,SD,DPSB,NX,NY,AG,XMU)
C
C Calculate the unaccelerated increment in the background
C image
C
      IMPLICIT NONE

      INTEGER NX,NY
      DOUBLE PRECISION PSB(NX,NY),CF(NX,NY),SD(NX,NY)
      DOUBLE PRECISION DPSB(NX,NY),AG,XMU

      INTEGER I,J

      DO J=1,NY
         DO I=1,NX
            DPSB(I,J)=PSB(I,J)*(CF(I,J)+AG*SD(I,J)+XMU)
         ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE PSFBLK(PIN,PX,PY,PSF,NX,NY,BX,BY)
C
C Create a PSF the same size as the data by inserting
C a blocked-up version of the supplied PSF into an empty
C image
C
      IMPLICIT NONE
      INTEGER PX,PY,NX,NY,BX,BY
      DOUBLE PRECISION PIN(PX,PY),PSF(NX,NY)

      INTEGER I,J,N,K,II,JJ,JA

C First zero the output
      DO J=1,NY
         DO I=1,NX
            PSF(I,J)=0.0
         ENDDO
      ENDDO

      JJ=NY/2-PY/BY/2+1

      DO J=1,PY/BY
         DO K=1,BY
            II=NX/2-PX/BX/2+1
            JA=(J-1)*BY+K
            DO I=1,PX/BX
               DO N=1,BX
                  PSF(II,JJ)=PSF(II,JJ)+PIN((I-1)*BX+N,JA)
               ENDDO
               II=II+1
            ENDDO
         ENDDO
         JJ=JJ+1
      ENDDO

      RETURN
      END

      SUBROUTINE PRFCON(IN,NX,NY,OUT)
C
C Convolve a PSF explicitly with a hard-wired PRF
C This version uses the PRF for WFPC2 used by Tiny Tim.
C
C Richard Hook, October 1997
C
      IMPLICIT NONE

      INTEGER I,J,NX,NY
      DOUBLE PRECISION IN(NX,NY),OUT(NX,NY)

      DO J=1,NY
         DO I=1,NX
            IF(I.EQ.1 .OR. I.EQ.NX .OR.
     :         J.EQ.1 .OR. J.EQ.NY) THEN
               OUT(I,J)=IN(I,J)
            ELSE
               OUT(I,J)=0.0125*IN(I-1,J-1)+
     :               0.05*IN(I-1,J)+
     :               0.0125*IN(I-1,J+1)+
     :               0.05*IN(I,J-1)+
     :               0.75*IN(I,J)+
     :               0.05*IN(I,J+1)+
     :               0.0125*IN(I+1,J-1)+
     :               0.05*IN(I+1,J)+
     :               0.0125*IN(I+1,J+1)
            ENDIF
         ENDDO
      ENDDO

      RETURN
      END

      subroutine arstat(data,nx,ny)
*
* Display some stats of a data array
*
      integer nx,ny
      double precision data(nx,ny),dmin,dmax,v,t
      character*80 chars

      t=0.0d0
      dmax=-1.0d20
      dmin=1.0d20

      do j=1,ny
         do i=1,nx
            v=data(i,j)
            if(v.gt.dmax) then
               dmax=v
               ixmax=i
               iymax=j
            else if(v.lt.dmin) then
               dmin=v
               ixmin=i
               iymin=j
            endif
            t=t+v
         enddo
      enddo

      write(chars,'('' Max of '',g12.5,'' at ('',i4,'','',
     :              i4,'')'')') dmax,ixmax,iymax
      call umsput(chars,1,0,istat)

      write(chars,'('' Min of '',g12.5,'' at ('',i4,'','',
     :              i4,'')'')') dmin,ixmin,iymin
      call umsput(chars,1,0,istat)

      write(chars,'('' 1st 5: '',5(1x,g12.5))') (data(k,1),
     :      k=1,5)
      call umsput(chars,1,0,istat)

      write(chars,'('' Last 5: '',5(1x,g12.5))') (data(k,ny),
     :      k=1,5)
      call umsput(chars,1,0,istat)

      end
