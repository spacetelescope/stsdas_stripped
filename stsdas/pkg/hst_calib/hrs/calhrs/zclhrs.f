        SUBROUTINE ZCLHRS
*
*  Module number:
*
*  Module name: ZCLHRS
*
*  Keyphrase:
*  ----------
*       Standard reduction of GHRS data
*
*  Description:
*  ------------
*       This routine performs the pipeline processing of GHRS
*       data.  It requires as input the rootname of the observation.
*       It will use the following input data files as specified by
*       <rootname>.<qual> where qual has the following values.
*
*             <qual>            type
*               d0h     Raw science data for 500 diodes on main array
*               x0h     Special diodes and 12 words of engineering trailer
*               shh     standard header packet file
*               ulh     unique data log file
*               q0h     data quality vector for d0h file
*               xqh     data quality vector for x0h file
*
*       The associated data file for each is obtained by replacing the
*       last character in the qualifer from 'h' to 'd'.
*
*       The routine will process the data and place the results in
*       files specified by <rootname>.<qual> where <qual> is the file
*       qualifier with the following posible values.
*
*             <qual>            type
*               c0h     wavelength vector(s)
*               c1h     flux vector(s)
*               cqh     data quality vector(s)
*               c2h     propagated statistical error vector(s)
*               c3h     special diodes 
*               c4h     special diodes data quality
*               c5h     background subtraction
*
*       No processing is performed on input data files which are
*       target acquisition observations.
*
*       The following two steps of the reduction are always performed
*       for accumulation mode or direct downlink data.
*               1) conversion to count rates
*               2) data quality initialization (provided the
*                       data quality initialization file in the
*                       input header (keyword DQIDFILE) is not
*                       blank.
*       The performance of the remaining reduction steps are controlled
*       by header keywords of the form <step>_CORR.  These keywords
*       may have the value 'PERFORM' or 'OMIT'.  The following reduction
*       steps are available.
*
*               <step>          description
*		EXP	divide by exposure time (convert to count rates)
*               PPC     paired-pulse correction
*               DIO     diode non-uniformity correction
*		MAP	perform photocathode mapping function
*               PHC     photocathode nonuniformity correction
*               DOP     correct for on-board doppler compensation when
*                               removing photocathode non-uniformities
*                               and vignetting.
*               ADC     application of dispersion coefficients
*               INC     incidence angle correction for the large
*                               science aperture
*               VAC     vacumm to air correction to the wavelengths
*               HEL     wavelength correction for the earths motion
*		MER	merge substep bins
*               BCK     background subtraction 
*               BMD     background subtraction using count rate model 
*               MNF     mean filter the observed background
*               MDF     median filter the observed background
*               PLY     polynomial fit to the observed background
*               VIG     correct for vignetting
*               ECH     echelle ripple correction
*               FLX     conversion to absolute flux units
*
*       The following reference files as specfied by header keywords
*       are used for the reduction.
*               DIOHFILE - diode response header file
*               PHCHFILE - photocathode response header file
*               VIGHFILE - vignetting header file
*               ABSHFILE - absolute sensitivity file
*               NETHFILE - wavelengths for ABSHFILE
*               DQIHFILE - data quality initialization file
*               SAAHFILE - SAA contour mask Model 7
*
*       In addition the following reference tables specified by CL
*       parameters are used.
*
*               CCR0 - aperture sizes
*               CCR1 - photocathode line mapping parameters
*               CCR2 - photocathode sample parameters
*               CCR3 - detector parameters
*               CCR5 - spectral order constants
*               CCR6 - dispersion constants
*               CCR7 - thermal constants
*               CCR8 - incidence angle coefficients
*               CCR9 - Echelle ripple constants
*               CCRA - More echelle ripple constants
*               CCRB - spectral lamps to small science offsets.
*               CCRC - global wavelength coefficients.
*               CCRD - photocathode blemish table.
*               CCRE - background count rate model
*
*  FORTRAN name: zclhrs
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       rootname.<qual>         I/O     input and output file names
*       CCR0,CCR1,...,CCRE      Input   input table names
*       DIOHFILE                         diode response header file
*       PHCHFILE                         photocathode response header file
*       VIGHFILE                         vignetting header file
*       ABSHFILE                         absolute sensitivity file
*       NETHFILE                         wavelengths for ABSHFILE
*       DQIHFILE                         data quality initialization file
*       SAAHFILE                         SAA contour mask Model 7
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zclprc, zclopn, zclrel, zgetkw, zrdflg
*  SDAS:
*       uuclgs, ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jan 88  D. Lindler      Designed and coded
*			S. Hulbert	Added output files for special diodes
*			S. Hulbert	check for no science data
*     1.1       Jun 90  S. Hulbert 	Changes in header updating due to
*					changind data format
*     1.2       Oct 90  S. Hulbert 	added BUNITS
* 1.1.2.1	Feb 91	S. Hulbert	Fixed bugs in zclwrt and zcldop and 
*					started	new version number format
*   1.1.3	May 91	S. Hulbert	New headers
*					Changed grating names
*					Added cubic dispersion term
*					Reordered processing steps
*					Added scattered light corrections
*					Added polynomial background smoothing
*		Aug 91 	S. Hulbert	Bug fix in zclbkg
* 1.1.3.1       Sep 91  S. Hulbert      Don't look for OBSMODE in shp header
* 1.1.3.2       Sep 91  S. Hulbert      Use dynamic memory allocation when
*					reading tables. Normalize ripple
*					function.
* 1.2.3.2	Dec 91	S. Hulbert	Bug fix in zrccr6. New version number
*					for release 1.2
* 1.2.3.2.1	1Jan92	S. Hulbert	Bug fix in zrccr6, zrccr8, zrccr9--
*					problem with deallocating memory
*					Changed declaration in zrdabs
* 1.2.3.2.2	10Feb92	S. Hulbert	Bug fix in zclppc, zclwav, zgetkw and
*					zcladc
* 1.2.3.2.3     18May92 J. Eisenhamer   Fixed bug in calibrating long scan
*                                       instrument calibration data.  Added
*                                       routine zexdef which is called int
*                                       zclprc.
* 1.3.1         1Feb94  J. Eisenhamer   Added global coefficient wavelength
*                                       solution.
* 1.3.2         10Feb94 J. Eisenhamer   Added new output product for
*                                       background subtraction.
* 1.3.3         16Feb94 J. Eisenhamer   Implemented photocathode blemish
*                                       data quality flagging.
* 1.3.4         11Mar94 J. Eisenhamer   Implemented side 1 calibrations using
*                                       side 2 carrousel positions.
* 1.3.4.2        7Apr94 J. Eisenhamer   Removed hardwired constants for
*                                       wavelength guessing and ripple removal
* 1.3.5          7Apr94 J. Eisenhamer   Added PEDIGREE operations.
* 1.3.6          4Oct94 J. Eisenhamer   Modified Vignetting for both large
*                                       and small apertures.
* 1.3.7          6Oct94 J. Eisenhamer   Removed possibility of extrapolating
*                                       Ripple correction coefficients.
* 1.3.8          8May95 J. Eisenhamer   Bug fix: Properly write out data 
*                                       quality and filled calibration results
*                                       for missing readout data.
*                                       Report non-nominal FINCODEs.
* 1.3.9          9Aug95 J. Eisenhamer   Minor bug in zclwrt.  maxwave is
*                                       incorrectly determined.
* 1.3.9.1       24Jan96 J. Eisenhamer   Minor formatting problems on the 
*                                       thermal/time dependent wave corrections
*                                       No effect on the calibrations
* 1.3.10        Mar96   J. Eisenhamer   No background no longer crashes calhrs.
*                                       Call error on bad calibration.
*                                       Close all reference files.
*                                       Target acquisitions are now properly
*                                       skipped.
* 1.3.11        Oct 96  M. De La Pena   Added background count rate model as
*                                       a user-specified option for post-
*                                       production processing only. Included
*                                       JE change to PHC reference for APER. 
* 1.3.12        May 98  M. De La Pena   Mods to zrccre.f to correct table
*                                       column name.
* 1.3.13        Jun 98  M. De La Pena   Fixed minor syntax errors in FORMAT
*                                       statements in ZCLBLM and ZTEMP.
*                                       Installed zrccre.f fix in STSDAS Oct98.
* 1.3.14        Sep 04  P. Barrett      Fixed occasional problem when reading
*                                       vignetting calibration file.
*------------------------------------------------------------------------------
C
C     Version number
C
      CHARACTER*9 VERSION
      PARAMETER (VERSION = '1.3.14' )
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
      INTEGER RDONLY
      PARAMETER (RDONLY = 1)
C
C                       /HRSMOD/
C Common block containing observing mode parameters
C
C   GRAT - grating mode
C   DET - detector
C   SCLAMP - spectral calibration lamp
C   CARPOS - carrousel position
C   OBSMOD - observation mode (DIR, ACC, TAR)
C   APER - aperture (LSA, SSA, SC1, SC2)
C
        CHARACTER*3 OBSMOD,APER
        CHARACTER*5 GRAT
        INTEGER DET,SCLAMP,CARPOS
        COMMON /HRSMOD/ DET,SCLAMP,CARPOS
        COMMON /HRSMD1/ GRAT,OBSMOD,APER
C
C			/HRSIO/
C Common Block containting input/output parameters
C
C   IDS(20) - input file IDs
C		1 - .shh
C		2 - .ulh
C		3 - .d0h
C		4 - .q0h
C		5 - .x0h
C		6 - .xqh
C		7 - .c0h
C		8 - .c1h
C		9 - .cqh
C		10 - .c2h
C               11 - .c3h
C               12 - .c4h
C               13 - .c5h
C   GCOUNT(20) - group counts for input files
C   MERGE - Number of bins merged in output spectra
C   OBSRPT - observation repeats
C   NGOUT - number of output groups
C   NGSDT - number of output groups for special diode files
C   READNO - readout number
C   NSOUT - number of samples in the output data sets
C
        INTEGER IDS(20),MERGE,OBSRPT,NGOUT,GCOUNT(20),READNO,NSOUT,NGSDT
        COMMON /HRSIO/ IDS,GCOUNT,MERGE,OBSRPT,NGOUT,READNO,NSOUT,NGSDT
C
C ROOTNAME Common block for addition to text messages
C
        CHARACTER*10 ROOTNM
        COMMON /ZMSGCM/ROOTNM
C
C			/ZREFID/
C
C COMMON BLOCK containing id's for reference files which remain
C open throughout the whole calibration.
C
        INTEGER IDREF(7)
        COMMON /ZREFID/ IDREF
C
C LOCAL VARIABLES -------------------------------------------------------------
C
        CHARACTER*80 CONTXT
        CHARACTER*64    ROOT
        CHARACTER*64    ROOTO
C                                    --->rootname of the file
        INTEGER         ISTAT,ISTAT1
C                                    --->error status
C                                    --->text message
        INTEGER I
        CHARACTER*64    FNAME
        CHARACTER*3 QUAL(6)
C                                    --->Qualifiers of the output files
        DATA QUAL/'c0h','c1h','cqh','c2h','c3h','c4h'/
C
C------------------------------------------------------------------------------
C
C CALHRS Version info
C
        CONTXT='*** CALHRS - Version '//VERSION//' ***'
        CALL UMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C Initilize rootnm to blank in case we do not get one and 
C file ids to -1 so we know what is opened
C
        ROOTNM = ' '
C
        DO 3 I=1,7
3            IDREF(I)=-1
        DO 4 I=1,20
4            IDS(I)=-1
C
C Open input data files and get reference file names and processing flags
C
        CALL UUCLGS('rootname',ROOT,ISTAT)
        CALL UUCLGS('output',ROOTO,ISTAT1)
        IF((ISTAT.NE.0).OR.(ISTAT1.NE.0))THEN
                CALL ZMSPUT('Error getting rootname of data files',
     *                          STDOUT+STDERR,0,ISTAT)
                GO TO 999
        ENDIF
        IF(ROOTO.EQ.' ')ROOTO=ROOT
C
C open .shh and get rootname of the observation
C
        CALL ZFNAME(ROOT,'shh',1,0,FNAME)
        CALL UIMOPN(FNAME,RDONLY,IDS(1),ISTAT)
        IF(ISTAT.NE.0)THEN
              CONTXT='ERROR opening .shh file '//FNAME
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              GO TO 999
        ENDIF

        CALL UHDGST(IDS(1),'ROOTNAME',ROOTNM,ISTAT)
        IF(ISTAT.NE.0)THEN
              CONTXT='ERROR getting ROOTNAME from '//FNAME
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              GO TO 999
        ENDIF
C
        CONTXT='BEGIN processing of root file name '//ROOT
        CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
        CONTXT='Output root file name ='//ROOTO
        CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C Open input data files
C
        CALL ZCLOPN(ROOT,ISTAT)
        IF (ISTAT.NE.0) THEN
C
C Check for no science data present
C
            IF (ISTAT.LT.0) ISTAT = 0
	    GO TO 999
	ENDIF
C
C Process the observation if it is not target acquisition
C
        IF(OBSMOD .NE. 'TAR')THEN
C
C get processing flags and reference file names
C
                CALL ZRDFLG(ISTAT)
                IF(ISTAT.NE.0) GO TO 999
C
C get reference relation table names
C
                CALL ZCLREL(IDS(3),ISTAT)
                IF(ISTAT.NE.0) GO TO 999
C
C check that all reference relation tables and reference files exist
C
                CALL ZCLINQ(ISTAT)
                IF(ISTAT.NE.0) GO TO 999
C
C get input keywords
C
                CALL ZGETKW(ISTAT)
                IF(ISTAT.NE.0) GO TO 999
C
C determine the size of the output data set
C
                CALL ZCLOUT(ROOT,ISTAT)
                IF(ISTAT.NE.0) GO TO 999
C
C PROCESS EITHER DDLINK OR ACCUM MODE DATA
C
                CALL ZCLPRC(ROOT,ROOTO,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
        ELSE
              CONTXT='Target acquisition: calibration '//
     $               'processing not performed'
              CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF
        ISTAT = 0
C
C Done
C
C
C Close any input files left open
C
 999   CONTINUE
       DO 1000 I=1,6
          IF(IDS(I).GT.0)CALL UIMCLO(IDS(I),ISTAT1)
 1000  CONTINUE
       DO 1001 I=1,7
          IF(IDREF(I).GT.0)THEN
             IF(I.GT.5)THEN
                CALL UIMCLO(IDREF(I),ISTAT1)
             ELSE
                CALL UTTCLO(IDREF(I),ISTAT1)
             ENDIF
          ENDIF
 1001  CONTINUE
C
C Close any output files left open
C
        DO 1100 I=1,7
              IF(IDS(I+6).GT.0)CALL UIMCLO(IDS(I+6),ISTAT1)
              IF(ISTAT1.NE.0)THEN
                  ISTAT=1
                  WRITE(CONTXT,99)QUAL(I)
99                FORMAT('ERROR closing ',A3,' file')
              ENDIF
1100    CONTINUE
C
C Write final message
C
        IF(ISTAT.NE.0)THEN
           CONTXT='Reduction terminated abnormally for observation '//
     *                  ROOT
           CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CALL UERROR(CONTXT)
         ELSE
           CONTXT='Reduction complete for observation '//ROOT
           CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF

        RETURN
        END
