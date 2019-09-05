C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLFOS
*
*  Module number:
*
*  Module name: yclfos
*
*  Keyphrase:
*  ----------
*       Calculate wavelengths from FOS pixel data
*
*  Description:
*  ------------
*       This routine reads the FOS data parameters/inputs.  It has
*       six input cl parameters:
*               input - input rootname
*               output - output wavelength rootname
*               pixtab - input pixel table rootname
*               sub_pix - switch to apply the sub-pixel correction
*               poafile - input '.poa' file rootname
*               specnum - spec number of the sub-pixel value
*
*
*  FORTRAN name:
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       The following raw data files are accessed:
*               <input>.d0h .d0d - raw data file
*               <input>.q0h .q0d - data quality for raw data
*               <input>.d1h .d1d - trailer file (reject array)
*               <input>.q1h .q1d - data quality for trailer file
*               <input>.ulh .uld - unique data log file.
*
*       The following output files are produced:
*               <output>.c0h .c0d - wavelengths
*               <output>.c1h .c1d - calibrated flux
*               <output>.c2h .c2d - propagated errors
*               <output>.cqh .cqd - output data quality
*               <output>.c3h .c2d - special statistics file
*               <output>.c4h .c4d - count rate file
*               <output>.c5h .c5d - flat fielded object spectra
*               <output>.c6h .c6d - flat fielded sky spectra
*               <output>.c7h .c7d - background spectra
*               <output>.c8h .c8d - flat fielded object minus smoothed sky
*
* The following reference files are used.  Files names are taken from the
* input .d0h file header.
*
*       BACHFILE - default background
*       FL1HFILE, FL2HFILE - flat fields
*       IV1HFILE, IV2HFILE - invserse sensitivity files. (old method)
*	AISHFILE - average inverse sensitivity file (new method)
*       RETHFILE - retardation file
*       DDTHFILE - disabled diode file
*       DQ1HFILE, DQ2HFILE - data quality initialization file
*       PCPHFILE - post-Costar Polarimetry calibration corrections file
*       CCG2- paired pulse coefficient table
*       CCS0 - aperture size table
*       CCS1 - upper/lower aperture position table
*       CCS2 - sky emission line position table
*       CCS3 - sky/background filter width table
*       CCS4 - Wollaston/Waveplate parameter table
*       CCS5 - sky shift table
*       CCS6 - wavelength coefficient table
*       CCS7 - GIMP correction scale factors
*       CCS8 - predicted background count rates
*	CCS9 - scattered light parameters
*	CCSA - OTA focus history
*	CCSB - aperture throughput coefficients
*	CCSC - aperture throughput vs focus
*	CCSD - time changes in sensitivity
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       yclprc, ymsput, yopd0h
*  SDAS:
*       uuclgs, uerror
*  Others:
*
*
*  History:
*  --------
* Version   Date        Author          Description
*       1   Jul 89      D. Lindler      Designed and coded
*     1.1   May 90      S. Hulbert      new DQI, changes to updating header
*                                       due to changing data format        
*     1.2   Oct 90                      add POLANG and BUNIT
* 1.1.2.1   Mar 91      S. Hulbert      If UDL missing, reset DEFDDT to false
*                                       and continue processing. Start new
*                                       version numbering.
*   1.1.3   May 91      S. Hulbert      Add GIMP correction
*                                       Change to use reprocessing headers
* 1.1.3.1   Aug 91      S. Hulbert      Add scaling of reference background
*	    				based on geomagnetic position
* 1.2.3.1   Nov 91      S. Hulbert      Bug fix to checking for missing UDL
*					Use new STSDAS version # 1.2
* 1.2.3.1.1 Jan 92	S. Hulbert	Bug fix in yosize for setting c5h
*					number of groups when only performing
*					first few calibrations steps,
*					modify message output in yclbck,
*					fix bug in yrccs8--bad counter,
*					change divide-by-zero checking in
*					ysclbk.
* 1.2.3.1.2 Jan 92	S. Hulbert	Bug Fix--don't update gimp group
*					pararmeters in c4h file (yclwrt).
*					Scale reference background with
*					mean predicted rate in the case
*					of ACCUM observations (yclbck,ysclbk).
* 1.2.3.1.3 Mar 92	S. Hulbert	Bug Fix--Assign zero statistical error
*					to a zero count rate. Modified yclerr,
*					yclmod, yclpol, yclprc. Modified ypolar
*					weighting scheme.
* 1.2.3.1.4 Jun 92      D. Bazell       Bug Fix--Check YSTEP and SLICE before
*                                       deallocating memory. Modified ysclbk
* 1.2.3.1.5 Jul 92      D. Bazell       Bug Fix--Deallocate some pointers.
*                                       Modified ymagfd and ygimp.
* 1.2.3.1.6 Sep 92      D. Bazell       Bug Fix--Fixed array indexing for
*                                       determining scaling in Linear
*                                       Polarization calc. Modified ypol2.
* 1.2.3.1.7 Nov 92      D. Bazell       Bug Fix--Change output keyword from
*                                       'COMPLETED' to 'COMPLETE' when a cal
*                                       step has been completed.  Modified
*                                       yuhead.f
* 1.2.3.1.8 Nov 92      D. Bazell       Bug Fix--Set the error for 0 counts
*                                       to 1 if processing spectropolarimetry
*                                       data, otherwise leave it set to 0.
*                                       Modified yclerr.f
* 1.2.3.1.9 Feb 93      E. Eisenhamer   Bug FIx--In spectropolarimetry,
*                                       when combining the two passes, the
*                                       Stoke's V parameter was wrongly
*                                       forced positive due to a missing 'abs()'
*                                       call.  Modified ypol2.f.
* 1.2.3.2   Apr 93	H. Bushouse	SDAS v1.2.3: Mods to most routines for
*					compatibility with RISC Fortran;
*					declare passed arrays with (*), not (1)
* 1.2.3.3   Apr 93	H. Bushouse	Bug Fix--In GIMP correction, when
*					unraveling separate ACCUM readouts,
*					correct for roundoff errors by setting
*					counts to zero if unraveled counts are
*					less than 0.5/(readout exposure time).
*					Modified ycloff.f.
* 1.2.3.4   May 93	H. Bushouse	Issue warning if performing GIMP corr
*					when onboard corr was already applied.
*					Modified ycloff.f.
* 1.3.0     Aug 93	H. Bushouse	Added ISTAT to argument lists of
*					HSTPOS.f and BFIELD.f.  STSDAS V1.3.
*	    Oct 93	H. Bushouse	Fixed memory allocation/deallocation
*					bugs in YCLBCK and YSCLBK.
* 1.3.1     Mar 94	H. Bushouse	Fixed output array initialization in
*					polarimetry routine YPOL2.
*           Mar 94	H. Bushouse	Added scattered light subtraction
*					routines YCLSCT and YRCCS9.
*           Mar 94	H. Bushouse	Fixed bug in YCLPOL for NREAD > 1.
* 1.3.2     Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords.
*					Fixed bug in YSCLBK for polarimetry
*					data with NREAD > 1.
* 1.3.2.1   May 94	H. Bushouse	Changed error to warning in YCLPOL
*					for < 4 waveplate positions.
* 1.3.2.2   Jun 94	H. Bushouse	Retain negative polarimetry I values.
*					Modified ypolar.f, ypol2.f, ypol3.f.
* 1.3.2.3   Sep 94	H. Bushouse	Initialize XSC in ypolar, ypol2, ypol3.
* 2.0       Oct 94	H. Bushouse	Added aperture, focus, time and new
*					flux calibration routines (YCLAPR,
*					YCLFCS, YCLTIM, YCLAIS).
* 2.1       Apr 95      J. Eisenhamer   Removed post-COSTAR check on new
*                                       flux calibration.  Fixed pedigree
*                                       handling of ais_corr.
* 2.2       May 95      J. Eisenhamer   Bug fix in bfield--Zero index.
*                                       Check new flux against polarimetry.
* 2.3       Sep 95      J. Eisenhamer   Modified so errors in the AIS_,
*                                       APR_, or TIM_CORR, simply skip
*                                       the calibration step.
* 2.3.1     Dec 95      J. Eisenhamer   If WAV_CORR is not done AIS stuff won't
*                                       occur.
* 2.3.2     Dec 95      J. Eisenhamer   Added median filtering to scattered
*                                       light correction.
* 2.3.3     Dec 95      J. Eisenhamer   Fixed min/max wave algorithm.
* 2.3.4     Jan 96      J. Eisenhamer   Put in message when no sky to subtract.
* 2.3.5     Feb 96      J. Eisenhamer   Clarified group parameter update errors.
* 2.4       Feb 96      J. Eisenhamer   Modified to return status to command.
* 2.4.1     Mar 96      J. Eisenhamer   Bad error return in yrdais.
*                                       Bad error message in yclwrt.
*                                       Change in STSDAS library for exit return
* 3.0       Mar 97      M. De La Pena   Mods for post-COSTAR polarimetry calib
* 3.1       Jun 98      M. De La Pena   Removed unecessary check on KYDPLY in
*                                       YCONFG.F.
* 3.2       Apr 99      M. De La Pena   Check for post-COSTAR POLSCAN=4 data.
*                                       Modified YCLPOL.F.  Removed STSDAS
*                                       version number from this file.
*     /////////////////////////////////////////////////////////////////////
*            Start of Post Operational Archive versions (poa_calfos)
*     /////////////////////////////////////////////////////////////////////
*     1.0       Jul 00  A. Alexov       Replaces calfos with poa_calfos version
*     1.1       May 01  A. Alexov       V1.1 of poa_calfos pipeline
*     1.2       Oct 01  A. Alexov       Added the poafile, sub_pix, specnum 
*                                       input
*     1.2.1     Oct 01  A. Alexov       Version number changed to 1.2.1
*-------------------------------------------------------------------------------
C
C     Version number
C
      CHARACTER*14 VERSN
      PARAMETER (VERSN = '1.2.1-Nov-2001')
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Common block containing confiquration parameters
C
        CHARACTER*5 DET
        CHARACTER*3 FGWAID,APERID,YTYPE(3)
        CHARACTER*1 POLID
        INTEGER FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME
        LOGICAL HEADER,TRAILR,DEFDDT,KYDPLY
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
        COMMON /CONFG2/FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME,HEADER,TRAILR,
     *          DEFDDT
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
C Common block containing rootname for YMSPUT.FOR
C
        CHARACTER*10 ROOTNM
        COMMON /YMSGCM/ROOTNM
C
C Common block containing ground mode
C
        CHARACTER * 18 GRNDMD
        COMMON /GMODE/ GRNDMD
C
C Local variables
C
        CHARACTER*64 ROOT,ROOTO,PIXTAB,POAFILE
C                                    --->Input and output root names
        INTEGER SPECNUM
C                                    --->Input spectral number
        LOGICAL SUB_PIX
C                                    --->Input - do the sub pix correction?
        INTEGER ISTAT,ISTAT1,ISTAT2,ISTAT3,ISTAT4,ISTAT5,ISTAT6
C                                    --->error status
        CHARACTER*80 CONTXT
C                                    --->text message
        INTEGER I
C
C -----------------------------------------------------------------------
C
C FOS_PIX2WAV Version info
C
        CONTXT='*** FOS_PIX2WAV - Version '//VERSN//' ***'
        CALL UMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C initialization
C
        ROOTNM=' '
        DO 10 I=1,30
                IDS(I)=-1
C                                    --->flag as not open
10      CONTINUE
C
C get rootnames of the input/output files
C
        CALL UUCLGS('input',ROOT,ISTAT1)
        CALL UUCLGS('pixtab',PIXTAB,ISTAT2)
        CALL UUCLGS('wavtab',ROOTO,ISTAT3)
        CALL UUCLGS('poafile',POAFILE,ISTAT5)
        CALL UCLGSB('sub_pix',SUB_PIX,ISTAT4)
        CALL UCLGSI('specnum',SPECNUM,ISTAT6)

        IF((ISTAT1.NE.0).OR.(ISTAT2.NE.0).OR.(ISTAT3.NE.0).OR.
     *     (ISTAT4.NE.0).OR.(ISTAT5.NE.0).OR.(ISTAT6.NE.0))THEN
                CONTXT='ERROR getting value of CL parameter'
                CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                ISTAT=1
                GO TO 999
        ENDIF
        IF(ROOTO.EQ.' ')THEN
                CONTXT='ERROR output wave table name not specified'
                GO TO 999
        ENDIF
C
C open input .D0h file
C
        CALL YOPD0H(ROOT,GRNDMD,ISTAT)
        IF(ISTAT.NE.0) GO TO 999
C
        CONTXT='Begin FOS_PIX2WAV for input file rootname: '//ROOT
        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
        CONTXT='                input pix table file: '//PIXTAB
        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
        CONTXT='                input poa file file: '//POAFILE
        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C inform on data type 
C
c        CONTXT='Data is of type ',GRNDMD
c        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C process data that is not time-tagged mode
C
        IF(GRNDMD.NE.'TIME-TAGGED')THEN
                CALL YCLPRC(ROOT,ROOTO,PIXTAB,SUB_PIX,POAFILE,SPECNUM,
     *               GRNDMD,ISTAT)
          ELSE
                CONTXT='TIME TAGGED DATA - no processing done'
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF
C
C Close any files remaining open
C
999     DO 100 I=1,10
                IF(IDS(I).GT.0)CALL UIMCLO(IDS(I),ISTAT1)
100     CONTINUE
        DO 200 I=11,30
            IF(IDS(I).GT.0)THEN
                CALL UIMCLO(IDS(I),ISTAT1)
                IF(ISTAT1.NE.0)THEN
                        CONTXT='ERROR closing an output file(s)'
                        ISTAT=1
                ENDIF
            ENDIF
200     CONTINUE
C
C print completion message
C
        IF(ISTAT.EQ.0)THEN
                CONTXT='Reduction completed for input file '//ROOT
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
           ELSE
                CONTXT='Reduction NOT completed for input file '//
     *                           ROOT
                CALL YERROR(ISTAT,CONTXT)
        ENDIF
        RETURN
        END
