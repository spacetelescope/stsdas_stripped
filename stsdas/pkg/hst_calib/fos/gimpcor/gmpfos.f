        SUBROUTINE GMPFOS
*
*  Module number:
*
*  Module name: gmpfos
*
*  Keyphrase:
*  ----------
*       Calibrate FOS data
*
*  Description:
*  ------------
*       This routine determines the amount of gimp in FOS data.  It has
*       one input cl parameters:
*               input - input rootname
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
*               <input>.shh .shd - SHP file.
*
* The following reference files are used.  Files names are taken from the
* input .d0h file header.
*
*       CCS7 - GIMP correction scale factors
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
* 1.2.3.1.9 Feb 93      J. Eisenhamer   Bug FIx--In spectropolarimetry,
*                                       when combining the two passes, the
*                                       Stoke's V parameter was wrongly
*                                       forced positive due to a missing 'abs()'
*                                       call.  Modified ypol2.f.
* GIMP      Mar 93      J. Eisenhamer   Pulled out of calfos the gimp
*                                       determination to print out.
* GIMP 1.2  Aug 93	H. Bushouse	Changed task name to GIMPCOR.
* GIMP 1.3  Dec 2000    M. De La Pena   Added KYDPLY to CONFG1 common.
*-------------------------------------------------------------------------------
C
C     Version number
C
      CHARACTER * 9 VERSN
      PARAMETER (VERSN = 'GIMP VER')
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
        LOGICAL HEADER,TRAILR,DEFDDT
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
        CHARACTER*64 ROOT,ROOTO
C                                    --->Input and output root names
        INTEGER ISTAT,ISTAT1
C                                    --->error status
        CHARACTER*80 CONTXT
C                                    --->text message
        INTEGER I
C
C -----------------------------------------------------------------------
C
C CALFOS Version info
C
        CONTXT='*** CALFOS - Version '//VERSN//' ***'
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
C
C for GIMP version, no output is necessary
C
c        CALL UUCLGS('output',ROOTO,ISTAT2)
        IF((ISTAT1.NE.0))THEN
                CONTXT='ERROR getting value of CL parameter'
                GO TO 999
        ENDIF
        IF(ROOTO.EQ.' ')ROOTO=ROOT
C
C open input .D0h file
C
        CALL YOPD0H(ROOT,GRNDMD,ISTAT)
        IF(ISTAT.NE.0) GO TO 999
C
        CONTXT='Begin CALFOS for input file rootname: '//ROOT
        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
c        CONTXT='                output file rootname: '//ROOTO
c        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C process data that is not time-tagged mode
C
        IF(GRNDMD.NE.'TIME-TAGGED')THEN
                CALL GMPPRC(ROOT,ROOTO,GRNDMD,ISTAT)
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
                CALL UERROR(CONTXT)
        ENDIF
        RETURN
        END
