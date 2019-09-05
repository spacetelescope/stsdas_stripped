        SUBROUTINE ZIMCPY(ROOT,EXT,IGOUT,NGOUT,DATATYPE,NS,
     $			IDSIN,FILETYPE,BUNIT,IDSOUT,ISTAT)
*
*  Module number:
*
*  Module name: ZIMCPY
*
*  Keyphrase:
*  ----------
*       Open calibration output files
*
*  Description:
*  ------------
*       This creates the output data files containing the
*       calibrated data.  Up to 6 files are written. They are
*               <rootname>.c0h  wavelengths
*               <rootname>.cqh  data quality vectors
*               <rootname>.c2h  propagated statistical error
*               <rootname>.c3h  special diodes
*               <rootname>.c4h  special diodes data quality
*               <rootname>.c5h  background
*       On input to this routine the .C1H file is already opened. 
*
*  FORTRAN name: zimcpy.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <root>.c1h              I/O     Output flux file, also used
*                                       as template for other files
*       <root>.c0h              O       wavelength file
*       <root>.cqh              O       data quality file
*       <root>.c2h              O       propagated statistical errors
*       <root>.c3h              O       special diodes 
*       <root>.c4h              O       special diodes data quality
*       <root>.c5h              O       Background
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*  SDAS:
*	uuimcp, uhdpst
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*		Sep 90	S. Hulbert	Split off of zclwrt
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       root - rootname of the observation (ch*64)
*       ext - extension of new copy image
*	igout - current group number
*	ngout - total number of groups
*	datatype - datatype of new copy
*       ns - number of sample positions in each output spectrum
*	idsin - image descriptor of template
*       nspec - number of output spectra
*	filetype - filetype keyword value
*	bunit - bunit keyword value
*
* Output parameter
*
*	idsout - image descriptor of new copy
*       istat - error status (integer)
*
*******************************************************************************
        CHARACTER*64 ROOT
        CHARACTER*30 BUNIT
	CHARACTER*3 EXT, FILETYPE
        INTEGER IGOUT, NGOUT, DATATYPE, NS, IDSIN, IDSOUT, ISTAT
C
C--------------------------------------------------------------------------
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C----------------------------------------------------------------------------
C
C LOCAL VARIABLES
C
        CHARACTER*64 FNAME
        CHARACTER*80 CONTXT
C
C----------------------------------------------------------------------------
C
        CALL ZFNAME(ROOT,EXT,IGOUT,NGOUT,FNAME)
        CALL UUIMCP(FNAME,DATATYPE,1,NS,IDSIN,IDSOUT,ISTAT)
        IF (ISTAT.NE.0)THEN
           CONTXT='ERROR: error opening output file'//
     *            FNAME
           GO TO 999
        ENDIF
C
C update file type
C
        CALL UHDPST(IDSOUT,'FILETYPE',FILETYPE,ISTAT)
        IF (ISTAT.NE.0)THEN
           CONTXT='ERROR Updating FILETYPE in '//EXT//' file'
           GO TO 999
        ENDIF
C
C update bunit
C
        CALL UHDPST(IDSOUT,'BUNIT',BUNIT,ISTAT)
        IF (ISTAT.NE.0)THEN
           CONTXT='ERROR Updating BUNIT in '//EXT//' file'
           GO TO 999
        ENDIf
C
      ISTAT=0
      GO TO 1000
  999 CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT=1
 1000 RETURN
      END
