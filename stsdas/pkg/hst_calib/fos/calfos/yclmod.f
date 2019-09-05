        SUBROUTINE YCLMOD(ROOT,PFLAGS,FRAME,NFRAME,GRNDMD,RETFIL,CCS4,
     *                  PCPFIL,FILL,WAVE,DATA,ERR,EPS,PEDGR1,PEDGR2,
     *                  PEDGR3,DESCR1,DESCR2,DESCR3,ISTAT)
*
*  Module number:
*
*  Module name: YCLMOD
*
*  Keyphrase:
*  ----------
*       Mode dependent calibrations
*
*  Description:
*  ------------
*       This routine performs the special calibrations for time-resolved,
*       polarimetry, and rapid-readout modes.
*
*  FORTRAN name: yclmod.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       yclpol, yclrr, ycltr, ymsput
*  SDAS:
*
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Aug 89  D. Lindler      Designed and coded
*       2       Mar 92  S. Hulbert      Pass EPS array and FILL value to yclpol
*	2.1	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1)
*	3	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*       3.1     Mar 97  M. De La Pena   Added KYDPLY, PCPFIL, PEDGR3, & DESCR3
*-------------------------------------------------------------------------------
*
* INPUTS:
*       root   - output root name
*       pflags - processing flags
*       frame  - frame number
*       nframe - number of frames
*       grndmd - ground mode
*       retfil - retard file name
*       ccs4   - Wollaston/waveplate parameter table name
*       pcpfil - post-COSTAR polarimetry calibration table name
*       fill   - bad data epsilon threshold
*       wave   - wavelength array
*       data   - data array
*       err    - error array
*       eps    - data quality array
*
* Output parameter
*	pedgr1 - retard PEDIGREE keyword
*	pedgr2 - ccs4 PEDIGREE keyword
*       pedgr3 - post-Costar polarimetry corrections PEDIGREE keyword
*	descr1 - retard DESCRIP keyword
*	descr2 - ccs4 DESCRIP keyword
*       descr3 - post-Costar polarimetry corrections DESCRIP keyword
*       istat  - error status
*
*-----------------------------------------------------------------------
        INTEGER FRAME,NFRAME,ISTAT
        CHARACTER*64 RETFIL,CCS4,PCPFIL,ROOT
	CHARACTER*68 PEDGR1,PEDGR2,PEDGR3,DESCR1,DESCR2,DESCR3
        CHARACTER*8 PFLAGS(*)
        CHARACTER*18 GRNDMD
        REAL FILL,DATA(*),ERR(*),EPS(*),WAVE(*)
C
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
        INTEGER NX,NOBJ,NSKY,NBCK
        COMMON /CONFG3/NX,NOBJ,NSKY,NBCK
        LOGICAL PAIRED
        REAL YUPPER,YLOWER
        COMMON /CCS1CM/PAIRED,YUPPER,YLOWER
C
C Local variables
C
        CHARACTER*80 CONTXT
C----------------------------------------------------------------------------
C
        IF(NOBJ.EQ.0)THEN
            CONTXT='ERROR: MOD_CORR set to perform and'//
     *                          ' no object spectra are present'
            GO TO 999
        ENDIF
C
C determine type of special processing
C
C  rapid-readout
C
        IF(GRNDMD.EQ.'RAPID-READOUT')THEN
                CALL YCLRR(FRAME,ROOT,NFRAME,PFLAGS,NX,FILL,
     *                          DATA,ERR,EPS,ISTAT)
                IF(ISTAT.NE.0)GO TO 1000
        ENDIF
C
C time-resolved
C
        IF ((FRAME.EQ.NFRAME).AND.(GRNDMD.EQ.'TIME-RESOLVED'))THEN
                CALL YCLTR(ROOT,PFLAGS,FILL,DATA,ERR,EPS,ISTAT)
                IF(ISTAT.NE.0)GO TO 1000
        ENDIF
C
C spectropolarimetry
C
        IF (GRNDMD.EQ.'SPECTROPOLARIMETRY')THEN
                CALL YCLPOL(FRAME,ROOT,PFLAGS,NFRAME,RETFIL,CCS4,
     *                      PCPFIL,WAVE,DATA,ERR,EPS,FILL,PEDGR1,PEDGR2,
     *                      PEDGR3,DESCR1,DESCR2,DESCR3,ISTAT)
                IF(ISTAT.NE.0)GO TO 1000
        ENDIF
C
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT = 1
1000    RETURN
        END
