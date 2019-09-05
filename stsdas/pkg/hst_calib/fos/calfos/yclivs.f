        SUBROUTINE YCLIVS(FRAME,NAME1,NAME2,FILL,EPS,DATA,ERR,
     *                    PEDGR1,PEDGR2,DESCR1,DESCR2,ISTAT)
*
*  Module number:
*
*  Module name: YCLIVS
*
*  Keyphrase:
*  ----------
*       Convert to flux units
*
*  Description:
*  ------------
*       This routine multiplies  OBJECT spectra by the appropriate inverse
*	sensitivity vector.  Points where the inverse sens. is 0.0 are
*	flagged with a data quality value of 200.0
*
*  FORTRAN name: YCLIVS.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       NAME1                   I       first INV. SENS. file (IV1HFILE)
*       NAME2                   I       second one (IV2HFILE)
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput, yrdivs
*
*  History:
*  --------
*  Version      Date        Author          Description
*    1          AUG 89  D. Lindler      Designed and coded
*		Sep 90	S. Hulbert	update BUNITS
*    1.1	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1)
*    2		Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*    2.1        Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* INPUTS:
*       frame - frame number
*       name1, name2 - names of the 2 inv. sens. ref. files
*       fill - bad epsilon limit
*       eps - epsilon array
*
* INPUT/OUTPUT
*       data - data array
*       err - error array
*
* OUTPUT:
*	pedgr1 - ref file 1 PEDIGREE keyword
*	pedgr2 - ref file 2 PEDIGREE keyword
*	descr1 - ref file 1 DESCRIP keyword
*	descr2 - ref file 2 DESCRIP keyword
*       istat - error status
*
        INTEGER FRAME,ISTAT
        CHARACTER*64 NAME1,NAME2
	CHARACTER*68 PEDGR1,PEDGR2,DESCR1,DESCR2
        REAL FILL,EPS(*),DATA(*),ERR(*)
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
C brigtness units for output files
C
        CHARACTER * 20 BUNITS(10)
        COMMON /BUNITS/ BUNITS
C
C Local variables
C
        REAL IVS(5000,2),VAL,YPOS
        LOGICAL FOUND(2)
        CHARACTER*3 DTYPE
        INTEGER IVSNUM,YOFF,SOFF,I,II,IS,IY
        CHARACTER*80 CONTXT
C---------------------------------------------------------------------------
C
C read reference files if first frame (2 files for paired apertures or
C polarimetry mode)
C
        IF(FRAME.EQ.1)THEN
            FOUND(1)=.FALSE.
            FOUND(2)=.FALSE.
            CALL YRDIVS('IV1HFILE ',NAME1,5000,FOUND,IVS,PEDGR1,
     *                   DESCR1,ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
            CONTXT='Conversion to abs. flux units using '//NAME1
            CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
            IF(PAIRED.OR.(POLID.NE.'C'))THEN
                CALL YRDIVS('IV2HFILE ',NAME2,5000,FOUND,IVS,PEDGR2,
     *                       DESCR2,ISTAT)
                IF(ISTAT.NE.0)GO TO 1000
                CONTXT='                                and '//NAME2
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                IF((.NOT.FOUND(1)).OR.(.NOT.FOUND(2)))THEN
                    CONTXT='ERROR: both inv. sens. ref. files are '//
     *                          ' for same aper. or pass dir.'
                    GO TO 999
                ENDIF
             ENDIF
C
C If the reference files contain dummy data, then skip correction
C
             IF(PEDGR1(1:5).EQ.'DUMMY'.OR.PEDGR2(1:5).EQ.'DUMMY')THEN
             IF (PEDGR1(1:5).EQ.'DUMMY') THEN
                 CONTXT='WARNING: PEDIGREE = DUMMY for the IV1HFILE;'
                 CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
             END IF
             IF (PEDGR2(1:5).EQ.'DUMMY') THEN
                 CONTXT='WARNING: PEDIGREE = DUMMY for the IV2HFILE;'
                 CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
             END IF
             CONTXT='         Flux calibration will be skipped'
             CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
             GO TO 1000
	     END IF
        ENDIF
C------------------ end of frame 1 only processing ----------------------------
C
C Loop on slices
C
        DO 500 IS=1,SLICES
            SOFF = NX*YSTEPS*(IS-1)
C                                    --->offset for the slice
C
C loop on ysteps
C
            DO 400 IY=1,YSTEPS
C
C Is it a OBJ
C
                IF(IY.GT.3)THEN
                        DTYPE='OBJ'
                   ELSE
                        DTYPE=YTYPE(IY)
                ENDIF
                IF(DTYPE.EQ.'OBJ')THEN
C
C Determine which of the 2 possible inv. sens. vectors to use
C
                    IVSNUM=1
                    IF(PAIRED)THEN
                        YPOS=YBASE+(YRANGE*32)/YSTEPS*(IY-1)
                        IF(ABS(YPOS-YLOWER).LT.ABS(YPOS-YUPPER))
     *                                                  IVSNUM=2
                    ENDIF
                    IF((POLID.NE.'C').AND.(IY.GT.1))IVSNUM=2
C
C multiply by the inv. sens.
C
                    YOFF = SOFF + (IY-1)*NX
                    DO 100 I=1,NX
                        II = YOFF + I
                        IF(EPS(II).LT.FILL)THEN
                            VAL = IVS(I,IVSNUM)
                            IF(VAL.NE.0.0)THEN 
                                DATA(II) = DATA(II)*VAL
                                ERR(II) = ERR(II)*VAL
                              ELSE
                                DATA(II) = 0.0
                                ERR(II) = 0.0
                                IF(EPS(II).LT.200.0)EPS(II)=200.0
                            ENDIF
                        ENDIF
100                 CONTINUE
                ENDIF
400         CONTINUE
500     CONTINUE
C
C update brightness units for output files
C
        BUNITS(2) = 'ERGS/CM**2/S/A'
        BUNITS(3) = 'ERGS/CM**2/S/A'
        BUNITS(5) = 'ERGS/CM**2/S/A'
C
        ISTAT = 0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT = 1
1000    RETURN
        END
