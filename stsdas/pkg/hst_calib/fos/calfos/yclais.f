        SUBROUTINE YCLAIS(FRAME,NAME1,FILL,REFAPR,EPS,DATA,ERR,
     *                    PEDGR1,DESCR1,ISTAT)
*
*  Module number:
*
*  Module name: YCLAIS
*
*  Keyphrase:
*  ----------
*       Convert to flux units (new method)
*
*  Description:
*  ------------
*       This routine multiplies  OBJECT spectra by the appropriate inverse
*	sensitivity vector.  Points where the inverse sens. is 0.0 are
*	flagged with a data quality value of 200.0. This routine uses
*	the new method of applying a single average inverse sensitivity
*	curve to data from all apertures and using the APR_CORR routine
*	to correct the data for the relative throughputs of different
*	apertures.
*
*  FORTRAN name: YCLAIS.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       NAME1                   I       average INV. SENS. file (AISHFILE)
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput, yrdais
*
*  History:
*  --------
*  Version      Date        Author          Description
*    1		Oct 94	H. Bushouse	Designed and coded (based on YCLIVS)
*    1.1        Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*    1.2        Sep 98  M. De La Pena   Removed dead statements.
*    1.3        May 01  M. De La Pena   Minor mod to CONTXT stmt for Tru64
*-------------------------------------------------------------------------------
*
* INPUTS:
*       frame - frame number
*       name1 - name of the inv. sens. ref. file
*       fill - bad epsilon limit
*	refapr - reference aperture from CCSB table
*       eps - epsilon array
*
* INPUT/OUTPUT
*       data - data array
*       err - error array
*
* OUTPUT:
*	pedgr1 - ref file PEDIGREE keyword
*	descr1 - ref file DESCRIP keyword
*       istat - error status
*
	IMPLICIT NONE
C
        INTEGER FRAME,ISTAT
	CHARACTER*3  REFAPR
        CHARACTER*64 NAME1
	CHARACTER*68 PEDGR1,DESCR1
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
        REAL IVS(5000)
        CHARACTER*3 DTYPE
        INTEGER YOFF,SOFF,I,II,IS,IY
        CHARACTER*80 CONTXT
C---------------------------------------------------------------------------
C
C read reference file if first frame
C
        IF(FRAME.EQ.1)THEN
            CALL YRDAIS(NAME1,REFAPR,5000,IVS,PEDGR1,DESCR1,ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
            CONTXT='Conversion to abs. flux units using '//NAME1
            CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C If the reference file contains dummy data, then skip correction
C
            IF(PEDGR1(1:5).EQ.'DUMMY')THEN
               CONTXT='WARNING: PEDIGREE = DUMMY for '//NAME1
               CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
               CONTXT='         Flux calibration will be skipped'
               CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
               GO TO 1000
	    END IF
C
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
C multiply by the inv. sens.
C
                    YOFF = SOFF + (IY-1)*NX
                    DO 100 I=1,NX
                        II = YOFF + I
                        IF(EPS(II).LT.FILL)THEN
                           DATA(II) = DATA(II)*IVS(I)
                           ERR(II)  = ERR(II) *IVS(I)
                           IF(IVS(I).EQ.0 .AND. EPS(II).LT.200.0)
     *			      EPS(II)=200.0
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
1000    RETURN
        END
