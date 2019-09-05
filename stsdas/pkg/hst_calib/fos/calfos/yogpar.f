        SUBROUTINE YOGPAR(ID,YSTEP,YTYPE1,IGIMP,ISTAT)
*
*  Module number: 
*
*  Module name: yogpar
*
*  Keyphrase:
*  ----------
*	Write group parameters
*  Description:
*  ------------
*	This routine updates the group parameters in an output data set
*
*  FORTRAN name: yogpar.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*	ID			O	output calfos data file
*  Subroutines Called:
*  -------------------
*  CDBS:
*	
*  SDAS:
*	uhdps*, ymsput
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*	1	Jul 89	D. Lindler	Designed and coded
*		May 90	S. Hulbert	YPOS, EXPTIME (old/new data format)
*	1.2	May 91	S. Hulbert	changed EXPTIME to EXPOSURE, added
*					X_OFFSET; changed calling sequence
*	1.3	Jun 91	S. Hulbert	Added y-offset
*	2	Mar 94	H. Bushouse	Added SCT_VAL and SCT_ERR group pars
*       3       Feb 96  J. Eisenhamer   Clarified error messages.
*       3.1     Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* inputs:
*       ID - FILE id
*       YSTEP - YSTEP number
*       YTYPE1 - type 'OBJ', 'SKY', 'BCK'
*
* outputs:
*       istat - error status
*---------------------------------------------------------------------------
        INTEGER ID,ISTAT,YSTEP,IGIMP
        CHARACTER*3 YTYPE1
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
        INTEGER TYREAL
        PARAMETER (TYREAL = 6)
C------------------------------------------------------------------------------
C Get IRAF MEM common into main program.
C
      LOGICAL          MEMB(1)
      INTEGER*2        MEMS(1)
      INTEGER*4        MEMI(1)
      INTEGER*4        MEML(1)
      REAL             MEMR(1)
      DOUBLE PRECISION MEMD(1)
      COMPLEX          MEMX(1)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
C------------------------------------------------------------------------------
C
C Header I/O parameters
C
        INTEGER USHPNF
        PARAMETER (USHPNF = 40)
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
        REAL EXPO
        COMMON /CONFG4/EXPO
        LOGICAL PAIRED
        REAL YUPPER,YLOWER
        COMMON /CCS1CM/ PAIRED,YUPPER,YLOWER
        INTEGER PKTFMT
        COMMON /CONFG6/ PKTFMT
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
        INTEGER XOFFST, YOFFST, NSPEC
        COMMON /GMPOFF/ XOFFST, YOFFST, NSPEC
C
	INTEGER SCTVAL, SCTERR
	COMMON /SCTCOR/ SCTVAL, SCTERR
C
C local variables
C
        CHARACTER*80 CONTXT
        CHARACTER*8 KEYWRD
        CHARACTER*6 APERPS
        REAL YPOS
        INTEGER PASSDR
C-----------------------------------------------------------------------------
C
C PASS_DIR
C
        IF(POLID.EQ.'C')THEN
                PASSDR=0
           ELSE
                IF(YSTEP.EQ.1)THEN
                        PASSDR=1
                   ELSE
                        PASSDR=2
                ENDIF
        ENDIF
        KEYWRD='PASS_DIR'
        CALL UHDPSI(ID,KEYWRD,PASSDR,ISTAT)
        IF(ISTAT.NE.0)GO TO 999
C
C YPOS
C Don't update ypos for mode II binary search observations
C packet format code 128(80hex)
C
        IF (PKTFMT .NE. 128) THEN 
                YPOS = YBASE + ((32.0 * YRANGE) / YSTEPS) * (YSTEP-1)
                KEYWRD='YPOS'
                CALL UHDPSR(ID,KEYWRD,YPOS,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
        ENDIF
C
C YTYPE
C
        KEYWRD='YTYPE'
        CALL UHDPST(ID,KEYWRD,YTYPE1,ISTAT)
        IF(ISTAT.NE.0)GO TO 999
C
C APER_POS
C
        IF(PAIRED)THEN
            IF(ABS(YPOS-YUPPER).LT.ABS(YPOS-YLOWER))THEN
                 APERPS='UPPER'
              ELSE
                 APERPS='LOWER'
            ENDIF
          ELSE
            APERPS='SINGLE'
        ENDIF
        KEYWRD='APER_POS'
        CALL UHDPST(ID,KEYWRD,APERPS,ISTAT)
        IF(ISTAT.NE.0)GO TO 999
C
C EXPOSURE TIME
C
        KEYWRD='EXPOSURE'
        CALL UHDPSR(ID,KEYWRD,EXPO,ISTAT)
        IF(ISTAT.NE.0)GO TO 999
C
C GIMP OFFSET
C Don't update X and Y_OFFSET for c4h file because correction is
C not applied yet.
C
        IF (IGIMP .GT. 0) THEN
            IF (XOFFST .GT. 0) THEN
               KEYWRD='X_OFFSET'
               CALL UHDPSR(ID,KEYWRD,MEMR(XOFFST+IGIMP-1),ISTAT)
               IF (ISTAT .NE. 0) GO TO 999
	    ENDIF
            IF (YOFFST .GT. 0) THEN
               KEYWRD='Y_OFFSET'
               CALL UHDPSR(ID,KEYWRD,MEMR(YOFFST+IGIMP-1),ISTAT)
               IF (ISTAT .NE. 0) GO TO 999
	    ENDIF
        ENDIF
C
C SCATTERED LIGHT
C Don't update SCT_VAL and SCT_ERR for c4h file because correction
C is not applied yet.
C
	IF (IGIMP .GT. 0) THEN
	    IF (SCTVAL .GT. 0) THEN
               KEYWRD='SCT_VAL'
               CALL UHDPSR(ID,KEYWRD,MEMR(SCTVAL+IGIMP-1),ISTAT)
               IF (ISTAT .NE. 0) GO TO 999
	    ENDIF
	    IF (SCTERR .GT. 0) THEN
               KEYWRD='SCT_ERR'
               CALL UHDPSR(ID,KEYWRD,MEMR(SCTERR+IGIMP-1),ISTAT)
               IF (ISTAT .NE. 0) GO TO 999
	    ENDIF
	ENDIF
C
        ISTAT=0
        GO TO 1000
C
999     CONTINUE
        WRITE(CONTXT,998)KEYWRD
 998    FORMAT('ERROR: cannot access group parameter ',a8)
        CALL YMSPUT(CONTXT,STDOUT+STDERR,0,STAT)
 1000   CONTINUE
        END
