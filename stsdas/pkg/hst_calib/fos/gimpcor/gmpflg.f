        SUBROUTINE GMPFLG(GRNDMD,PFLAGS,ISTAT)
*
*  Module number:
*
*  Module name: GMPFLG
*
*  Keyphrase:
*  ----------
*       Get FOS processing flags
*
*  Description:
*  ------------
*       This routine reads the FOS processing flags for the input .d0h
*       header and places them into a vector as boolean values
*
*  FORTRAN name: gmpflg.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.d0h          I       FOS science data file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*       uhdgst
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jul 89  D. Lindler      Designed and coded
*	1.1	May 90	S. Hulbert	Added GIMP correction 
*	1.2	Aug 90	S. Hulbert	Added scaling of reference background
*       1.3     Mar 93  J. Eisenhamer   Ripped from CALFOS just to do gimp.
*-------------------------------------------------------------------------------
*
* Inputs:
*       grndmd - ground mode
*
* Outputs:
*       pflags - boolean vector of flags
*            position           step
*               1       conversion to count rates
*		2	GIMP correction
*               3       paired pulse
*               4       background subtraction
*               5       flat fielding
*               6       sky subtraction
*               7       wavelengths
*               8       conversion to flux
*               9       error propagation
*               10       mode dependent reductions
*               11       scale reference background
*
*       istat - error status
*
        CHARACTER*18 GRNDMD
        CHARACTER*8 PFLAGS(*)
        INTEGER ISTAT
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
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
C Local variables
C
        INTEGER I
        CHARACTER*7 VAL
        CHARACTER*80 CONTXT
        CHARACTER*8 NAMES(11)
        DATA NAMES/'CNT_CORR','OFF_CORR', 'PPC_CORR','BAC_CORR',
     *		   'FLT_CORR',
     *             'SKY_CORR','WAV_CORR','FLX_CORR','ERR_CORR',
     *             'MOD_CORR', 'GMF_CORR'/
C
C--------------------------------------------------------------------------
        DO 100 I=1,11
                CALL UHDGST(IDS(1),NAMES(I),VAL,ISTAT)
                IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR: getting d0h processing flag '//
     *                          NAMES(I)
                    GO TO 999
                ENDIF
                PFLAGS(I)='OMIT'
C
C     do not perform any steps, regardless of what the header says.
C
c                IF(VAL.EQ.'PERFORM')PFLAGS(I)=.TRUE.
100     CONTINUE
C
C     For This version, we just want to do GIMP correction.
C
        PFLAGS(2)='PERFORM'
C
C	check that the two background-related switches are consistent
C
C	IF (.NOT.PFLAGS(4)) PFLAGS(11)=.FALSE.
C
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
