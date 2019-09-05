C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YMSPUT(CONTXT,DEST,PRIO,ISTAT)
*
*  Module number:
*
*  Module name: YMSPUT
*
*  Keyphrase:
*  ----------
*       output message with rootname
*
*  Description:
*  ------------
*       This routine outputs an message with the rootname concatenated
*       to it.
*
*  FORTRAN name: YMSPUT.for
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       YMSPUT
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jun 89  D. Lindler      Designed and coded
*-------------------------------------------------------------------------------
*
* INPUTS:
*       CONTXT - message to print
*       DEST - destination
*       PRIO - priority
*
* OUTPUTS:
*       ISTAT - error status
*
*--------------------------------------------------------------------------
        CHARACTER*(*) CONTXT
        INTEGER DEST,PRIO,ISTAT
C
C Local variables
C
        INTEGER IDEST
        CHARACTER*80 MESSAG
C---------------------------------------------------------------------------
        MESSAG = CONTXT
C
C delete usrlog from DEST
C
        IDEST = DEST
        IF(IDEST.GE.4)IDEST=IDEST-4
        CALL UMSPUT(MESSAG,IDEST,PRIO,ISTAT)
        RETURN
        END
