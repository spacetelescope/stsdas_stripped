        SUBROUTINE ZMSPUT(CONTXT,DEST,PRIO,ISTAT)
*
*  Module number:
*
*  Module name: ZMSPUT
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
*  FORTRAN name: zmsput.for
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZMSPUT
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
C common block with rootname of the data file
C
        CHARACTER*10 ROOTNM
        COMMON /ZMSGCM/ROOTNM
C
C Local variables
C
        CHARACTER*132 MESSAG
C---------------------------------------------------------------------------
        MESSAG = ROOTNM//' '//CONTXT
        CALL UMSPUT(MESSAG,DEST,PRIO,ISTAT)
        RETURN
        END
