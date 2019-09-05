        SUBROUTINE ZERROR(CONTXT)
*
*  Module number:
*
*  Module name: ZERROR
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
*  FORTRAN name: zerror.for
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZERROR
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Mar96       J. Eisenhamer   Modified from zmsput
*-------------------------------------------------------------------------------
*
* INPUTS:
*       CONTXT - message to print
*
* OUTPUTS:
*      N/A - This routine does not return.
*
*--------------------------------------------------------------------------
        CHARACTER*(*) CONTXT
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
        CALL UERROR(MESSAG)
        RETURN
        END
