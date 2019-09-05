      SUBROUTINE ZNOFLG(ID,ISTAT)
*
*  Module number:
*
*  Module name: ZNOFLG
*
*  Keyphrase:
*  ----------
*       Undo calibration flags for steps that occur after 
*       background subtraction
*
*  Description:
*  ------------
*       This routine "undoes" the calibration flags for those steps
*       that occur after the background subtraction.  This is done for the
*       output product:
*               <rootname>.c5h  background
*
*       This is to represent that fact that the background was calculated
*       before any of these steps were taken.
*
*       The flags affected are:
*       IAC_CORR
*       ECH_CORR
*       FLX_CORR
*       HEL_CORR
*       VAC_CORR
*
*  FORTRAN name: znoflg.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <root>.c5h              O       background
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*     zmsput
*  SDAS:
*     ugdgst
*
*  History:
*  --------
*  Version      Date        Author          Description
*     1         Mar94    J.Eisenhamer       Created
*     1.1       Oct 96   M. De La Pena      Added FBMD
*-------------------------------------------------------------------------------
*
* Input parameters
*
*     id - Id of the .c5h file.
*
* Output parameter
*
*       istat - error status (integer)
*
*******************************************************************************
      INTEGER ID, ISTAT,STDOUT,STDERR
      PARAMETER (STDOUT = 1)
      PARAMETER (STDERR = 2)
C
C                       /HRSFLG/
C Common block containing processing flags
C
C Initial values = 'PERFORM' or 'OMIT'
C final values = 'PERFORMED' or 'OMITTED'
C
        CHARACTER*12 FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *                  FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *                  FPLY,FGWC,FBMD
        COMMON /HRSFLG/FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *                  FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *                  FPLY,FGWC,FBMD
C
C LOCAL VARIABLES
C
        CHARACTER*80 CONTXT
        CHARACTER*8 NAME
C-----------------------------------------------------------------------------
C
C Set the flags back to PERFORM if they were supposed to be done.
C     
        NAME='IAC_CORR'
        IF(FIAC.NE.'OMIT')THEN
           CALL UHDAST(ID,NAME,'PERFORM',' ',0,ISTAT)
           IF(ISTAT.NE.0)GO TO 999
        ENDIF
C     
        NAME='VAC_CORR'
        IF(FVAC.NE.'OMIT')THEN
           CALL UHDAST(ID,NAME,'PERFORM',' ',0,ISTAT)
           IF(ISTAT.NE.0)GO TO 999
        ENDIF
C     
        NAME='HEL_CORR'
        IF(FHEL.NE.'OMIT')THEN
           CALL UHDAST(ID,NAME,'PERFORM',' ',0,ISTAT)
           IF(ISTAT.NE.0)GO TO 999
        ENDIF
C     
        NAME='ECH_CORR'
        IF(FECH.NE.'OMIT')THEN
           CALL UHDAST(ID,NAME,'PERFORM',' ',0,ISTAT)
           IF(ISTAT.NE.0)GO TO 999
        ENDIF
C     
        NAME='FLX_CORR'
        IF(FFLX.NE.'OMIT')THEN
           CALL UHDAST(ID,NAME,'PERFORM',' ',0,ISTAT)
           IF(ISTAT.NE.0)GO TO 999
        ENDIF
C     
C     That's all folks.
C     
        ISTAT=0
        GO TO 1000
 999    CONTINUE
        WRITE(CONTXT,99)NAME
 99     FORMAT('Warning: could not reset flag ',A8,
     *                  ' in output .c5h header')
        CALL ZMSPUT(CONTXT,STDERR+STDOUT,0,I)
 1000   CONTINUE
        RETURN
        END
