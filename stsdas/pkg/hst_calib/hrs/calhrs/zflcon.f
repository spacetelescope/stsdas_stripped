      SUBROUTINE ZFLCON
*
*  Module number:
*
*  Module name: ZFLCON
*
*  Keyphrase:
*  ----------
*       Check calibration flag consistency.
*
*  Description:
*  ------------
*       Check to make sure the calibration flags are self-consistent.
*       Make sure that steps don't occur which depend on steps that
*       don't occur.  
*
*  FORTRAN name: zflcon
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*
*  Subroutines Called:
*  -------------------
*
*  History:
*  --------
*  Version      Date        Author          Description
*     1         Apr94    J. Eisenhamer      Created.
*     1.1       Oct 96   M. De La Pena      Added FBMD,background checks,
*                                           and messages
*
*-------------------------------------------------------------------------------
C
C LOCAL VARIABLES
C
        INTEGER      ISTAT
        CHARACTER*90 CONTXT
C------------------------------------------------------------------------------
C
C ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER   STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER   STDERR
      PARAMETER (STDERR = 2)
C------------------------------------------------------------------------------
C
C                       /HRSFLG/
C Common block containing processing flags
C
C Initial values = 'PERFORM' or 'OMIT'
C final values = 'COMPLETE', 'OMIT', or 'SKIPPED'
C
        CHARACTER*12 FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *                  FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *			FPLY,FGWC,FBMD
        COMMON /HRSFLG/FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *                  FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *			FPLY,FGWC,FBMD
C
        IF(FEXP.NE.'PERFORM')FPPC='OMIT'
        IF(FMAP.NE.'PERFORM')THEN
                FADC='OMIT'
                FVIG='OMIT'
                FPHC='OMIT'
                FMER='OMIT'
        ENDIF
        IF(FMER.NE.'PERFORM')FBCK='OMIT'
        IF((FPHC.NE.'PERFORM').AND.(FVIG.NE.'PERFORM'))FDOP='OMIT'
        IF(FADC.NE.'PERFORM')THEN
                FIAC='OMIT'
                FVAC='OMIT'
                FHEL='OMIT'
                FFLX='OMIT'
                FECH='OMIT'
                FBCK='OMIT'
                FGWC='OMIT'
        ENDIF
        IF(FBCK.NE.'PERFORM')THEN
                FMNF='OMIT'
                FMDF='OMIT'
                FPLY='OMIT'
                FBMD='OMIT'
        ENDIF
        IF(((FMNF.EQ.'PERFORM').OR.(FMDF.EQ.'PERFORM').OR.
     *      (FPLY.EQ.'PERFORM')).AND.(FBMD.EQ.'PERFORM'))THEN
            FBMD  ='SKIPPED'
            ISTAT =1
            CONTXT='Warning: Option BMD_CORR is exclusive ' //
     *             'of other background options;option reset ' //
     *             'to SKIPPED.'
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
C
        RETURN
        END
