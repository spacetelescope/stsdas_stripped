        SUBROUTINE ZUDFLG(ISTAT)
*
*  Module number:
*
*  Module name: ZUDFLG
*
*  Keyphrase:
*  ----------
*       Update processing flags in output header
*
*  Description:
*  ------------
*       This routine updates processing flags from PERFORM to
*       COMPLETE in the output header.
*
*  FORTRAN name: zudflg.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.c1h          O       Output flux file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zmsput
*  SDAS:
*       uhdgst
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jun 89  D. Lindler      Designed and coded
*       1.1     Oct 96  M. De La Pena   Added FBMD
*-------------------------------------------------------------------------------
* Output parameter
*
*        ISTAT - error status
*
        INTEGER ISTAT
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
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
C                       /HRSIO/
C Common Block containting input/output parameters
C
C   IDS(20) - input file IDs
C               1 - .shh
C               2 - .ulh
C               3 - .d0h
C               4 - .q0h
C               5 - .x0h
C               6 - .xqh
C               7 - .c0h
C               8 - .c1h
C               9 - .cqh
C               10 - .c2h
C               11 - .c3h
C               12 - .c4h
C               13 - .c5h
C   GCOUNT(20) - group counts for input files
C   MERGE - Number of bins merged in output spectra
C   OBSRPT - observation repeats
C   NGOUT - number of output groups
C   NGSDT - number of output groups for the special diode files
C   READNO - readout number
C   NSOUT - number of sample in output data sets
C
        INTEGER IDS(20),MERGE,OBSRPT,NGOUT,GCOUNT(20),READNO,NSOUT,NGSDT
        COMMON /HRSIO/ IDS,GCOUNT,MERGE,OBSRPT,NGOUT,READNO,NSOUT,NGSDT
C
C LOCAL VARIABLES
C
        CHARACTER*80 CONTXT
        CHARACTER*8 NAME
C-----------------------------------------------------------------------------
C
C READ PROCESSING FLAGS
C
            NAME='PPC_CORR'
            CALL ZXUDFL(NAME,FPPC,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='DIO_CORR'
            CALL ZXUDFL(NAME,FDIO,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='PHC_CORR'
            CALL ZXUDFL(NAME,FPHC,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='VIG_CORR'
            CALL ZXUDFL(NAME,FVIG,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='DOP_CORR'
            CALL ZXUDFL(NAME,FDOP,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='ADC_CORR'
            CALL ZXUDFL(NAME,FADC,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            IF(FADC.EQ.'PERFORM')THEN
               NAME='GWC_CORR'
               CALL ZXUDFL(NAME,FGWC,IDS(8),ISTAT)
               IF(ISTAT.NE.0)GO TO 999
            ENDIF
C
            NAME='IAC_CORR'
            CALL ZXUDFL(NAME,FIAC,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='VAC_CORR'
            CALL ZXUDFL(NAME,FVAC,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='HEL_CORR'
            CALL ZXUDFL(NAME,FHEL,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='BCK_CORR'
            CALL ZXUDFL(NAME,FBCK,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='BMD_CORR'
            CALL ZXUDFL(NAME,FBMD,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='MNF_CORR'
            CALL ZXUDFL(NAME,FMNF,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='MDF_CORR'
            CALL ZXUDFL(NAME,FMDF,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='PLY_CORR'
            CALL ZXUDFL(NAME,FPLY,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='ECH_CORR'
            CALL ZXUDFL(NAME,FECH,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='FLX_CORR'
            CALL ZXUDFL(NAME,FFLX,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='EXP_CORR'
            CALL ZXUDFL(NAME,FEXP,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='MAP_CORR'
            CALL ZXUDFL(NAME,FMAP,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='DQI_CORR'
            CALL ZXUDFL(NAME,FDQI,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
            NAME='MER_CORR'
            CALL ZXUDFL(NAME,FMER,IDS(8),ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
        ISTAT=0
        GO TO 1000
999     WRITE(CONTXT,99)NAME
99      FORMAT('Error: updating processing flag ',A8,
     *                  ' in output .c1h header')
        CALL ZMSPUT(CONTXT,STDOUT+STDOUT,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
