        SUBROUTINE ZRDFLG(ISTAT)
*
*  Module number:
*
*  Module name: ZRDFLG
*
*  Keyphrase:
*  ----------
*       Read processing flags and reference file names
*  Description:
*  ------------
*       This routine reads the reference file names and the processing
*       flags from the header of the .D0H file and places the processing
*       flags in common block HRSPFG and the reference file names in
*       common block HRSREF.
*
*       If an error occurs reading any processing flag its value will be
*       set to PERFORM.  If an error occurs reading a reference file name
*       the name is set to blank.
*
*  FORTRAN name: ZRDFLG.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*   <rootname>.d0h              Input   Header of the input data file
*                                       previously opened to unit IDS(3)
*                                       stored in common block HRSIO.
*  Subroutines Called:
*  -------------------
*  CDBS:
*       none
*  SDAS:
*       uhdgst, ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jan 89  D. Lindler      Designed and coded
*      1.1      Jun 91  S. Hulbert      Changed grating names
*      1.2      Aug 91  S. Hulbert      Turn off background if not applying
*					dispersion correction
*      1.3      Feb94   J. Eisenhamer   Added GWC_CORR.
*      1.4      Oct 96  M. De La Pena   Added FBMD,CCRE
*      1.5      Nov 96  M. De La Pena   Added SAAFIL
*-------------------------------------------------------------------------------
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
     *			FPLY,FGWC,FBMD
        COMMON /HRSFLG/FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *                  FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *			FPLY,FGWC,FBMD
C
C                       HRSREF
C  Common block containing reference file names and table relation
C  names
C
        CHARACTER*64 CCR1,CCR2,CCR3,CCR4,CCR5,CCR6,CCR7,CCR8,CCR9,
     *                  CCRA,CCRB,CCG2,DIOFIL,PHCFIL,VIGFIL,ABSFIL,
     *                  NETFIL,DQIFIL,CCRC,CCRD,CCRE,SAAFIL
        COMMON /HRSREF/ CCR1,CCR2,CCR3,CCR4,CCR5,CCR6,CCR7,CCR8,CCR9,
     *                  CCRA,CCRB,CCG2,DIOFIL,PHCFIL,VIGFIL,ABSFIL,
     *                  NETFIL,DQIFIL,CCRC,CCRD,CCRE,SAAFIL
C
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
C                       /HRSMOD/
C Common block containing observing mode parameters
C
C   GRAT - grating mode
C   DET - detector
C   SCLAMP - spectral calibration lamp
C   CARPOS - carrousel position
C   OBSMOD - observation mode (DIR, ACC, TAR)
C   APER - aperture (LSA, SSA, SC1, SC2)
C
        CHARACTER*3 OBSMOD,APER
        CHARACTER*5 GRAT
        INTEGER DET,SCLAMP,CARPOS
        COMMON /HRSMOD/ DET,SCLAMP,CARPOS
        COMMON /HRSMD1/ GRAT,OBSMOD,APER
C
C
C LOCAL VARIABLES
C
        INTEGER ISTAT,ID,I
        CHARACTER*8 NAME
        CHARACTER*80 CONTXT
        LOGICAL GMODE
        CHARACTER*5 GRATS(7)
        DATA GRATS/'G140M','G160M','G200M','G270M','G140L',
     $		   'ECH-A','ECH-B'/
C-----------------------------------------------------------------------------
C
C USE .DOH FILE
C
        ID=IDS(3)
C
C READ PROCESSING FLAGS
C
        NAME='PPC_CORR'
        CALL UHDGST(ID,NAME,FPPC,ISTAT)
        IF(ISTAT.NE.0)THEN
                FPPC='PERFORM'
                WRITE(CONTXT,99)NAME
99              FORMAT('Error: reading processing flag ',A8,
     *                  '- PERFORM will be used')
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='DIO_CORR'
        CALL UHDGST(ID,NAME,FDIO,ISTAT)
        IF(ISTAT.NE.0)THEN
                FDIO='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='PHC_CORR'
        CALL UHDGST(ID,NAME,FPHC,ISTAT)
        IF(ISTAT.NE.0)THEN
                FPHC='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='VIG_CORR'
        CALL UHDGST(ID,NAME,FVIG,ISTAT)
        IF(ISTAT.NE.0)THEN
                FVIG='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='DOP_CORR'
        CALL UHDGST(ID,NAME,FDOP,ISTAT)
        IF(ISTAT.NE.0)THEN
                FDOP='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='GWC_CORR'
        CALL UHDGST(ID,NAME,FGWC,ISTAT)
        IF(ISTAT.NE.0)THEN
                FGWC='OMIT'
                WRITE(CONTXT,101)NAME
 101            FORMAT('Warning: no processing flag ',A8,
     *                  '- OMIT will be used.')
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='ADC_CORR'
        CALL UHDGST(ID,NAME,FADC,ISTAT)
        IF(ISTAT.NE.0)THEN
                FADC='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='IAC_CORR'
        CALL UHDGST(ID,NAME,FIAC,ISTAT)
        IF(ISTAT.NE.0)THEN
                FIAC='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='VAC_CORR'
        CALL UHDGST(ID,NAME,FVAC,ISTAT)
        IF(ISTAT.NE.0)THEN
                FVAC='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='HEL_CORR'
        CALL UHDGST(ID,NAME,FHEL,ISTAT)
        IF(ISTAT.NE.0)THEN
                FHEL='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='BCK_CORR'
        CALL UHDGST(ID,NAME,FBCK,ISTAT)
        IF(ISTAT.NE.0)THEN
                FBCK='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='BMD_CORR'
        CALL UHDGST(ID,NAME,FBMD,ISTAT)
        IF(ISTAT.NE.0)THEN
                FBMD='OMIT'
                WRITE(CONTXT,101)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='MNF_CORR'
        CALL UHDGST(ID,NAME,FMNF,ISTAT)
        IF(ISTAT.NE.0)THEN
                FMNF='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='MDF_CORR'
        CALL UHDGST(ID,NAME,FMDF,ISTAT)
        IF(ISTAT.NE.0)THEN
                FMDF='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	ENDIF
        NAME='PLY_CORR'
        CALL UHDGST(ID,NAME,FPLY,ISTAT)
        IF(ISTAT.NE.0)THEN
                FPLY='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='ECH_CORR'
        CALL UHDGST(ID,NAME,FECH,ISTAT)
        IF(ISTAT.NE.0)THEN
                FECH='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='FLX_CORR'
        CALL UHDGST(ID,NAME,FFLX,ISTAT)
        IF(ISTAT.NE.0)THEN
                FFLX='PERFORM'
                WRITE(CONTXT,99)NAME
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        NAME='EXP_CORR'
        CALL UHDGST(ID,NAME,FEXP,ISTAT)
        IF(ISTAT.NE.0)THEN
                FEXP='PERFORM'
        ENDIF
        NAME='MAP_CORR'
        CALL UHDGST(ID,NAME,FMAP,ISTAT)
        IF(ISTAT.NE.0)THEN
                FMAP='PERFORM'
        ENDIF
        NAME='DQI_CORR'
        CALL UHDGST(ID,NAME,FDQI,ISTAT)
        IF(ISTAT.NE.0)THEN
                FDQI='PERFORM'
        ENDIF
        NAME='MER_CORR'
        CALL UHDGST(ID,NAME,FMER,ISTAT)
        IF(ISTAT.NE.0)THEN
                FMER='PERFORM'
        ENDIF
C
C DELETE PROCESSING STEPS NOT RELEVANT FOR GIVEN GRATING MODE
C
        GMODE=.FALSE.
        DO 10 I=1,7
                IF(GRAT.EQ.GRATS(I))GMODE=.TRUE.
10      CONTINUE
        IF(.NOT.GMODE)THEN
           IF(FADC.EQ.'PERFORM')THEN
              WRITE(CONTXT,11)GRAT
 11           FORMAT('WARNING: Grating mode ',a5,
     &             ' is not a physical grating')
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              WRITE(CONTXT,12)
 12           FORMAT('  Wavelengths will not be determined')
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           ENDIF
           FADC='OMIT'
           IF(FVIG.EQ.'PERFORM')THEN
              WRITE(CONTXT,11)GRAT
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              WRITE(CONTXT,14)
 14           FORMAT('  Vignetting will not be determined')
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           ENDIF
           FVIG='OMIT'
        ENDIF
        IF((GRAT.NE.'ECH-A').AND.(GRAT.NE.'ECH-B'))FECH='OMIT'
C
C CHECK FOR INCONSISTENCIES IN PROCESSING FLAGS 
C
        CALL ZFLCON
C
C READ REFERENCE FILE NAMES ------------------------------------------------
C
        IF(FDIO.EQ.'PERFORM')THEN
           NAME='DIOHFILE'
           CALL UHDGST(ID,NAME,DIOFIL,ISTAT)
           IF(ISTAT.NE.0)THEN
              WRITE(CONTXT,199)NAME
 199          FORMAT('Error reading reference file name ',A8,
     *             ' from .D0H file')
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              DIOFIL=' '
           ENDIF
        ENDIF
        IF(FPHC.EQ.'PERFORM')THEN
           NAME='PHCHFILE'
           CALL UHDGST(ID,NAME,PHCFIL,ISTAT)
           IF(ISTAT.NE.0)THEN
              WRITE(CONTXT,199)NAME
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              PHCFIL=' '
           ENDIF
        ENDIF
        IF(FVIG.EQ.'PERFORM')THEN
           NAME='VIGHFILE'
           CALL UHDGST(ID,NAME,VIGFIL,ISTAT)
           IF(ISTAT.NE.0)THEN
              WRITE(CONTXT,199)NAME
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              VIGFIL=' '
           ENDIF
        ENDIF
        IF(FFLX.EQ.'PERFORM')THEN
           NAME='ABSHFILE'
           CALL UHDGST(ID,NAME,ABSFIL,ISTAT)
           IF(ISTAT.NE.0)THEN
              WRITE(CONTXT,199)NAME
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              ABSFIL=' '
           ENDIF
           NAME='NETHFILE'
           CALL UHDGST(ID,NAME,NETFIL,ISTAT)
           IF(ISTAT.NE.0)THEN
              WRITE(CONTXT,199)NAME
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              NETFIL=' '
           ENDIF
        ENDIF
        IF(FDQI.EQ.'PERFORM')THEN
           NAME='DQIHFILE'
           CALL UHDGST(ID,NAME,DQIFIL,ISTAT)
           IF(ISTAT.NE.0)THEN
              WRITE(CONTXT,199)NAME
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              DQIFIL=' '
           ENDIF
        ENDIF
        IF(FBMD.EQ.'PERFORM')THEN
           NAME='SAAHFILE'
           CALL UHDGST(ID,NAME,SAAFIL,ISTAT)
           IF(ISTAT.NE.0)THEN
              WRITE(CONTXT,199)NAME
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              SAAFIL=' '
           ENDIF
        ENDIF
C
        ISTAT = 0
        RETURN
        END
