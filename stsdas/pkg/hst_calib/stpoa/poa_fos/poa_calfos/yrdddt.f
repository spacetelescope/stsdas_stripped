C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YRDDDT(NAME,DDT,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRDDDT
*
*  Keyphrase:
*  ----------
*       read disabled diode reference file
*  Description:
*  ------------
*       This routine reads the disabled diode reference file,
*       verifies the detector of the file and returns the portion
*       specified by FCHNL and NCHNL
*
*  FORTRAN name: yrdDDT.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                        I/O     Description / Comments
*       NAME                    I       data quality reference file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*       uimopn, uhdgst, uhdgsi, uimgid, uuopgr
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jul 89  D. Lindler      Designed and coded
*	2	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*       2.1     Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* inputs:
*       name - reference file name
* outputs:
*       DDT - data quality values
*	pedgre - DDTHFILE PEDIGREE keyword string
*	descrp - DDTHFILE DESCRIP  keyword string
*       istat - error status
*
        CHARACTER*64 NAME
	CHARACTER*68 PEDGRE, DESCRP
        INTEGER ISTAT
        REAL DDT(*)
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
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
C
C LOCAL VARIABLES
C
        CHARACTER*80 CONTXT
        CHARACTER*5 DET1
        INTEGER I
        INTEGER IDIN,DIMEN(8),NAXIS,DTYPE
C                        --->file I/O parameters
C------------------------------------------------------------------------------
C
C open file
C
        CALL UIMOPN(NAME,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR opening data quality ref. file '//NAME
            GO TO 999
        ENDIF
C
C Get file size parameters and verify
C
        CALL UIMGID(IDIN,DTYPE,NAXIS,DIMEN,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading background file '//NAME
            GO TO 998
        ENDIF
        IF(NAXIS.NE.1) THEN
            CONTXT='ERROR: data quality ref. file has '//
     *              'invalid dimensions '//NAME
            GO TO 998
        ENDIF
        IF(DIMEN(1).GT.512)THEN
            CONTXT='ERROR: data quality ref. vector'//
     *                  ' not 512 elements '
            GO TO 998
        ENDIF
C
C get detector from file and verify
C
        CALL UHDGST(IDIN,'DETECTOR',DET1,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='WARNING: DETECTOR keyword missing from the '//
     *              'DDTHFILE'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
          ELSE
            IF((DET.NE.DET1).AND.(DET1.NE.'ANY'))THEN
                CONTXT='ERROR: DETECTOR keyword value in the '//
     *              'DDTHFILE file does not match observation'
                    GO TO 998
            ENDIF
        ENDIF
C
C get PEDIGREE and DESCRIP keywords from file
C
        CALL UHDGST(IDIN,'PEDIGREE',PEDGRE,ISTAT)
        CALL UHDGST(IDIN,'DESCRIP', DESCRP,ISTAT)
C
C Read  values
C
        CALL UIGL1R(IDIN,DDT,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading DDTHFILE '//NAME
            GO TO 998
        ENDIF
        IF(FCHNL.NE.0)THEN
                DO 300 I=1,NCHNLS
                        DDT(I)=DDT(I+FCHNL)
300         CONTINUE
        ENDIF
        CALL UIMCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
C
C ERROR SECTION
C
998     CALL UIMCLO(IDIN,ISTAT)
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
