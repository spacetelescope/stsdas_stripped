C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YRDAIS(NAME,REFAPR,MAXLEN,IVS,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRDAIS
*
*  Keyphrase:
*  ----------
*       read inv. sens. reference file
*  Description:
*  ------------
*       This routine reads the inv. sens. reference file.
*       and checks its consistency.
*       DETECTOR, FGWA_ID, and POLAR_ID must match the observation's.
*       NXSTEP must be greater than or equal to the observation's.
*       FCHNL and NCHNLS must span range of the observation's values,
*	and OVERSCAN must be greater than the observation's.
*       IF NXSTEP, FCHNL and NCHNLS do match the observation's values,
*       the proper data points in then input vector are extracted.
*
*  FORTRAN name: YRDAIS.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                        I/O     Description / Comments
*       NAME                    I       inv. sens. reference file
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
*	1	Oct 94	H. Bushouse	Designed and coded (based on YRDIVS)
*       2       Sep 95  J. Eisenhamer   No longer error; set to DUMMY
*       2.1     Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*       2.2     Sep 98  M. De La Pena   Removed dead statements.
*-------------------------------------------------------------------------------
*
* inputs:
*       name - reference file name
*	refapr - reference aperture from CCSB table
*       maxlen - maximum vector length allowed in the file
* outputs:
*       ivs - MAXLEN array containing inv. sens. values
*	pedgre - reference file PEDIGREE keyword
*	descrp - reference file DESCRIP keyword
*       istat - error status
*----------------------------------------------------------------------------
	IMPLICIT NONE
C
	CHARACTER*3  REFAPR
        CHARACTER*64 NAME
	CHARACTER*68 PEDGRE, DESCRP
        INTEGER MAXLEN,ISTAT
        REAL IVS(MAXLEN)
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
        LOGICAL PAIRED
        REAL YUPPER,YLOWER
        COMMON /CCS1CM/PAIRED,YUPPER,YLOWER
C
C LOCAL VARIABLES
C
        CHARACTER*80 CONTXT
        INTEGER FC,NC,NXS,OSCN
C                                    --->reference file x-pattern
        INTEGER DEL,N,ISTATS(4),I,INPOS
        CHARACTER*5 DET1
        CHARACTER*3 FGWA1,APR1,POL1
        CHARACTER*8 KWORD(4)
        INTEGER IDIN,DIMEN(8),NAXIS,DTYPE
C                        --->file I/O parameters
        DATA KWORD/'FCHNL','NCHNLS','NXSTEPS','OVERSCAN'/
C------------------------------------------------------------------------------
C
C open file
C
        IF(NAME.EQ.'fnf')THEN
           CONTXT='WARNING: no AIS file specified'
           GO TO 997
        ENDIF
        CALL UIMOPN(NAME,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR opening inv. sens. file '//NAME
            GO TO 998
        ENDIF
C
C Get file size parameters and verify
C
        CALL UIMGID(IDIN,DTYPE,NAXIS,DIMEN,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading inv. sens. file '//NAME
            GO TO 999
        ENDIF
        IF(NAXIS.NE.1) THEN
            CONTXT='ERROR: inv. sens. file has '//
     *              'invalid dimensions '//NAME
            GO TO 999
        ENDIF
        IF(DIMEN(1).GT.MAXLEN)THEN
            CONTXT='ERROR: inv. sens. reference file to large '
            GO TO 999
        ENDIF
C
C get PEDIGREE and DESCRIP header keywords
C
        CALL UHDGST(IDIN,'PEDIGREE',PEDGRE,ISTAT)
        CALL UHDGST(IDIN,'DESCRIP', DESCRP,ISTAT)
C
C get detector from file and verify
C
        CALL UHDGST(IDIN,'DETECTOR',DET1,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='WARNING: DETECTOR keyword missing from the '//
     *              'inv. sens. file'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ELSE
            IF((DET.NE.DET1).AND.(DET1.NE.'ANY'))THEN
                CONTXT='ERROR: DETECTOR keyword value in the '//
     *              'inv. sens. file does not match observation'
                GO TO 999
            ENDIF
        ENDIF
C
C get fgwa_id from file and verify
C
        CALL UHDGST(IDIN,'FGWA_ID',FGWA1,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='WARNING: FGWA_ID keyword missing from the '//
     *              'inv. sens. file'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ELSE
            IF((FGWAID.NE.FGWA1).AND.(FGWA1.NE.'ANY'))THEN
                CONTXT='ERROR: FGWAID keyword value in the '//
     *              'inv. sens. file does not match observation'
                GO TO 999
            ENDIF
        ENDIF
C
C get aperture id from file and verify
C
        CALL UHDGST(IDIN,'APER_ID',APR1,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='WARNING: APER_ID keyword missing from the '//
     *              'inv. sens. file'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ELSE
            IF((REFAPR.NE.APR1).AND.(APR1.NE.'ANY').AND.
     *          (REFAPR.NE.'ANY'))THEN
                CONTXT='ERROR: APER_ID keyword value in the '//
     *              'AIS file does not match CCSB table'
                GO TO 999
            ENDIF
        ENDIF
C
C get polar_id from file and verify
C
        CALL UHDGST(IDIN,'POLAR_ID',POL1,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='WARNING: POLAR_ID keyword missing from the '//
     *              'inv. sens. file'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ELSE
            IF((POLID.NE.POL1).AND.(POL1.NE.'N'))THEN
                CONTXT='ERROR: POLAR_ID keyword value in the '//
     *              'inv. sens. file does not match observation'
                GO TO 999
            ENDIF
        ENDIF
C
C Read pattern information
C
        CALL UHDGSI(IDIN,KWORD(1),FC,ISTATS(1))
        CALL UHDGSI(IDIN,KWORD(2),NC,ISTATS(2))
        CALL UHDGSI(IDIN,KWORD(3),NXS,ISTATS(3))
        CALL UHDGSI(IDIN,KWORD(4),OSCN,ISTATS(4))
        DO 300 I=1,4
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR: reading '//KWORD(I)//' from '//NAME
                GO TO 999
            ENDIF
300     CONTINUE
C
C Check consistency
C
        IF((OVERSN.NE.OSCN).OR.(FC.GT.FCHNL).OR.
     *     ((FC+NC).LT.(FCHNL+NCHNLS)).OR.
     *     (NXS.LT.NXSTEP) )THEN
                CONTXT='ERROR:Pattern information incompatible in '//
     *                  NAME
                GO TO 999
        ENDIF
C
C Read inv. sens. values
C
        CALL UIGL1R(IDIN,IVS,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading inv. sens. file '//NAME
            GO TO 999
        ENDIF
C
C If NXSTEP or FCHNL do not match file, extract right data
C
        IF((NXSTEP.NE.NXS).OR.(FC.NE.FCHNL))THEN
            N = NXSTEP*(NCHNLS+OVERSN-1)
            INPOS = (FCHNL-FC)*NXS+1
            DEL = NXS/NXSTEP
            DO 400 I=1,N
                IVS(I)=IVS(INPOS)
                INPOS=INPOS+DEL
400         CONTINUE
        ENDIF
        CALL UIMCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
C
C That's all folks
C
 997    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        DESCRP=CONTXT
        PEDGRE='DUMMY'
        ISTAT=0
        GO TO 1000
 999    CALL UIMCLO(IDIN,ISTAT)
 998    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
 1000   RETURN
        END
