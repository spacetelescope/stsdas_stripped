        SUBROUTINE YRDIVS(KEYWRD,NAME,MAXLEN,FOUND,IVS,PEDGRE,
     *                    DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRDIVS
*
*  Keyphrase:
*  ----------
*       read inv. sens. reference file
*  Description:
*  ------------
*       This routine reads the inv. sens. reference file.
*       and checks its consistency.
*       DETECTOR, FGWA_ID, POLAR_ID, APER_ID, PASS_DIR and
*	APER_POS must match the observation's.
*       NXSTEP must be greater than or equal to the observation's.
*       FCHNL and NCHNLS must span range of the observation's values,
*	and OVERSCAN must be greater than the observation's.
*       IF NXSTEP, FCHNL and NCHNLS do match the observation's values,
*       the proper data points in then input vector are extracted.
*
*  FORTRAN name: YRDIVS.for
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
*       1       Jul 89  D. Lindler      Designed and coded
*	2	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*       2.1     Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* inputs:
*       keyword - for which had the file name in the .D0H header
*       name - reference file name
*       maxlen - maximum vector length allowed in the file
* outputs:
*       found - logical flags (2 elements) showing which inv. sens.s was read.
*       IVS - MAXLEN x 2 array containing inv. sens.s for one or both apertures.
*               or both pass directions
*	pedgre - reference file PEDIGREE keyword
*	descrp - reference file DESCRIP keyword
*       istat - error status
*
        CHARACTER*9 KEYWRD
        CHARACTER*64 NAME
	CHARACTER*68 PEDGRE, DESCRP
        INTEGER MAXLEN,ISTAT
        LOGICAL FOUND(2)
        REAL IVS(MAXLEN,2)
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
        INTEGER IPOS,DEL,N,ISTATS(4),I,INPOS
        CHARACTER*5 DET1
        CHARACTER*3 FGWA1,APER1,POL1
        CHARACTER*6 APOS1
        INTEGER PASS1
        CHARACTER*8 KWORD(4)
        INTEGER IDIN,DIMEN(8),NAXIS,DTYPE
C                        --->file I/O parameters
        DATA KWORD/'FCHNL','NCHNLS','NXSTEPS','OVERSCAN'/
C------------------------------------------------------------------------------
C
C open file
C
        CALL UIMOPN(NAME,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR opening inv. sens. file '//KEYWRD//NAME
            GO TO 999
        ENDIF
C
C Get file size parameters and verify
C
        CALL UIMGID(IDIN,DTYPE,NAXIS,DIMEN,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading inv. sens. file '//NAME
            GO TO 998
        ENDIF
        IF(NAXIS.NE.1) THEN
            CONTXT='ERROR: inv. sens. file has '//
     *              'invalid dimensions '//NAME
            GO TO 998
        ENDIF
            IF(DIMEN(1).GT.MAXLEN)THEN
                CONTXT='ERROR: inv. sens. reference file to large '
                GO TO 998
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
                    GO TO 998
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
                    GO TO 998
            ENDIF
        ENDIF
C
C get aper_id from file and verify
C
        CALL UHDGST(IDIN,'APER_ID',APER1,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='WARNING: APER_ID keyword missing from the '//
     *              'inv. sens. file'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
          ELSE
            IF((APERID.NE.APER1).AND.(APER1.NE.'ANY'))THEN
                CONTXT='ERROR: APER_ID keyword value in the '//
     *              'inv. sens. file does not match observation'
                    GO TO 998
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
                    GO TO 998
            ENDIF
        ENDIF
C
C Determine aperture position for paired aperture
C
        IPOS=0
        IF(PAIRED)THEN
            CALL UHDGST(IDIN,'APER_POS',APOS1,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR: APER_POS keyword missing from inv.'//
     *                  ' sens file'
                GO TO 999
            ENDIF
            IF(APOS1.EQ.'UPPER')IPOS=1
            IF(APOS1.EQ.'LOWER')IPOS=2
            IF(IPOS.EQ.0)THEN
                CONTXT='ERROR: invalid APER_POS in inv. sens. file'
                GO TO 999
            ENDIF
        ENDIF
C
C Get pass_direction if polar_id ne 'C'
C
        IF(POLID.NE.'C')THEN
            CALL UHDGSI(IDIN,'PASS_DIR',PASS1,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR: PASS_DIR keyword missing from inv.'//
     *                  ' sens. file'
                GO TO 999
            ENDIF
            IPOS=PASS1
            IF((PASS1.LT.1).OR.(PASS1.GT.2))THEN
                CONTXT='ERROR: invalid PASS_DIR in inv. sens. file'
                GO TO 999
            ENDIF
        ENDIF
C
C Single aperture (polar_id='C')
C
        IF(IPOS.EQ.0)IPOS=1
C
C READ pattern information
C
        CALL UHDGSI(IDIN,KWORD(1),FC,ISTATS(1))
        CALL UHDGSI(IDIN,KWORD(2),NC,ISTATS(2))
        CALL UHDGSI(IDIN,KWORD(3),NXS,ISTATS(3))
        CALL UHDGSI(IDIN,KWORD(4),OSCN,ISTATS(4))
        DO 300 I=1,4
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR: reading '//KWORD(I)//
     *                  ' from '//KEYWRD//NAME
                GO TO 998
            ENDIF
300     CONTINUE
C
C CHECK consistency
C
        IF((OVERSN.NE.OSCN).OR.(FC.GT.FCHNL).OR.
     *          ( (FC+NC).LT.(FCHNL+NCHNLS) ).OR.
     *          (NXS.LT.NXSTEP) )THEN
                CONTXT='ERROR:Pattern information incompatible in '//
     *                  KEYWRD//NAME
                GO TO 998
        ENDIF
C
C Read inv. sens. values
C
        CALL UIGL1R(IDIN,IVS(1,IPOS),ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading inv. sens. file '//NAME
            GO TO 998
        ENDIF
C
C IF NXSTEP of FCHNL do not match file, extract right data
C
        IF((NXSTEP.NE.NXS).OR.(FC.NE.FCHNL))THEN
            N = NXSTEP*(NCHNLS+OVERSN-1)
            INPOS = (FCHNL-FC)*NXS+1
            DEL = NXS/NXSTEP
            DO 400 I=1,N
                IVS(I,IPOS)=IVS(INPOS,IPOS)
                INPOS=INPOS+DEL
400         CONTINUE
        ENDIF
        FOUND(IPOS)=.TRUE.
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
