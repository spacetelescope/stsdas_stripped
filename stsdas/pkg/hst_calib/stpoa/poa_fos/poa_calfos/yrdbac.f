C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YRDBAC(NAME,MAXLEN,BACK,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRDBAC
*
*  Keyphrase:
*  ----------
*       read background reference file
*  Description:
*  ------------
*       This routine reads the background reference file.
*       and checks its consistency.
*       DETECTOR and OVERSCAN must match observation's.
*       NXSTEP must be greater than or equal to the observation's.
*       FCHNL and NCHNLS must span range of the observation's values,
*
*       IF NXSTEP, FCHNL and NCHNLS do match the observation's values,
*       the proper data points in then input vector are extracted.
*
*  FORTRAN name: yrdbac.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                        I/O     Description / Comments
*       NAME                    I       background reference file
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
*       maxlen - maximum vector length allowed in the file
* outputs:
*       back - background vector
*	pedgre - BACHFILE PEDIGREE keyword
*	descrp - BACHFILE DESCRIP  keyword
*       istat - error status
*
*-------------------------------------------------------------------------------
        CHARACTER*64 NAME
	CHARACTER*68 PEDGRE,DESCRP
        INTEGER MAXLEN,ISTAT
        REAL BACK(MAXLEN)
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
        INTEGER FC,NC,NXS,OSCN
C                                    --->reference file x-pattern
        INTEGER IPOS,DEL,N,ISTATS(4),I
        CHARACTER*5 DET1
        CHARACTER*8 KEYWRD(4)
        INTEGER IDIN,DIMEN(8),NAXIS,DTYPE
C                        --->file I/O parameters
        DATA KEYWRD/'FCHNL','NCHNLS','NXSTEPS','OVERSCAN'/
C------------------------------------------------------------------------------
C
C open file
C
        CALL UIMOPN(NAME,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR opening background file '//NAME
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
            CONTXT='ERROR: background file has '//
     *              'invalid dimensions '//NAME
            GO TO 998
        ENDIF
            IF(DIMEN(1).GT.MAXLEN)THEN
                CONTXT='ERROR: background reference file to large '
                GO TO 998
            ENDIF
C
C get PEDIGREE and DESCRIP header keywords
C
	CALL UHDGST(IDIN,'PEDIGREE',PEDGRE,ISTAT)
	CALL UHDGST(IDIN,'DESCRIP', DESCRP,ISTAT)
C
C if the table contains dummy data, then don't bother reading it
C
	IF (PEDGRE(1:5).EQ.'DUMMY') THEN
            CALL UIMCLO(IDIN,ISTAT)
            GO TO 1000
	END IF
C
C get detector from file and verify
C
        CALL UHDGST(IDIN,'DETECTOR',DET1,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='WARNING: DETECTOR keyword missing from the '//
     *              'background file'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
          ELSE
            IF((DET.NE.DET1).AND.(DET1.NE.'ANY'))THEN
                CONTXT='ERROR: DETECTOR keyword value in the '//
     *              'background file does not match observation'
                    GO TO 998
            ENDIF
        ENDIF
C
C READ pattern information
C
        CALL UHDGSI(IDIN,KEYWRD(1),FC,ISTATS(1))
        CALL UHDGSI(IDIN,KEYWRD(2),NC,ISTATS(2))
        CALL UHDGSI(IDIN,KEYWRD(3),NXS,ISTATS(3))
        CALL UHDGSI(IDIN,KEYWRD(4),OSCN,ISTATS(4))
        DO 300 I=1,4
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR: reading '//KEYWRD(I)//
     *                  ' from BACFILE '//NAME
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
     *                  'BACFILE '//NAME
                GO TO 998
        ENDIF
C
C Read background values
C
        CALL UIGL1R(IDIN,BACK,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading background file '//NAME
            GO TO 998
        ENDIF
C
C IF NXSTEP of FCHNL do not match file, extract right data
C
        IF((NXSTEP.NE.NXS).OR.(FC.NE.FCHNL))THEN
            N = NXSTEP*(NCHNLS+OVERSN-1)
            IPOS = (FCHNL-FC)*NXS+1
            DEL = NXS/NXSTEP
            DO 400 I=1,N
                BACK(I)=BACK(IPOS)
                IPOS=IPOS+DEL
400         CONTINUE
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
