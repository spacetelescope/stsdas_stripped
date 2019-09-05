C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YRCCS3(CCS3,DET,BCKMD,BCKMN,SKYMD,SKYMN,
     *                    PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCS3
*
*  Keyphrase:
*  ----------
*       read filter width table
*
*  Description:
*  ------------
*       This routine reads the table contianing the filter widths for
*       smoothing the background and sky spectra.  The proper values are
*       selected by detector.
*
*  FORTRAN name: YRCCS3.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCS3                    I       table containing CCS3 coeffecients
*  Subroutines Called:
*  -------------------
*  CDBS
*       ymsput
*  SDAS:
*       uttopn, utpgti, utcfnd, utrgt*, uttclo
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       July 89  D. Lindler      Designed and coded
*	2	Mar 94	 H. Bushouse	 Mods to handle PEDIGREE keywords
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCS3 - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*
* Output parameters
*       BCKMD - background median filter width
*       BCKMN - background mean filter width
*       SKYMD - sky median filter width
*       SKYMN - sky mean filter width
*	PEDGRE - CCS3 PEDIGREE keyword string
*	DESCRP - CCS3 DESCRIP  keyword string
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
        INTEGER ISTAT
        CHARACTER*64 CCS3
        CHARACTER*5 DET
	CHARACTER*68 PEDGRE,DESCRP
        INTEGER BCKMD,BCKMN,SKYMD,SKYMN
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
C     THESE MAY BE READ BY UTPGTI BUT MAY NOT BE SET:
C
C                                       NUMBER OF ROWS WRITTEN TO
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
C     END IRAF77.INC
C
C local variables
C
        INTEGER IDIN,COLIDS(5),ROW,I,NROWS,ISTATS(4)
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(5)
        CHARACTER*5 DET1
        LOGICAL NULL
        DATA COLNAM/'DETECTOR','BCK_MD','BCK_MN','SKY_MD','SKY_MN'/
C---------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCS3,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCS3 table '//CCS3
                GO TO 998
        ENDIF
C
C get PEDIGREE and DESCRIP header keywords
C
        CALL UTHGTT(IDIN,'PEDIGREE',PEDGRE,ISTAT)
        CALL UTHGTT(IDIN,'DESCRIP',DESCRP,ISTAT)
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCS3 table '//CCS3
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,5,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCS3 table '//
     *                  CCS3
                GO TO 999
        ENDIF
C
C Loop on rows
C
        DO 500 ROW=1,NROWS
C
C check detector
C
                CONTXT='ERROR reading CCS3 table '//CCS3
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,DET1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(DET1.NE.DET) GO TO 500
C
C read values
C
                CALL UTRGTI(IDIN,COLIDS(2),1,ROW,BCKMD,NULL,ISTATS(1))
                CALL UTRGTI(IDIN,COLIDS(3),1,ROW,BCKMN,NULL,ISTATS(2))
                CALL UTRGTI(IDIN,COLIDS(4),1,ROW,SKYMD,NULL,ISTATS(3))
                CALL UTRGTI(IDIN,COLIDS(5),1,ROW,SKYMN,NULL,ISTATS(4))
                DO 450 I=1,4
                    IF(ISTATS(I).NE.0)THEN
                        CONTXT='ERROR reading CCR3 table '//CCS3
                        GO TO 999
                    ENDIF
450             CONTINUE
                GO TO 600
500     CONTINUE
C
C If we got here (no row found)
C
        WRITE(CONTXT,99)DET
99      FORMAT('ERROR: No rows found in CCS3 for ',A5,' detector')
        GO TO 999
600     CALL UTTCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
