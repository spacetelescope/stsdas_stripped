        SUBROUTINE YRCCS9(CCS9,DET,FGWA,POS,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCS9
*
*  Keyphrase:
*  ----------
*       read scattered light parameter table
*
*  Description:
*  ------------
*       This routine reads the table containing the beginning and
*       ending diode numbers of the regions of no sensitivity
*       from which the contribution of scattered light is measured.
*       Rows are selected by detector and fgwa_id.
*
*  FORTRAN name: YRCCS9.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCS9                    I       table containing CCS9 coeffecients
*
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
*	1	Mar 94	H. Bushouse	Designed and coded
*	2	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCS9 - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*       FGWA - grating mode
*
* Output parameters
*       POS - first and last diode values of no sensitivity range
*	PEDGRE - CCS9 PEDIGREE keyword
*	DESCRP - CCS9 DESCRIP  keyword
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
C
C     INPUT/OUTPUT DECLARATIONS
C
	CHARACTER*64 CCS9
	CHARACTER*5 DET
	CHARACTER*3 FGWA
	CHARACTER*68 PEDGRE, DESCRP
	REAL POS(2)
	INTEGER ISTAT
C
C     BEGIN IRAF77.INC
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
	INTEGER IDIN,COLIDS(4),ROW,NROWS
	CHARACTER*80 CONTXT
	CHARACTER*15 COLNAM(4)
	CHARACTER*5 DET1
	CHARACTER*3 FGWA1
	LOGICAL NULL(2)
	DATA COLNAM/'DETECTOR','FGWA_ID','RANGE_BEG','RANGE_END'/
C---------------------------------------------------------------------------
C
C Initialize range to zero
C
	POS(1) = 0.0
	POS(2) = 0.0
C
C Open table
C
        CALL UTTOPN(CCS9,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCS9 table '//CCS9
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
                CONTXT='ERROR reading CCS9 table '//CCS9
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,4,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCS9 table '//
     *                  CCS9
                GO TO 999
        ENDIF
C
C Loop on rows
C
        DO 500 ROW=1,NROWS
C
C check detector
C
                CONTXT='ERROR reading CCS9 table '//CCS9
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,DET1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(DET1.NE.DET) GO TO 500
C
C Check fgwa_id
C
                CALL UTRGTT(IDIN,COLIDS(2),1,ROW,FGWA1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(FGWA1.NE.FGWA) GO TO 500
C
C read limits
C
                CALL UTRGTR(IDIN,COLIDS(3),2,ROW,POS,NULL,ISTAT)
                IF(ISTAT.NE.0) GO TO 999
		GO TO 600
500	CONTINUE
C
C If we got here no applicable row was found
C
	WRITE(CONTXT,98)DET,FGWA
98	FORMAT('ERROR: No row found in CCS9 for ',A5,1X,A3)
	GO TO 999
600	CONTINUE
C
C If we got here we found a row
C
        CALL UTTCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
