C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YRCCS4(CCS4,DET,FGWA,POLID,ALPHA,W1,COMBPX,
     *                    COEF,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCS4
*
*  Keyphrase:
*  ----------
*       read polarimetry table ccs4
*
*  Description:
*  ------------
*       This routines reads the angle of each pass direction of the
*       Wollastan with respect to the Q = 1 coordinate axis of the
*       polarization reference frame.  It also reads the initial
*       waveplate position.
*
*  FORTRAN name: YRCCS4.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCS4                    I       table containing CCS4 coeffecients
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
*       CCS4 - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*       FGWA - disperser
*       POLID - polarizer id
*
* Output parameters
*       ALPHA - angles with respect to Q=1 coordinate axis
*       W1 - initial position of the waveplate
*       COMBPX - diode to compute wave. shift in two pass dir.
*       COEF - Interference coefficients
*	PEDGRE - CCS4 PEDIGREE keyword
*	DESCRP - CCS4 DESCRIP keyword
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
        INTEGER ISTAT,COMBPX
        CHARACTER*64 CCS4
	CHARACTER*68 PEDGRE, DESCRP
        CHARACTER*5 DET
        CHARACTER*3 FGWA
        CHARACTER*1 POLID
        REAL ALPHA(2),W1
        DOUBLE PRECISION COEF(7)
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
        INTEGER IDIN,COLIDS(14),ROW,NROWS
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(14)
        CHARACTER*5 DET1
        CHARACTER*3 FGWA1
        CHARACTER*1 POLID1
        LOGICAL NULL(14)
        DATA COLNAM/'DETECTOR','FGWA_ID','POLAR_ID','ALPHA1','ALPHA2',
     *              'W1','COMBPX','A','B','C1','C2','C3','C4','C5'/
C---------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCS4,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCS4 table '//CCS4
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
                CONTXT='ERROR reading CCS4 table '//CCS4
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,14,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCS4 table '//
     *                  CCS4
                GO TO 999
        ENDIF
C
C Loop on rows
C
        DO 500 ROW=1,NROWS
C
C check detector
C
                CONTXT='ERROR reading CCS4 table '//CCS4
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,DET1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(DET1.NE.DET) GO TO 500
C
C Check grating
C
                CALL UTRGTT(IDIN,COLIDS(2),1,ROW,FGWA1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(FGWA1.NE.FGWA) GO TO 500
C
C Check polar-id
C
                CALL UTRGTT(IDIN,COLIDS(3),1,ROW,POLID1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(POLID1.NE.POLID) GO TO 500
C
C read values
C
                CALL UTRGTR(IDIN,COLIDS(4),2,ROW,ALPHA,NULL,ISTAT)
                IF(ISTAT.NE.0) GO TO 999
                CALL UTRGTR(IDIN,COLIDS(6),1,ROW,W1,NULL,ISTAT)
                IF(ISTAT.NE.0) GO TO 999
                CALL UTRGTI(IDIN,COLIDS(7),1,ROW,COMBPX,NULL,ISTAT)
                IF(ISTAT.NE.0) GO TO 999
                CALL UTRGTD(IDIN,COLIDS(8),7,ROW,COEF,NULL,ISTAT)
                IF(ISTAT.NE.0) GO TO 999
                CALL UTTCLO(IDIN,ISTAT)
                ISTAT=0
                GO TO 1000
500     CONTINUE
C
C If we made it here, no rows found
C
        WRITE(CONTXT,99)DET,FGWA,POLID
99      FORMAT('ERROR: No rows found in CCS4 for ',A5,1X,A3,
     *             ' polar_id= ',A1)
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
