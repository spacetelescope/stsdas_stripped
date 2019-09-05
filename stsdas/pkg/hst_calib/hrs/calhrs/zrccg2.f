        SUBROUTINE ZRCCG2(CCG2,DET,TAU1,THRESH,Q0,Q1,F,
     $		RSAT,R20,R5,ISTAT)
*
*  Module number:
*
*  Module name: ZRCCG2
*
*  Keyphrase:
*  ----------
*       read paired pulse relation table CCG2.
*
*  Description:
*  ------------
*       This routine reads the paired pulse coefficient table.
*       Values for the HRS row with the sepcified detector are returned.
*
*  FORTRAN name: zrccg2.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       ccg2                    I       table containing CCG2 coeffecients
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZMSPUT, uttopn, utpgti, utcfnd, utrgt*, uttclo
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 88  D. Lindler      Designed and coded
*	1.1	May 91	S. Hulbert	Added new data quality
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCG2 - table name
*       DET  - detector number
*
* Output parameters
*
*       TAU1 - Time constant for first paired pulse formation
*       THRESH - Count rate threshold for paired pulse correction
*       Q0 - zeroth order constant for computing tau2
*       Q1 - first order constant for computing tau2
*       F - theshold used to compute linear varying tau2
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
        INTEGER DET,ISTAT
        REAL TAU1,THRESH,Q0,Q1,F
	REAL RSAT, R20, R5
        CHARACTER*64 CCG2
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
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
        INTEGER IDIN,COLIDS(10),ROW,IDET,I,ISTATS(10),NROWS
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(10)
        CHARACTER*3 INST
        LOGICAL NULL
        DATA COLNAM/'INSTRUMENT','DETECTOR','TAU1','THRESHOLD','Q0',
     *                  'Q1','F','RSAT','R20','R5'/
C
C---------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCG2,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCG2 table '//CCG2
                GO TO 998
        ENDIF
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCG2 table '//CCG2
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,10,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCG2 table '//
     *                  CCG2
                GO TO 999
        ENDIF
C
C Locate row with correct detector number and instrument
C
        DO 10 ROW=1,NROWS
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,INST,NULL,ISTATS(1))
                CALL UTRGTI(IDIN,COLIDS(2),1,ROW,IDET,NULL,ISTATS(2))
                IF((ISTATS(1).NE.0).OR.(ISTATS(2).NE.0))THEN
                        CONTXT='ERROR reading CCG2 table '//CCG2
                        GO TO 999
                ENDIF
                IF((DET.EQ.IDET).AND.(INST.EQ.'HRS'))GO TO 20
10      CONTINUE
C
C if we made it here, we did not find a correct row
C
        WRITE(CONTXT,99)DET
99      FORMAT('ERROR no row in CCG2 table found for HRS detector ',I2)
        GO TO 999
C
C read values
C
20      CALL UTRGTR(IDIN,COLIDS(3),1,ROW,TAU1,NULL,ISTATS(1))
        CALL UTRGTR(IDIN,COLIDS(4),1,ROW,THRESH,NULL,ISTATS(2))
        CALL UTRGTR(IDIN,COLIDS(5),1,ROW,Q0,NULL,ISTATS(3))
        CALL UTRGTR(IDIN,COLIDS(6),1,ROW,Q1,NULL,ISTATS(4))
        CALL UTRGTR(IDIN,COLIDS(7),1,ROW,F,NULL,ISTATS(5))
        CALL UTRGTR(IDIN,COLIDS(8),1,ROW,RSAT,NULL,ISTATS(6))
        CALL UTRGTR(IDIN,COLIDS(9),1,ROW,R20,NULL,ISTATS(7))
        CALL UTRGTR(IDIN,COLIDS(10),1,ROW,R5,NULL,ISTATS(8))
        DO 30 I=1,8
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR reading CCG2 table '//CCG2
                GO TO 999
            ENDIF
30      CONTINUE
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
