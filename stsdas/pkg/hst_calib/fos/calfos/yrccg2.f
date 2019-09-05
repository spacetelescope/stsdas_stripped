        SUBROUTINE YRCCG2(CCG2,DET,TAU1,THRESH,Q0,Q1,F,RSAT,
     *                    R20,R5,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCG2
*
*  Keyphrase:
*  ----------
*       read paired pulse relation table CCG2.
*
*  Description:
*  ------------
*       This routine reads the paired pulse coefficient table.
*       Values for the FOS row with the sepcified detector are returned.
*
*  FORTRAN name: YRCCG2.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       ccg2                    I       table containing CCG2 coeffecients
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
*       CCG2 - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*
* Output parameters
*
*       TAU1 - Time constant for first paired pulse formation
*       THRESH - Count rate threshold for paired pulse correction
*       Q0 - zeroth order constant for computing tau2
*       Q1 - first order constant for computing tau2
*       F - theshold used to compute linear varying tau2
*       RSAT - saturation input count rate level
*	R20 - count rate for 20% error in paired pulse correction
*	R5 - count rate for 5% error in correction
*	PEDGRE - CCG2 PEDIGREE keyword string
*	DESCRP - CCG2 DESCRIP  keyword string
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
        INTEGER ISTAT
        REAL TAU1,THRESH,Q0,Q1,F,RSAT,R5,R20
        CHARACTER*64 CCG2
        CHARACTER*5 DET
	CHARACTER*68 PEDGRE,DESCRP
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
        INTEGER IDIN,COLIDS(10),ROW,IDET,INTD,I,ISTATS(10),NROWS
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(10)
        CHARACTER*3 INST
        LOGICAL NULL
        DATA COLNAM/'INSTRUMENT','DETECTOR','TAU1','THRESHOLD','Q0',
     *                  'Q1','F','RSAT','R20','R5'/
C
C---------------------------------------------------------------------------
C
C determine detector number
C
        INTD=0
        IF(DET.EQ.'AMBER')INTD=1
        IF(DET.EQ.'BLUE')INTD=2
        IF(INTD.EQ.0)THEN
                WRITE(CONTXT,88)DET
88              FORMAT('ERROR: Invalid detector name = ',A5)
                GO TO 998
        ENDIF
C
C Open table
C
        CALL UTTOPN(CCG2,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCG2 table '//CCG2
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
                IF((INTD.EQ.IDET).AND.(INST.EQ.'FOS'))GO TO 20
10      CONTINUE
C
C if we made it here, we did not find a correct row
C
        WRITE(CONTXT,99)INTD
99      FORMAT('ERROR no row in CCG2 table found for FOS detector ',I2)
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
C
        CALL UTTCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
C
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
