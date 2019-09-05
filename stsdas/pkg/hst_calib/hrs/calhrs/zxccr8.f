        SUBROUTINE ZXCCR8(CCR8,GRAT,APER,ORDER,ID,
     *                  CARPOS,ROWS,NFOUND,ISTAT)
*
*  Module number:
*
*  Module name: ZXCCR8
*
*  Keyphrase:
*  ----------
*       Index table CCR8 (GHRS incidence angle correction)
*
*  Description:
*  ------------
*       This routine reads table CCR8 and extracts the carrousel positions
*       and there row numbers for the specified grating, aperture, and order.
*       A sorted list of carrousel positions and row index is returned.
*  FORTRAN name: ZXCCR8.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCR8                    I       incidence angle coef. table
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZMSPUT, uttopn, utpgti, utcfnd, utrgt*, uttclo
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       APR 89  D. Lindler      Designed and coded
*	1.1	Sep 91	S. Hulbert	Implemneted dynamic memory allocation
*					for storing the table
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCR8 - table name (character*64)
*       GRAT - grating mode (character*5)
*       APER - aperture (LSA, SC1, or SC2)
*       ORDER - spectral order.
*       CARPOS - pointer to vector of carrousel positions (sorted)
*       ROWS - pointer to row numbers for each y-deflection
*
* Output parameters
*
*       NFOUND - number of carrousel positions found
*       istat - ERROR status (integer)
*
**************************************************************************
        CHARACTER*64 CCR8
        INTEGER ISTAT,NFOUND,ROWS,ID,ORDER,CARPOS
        CHARACTER*3 APER
        CHARACTER*5 GRAT
C------------------------------------------------------------------------------
C Get IRAF MEM common into main program.
C
      LOGICAL          MEMB(1)
      INTEGER*2        MEMS(1)
      INTEGER*4        MEMI(1)
      INTEGER*4        MEML(1)
      REAL             MEMR(1)
      DOUBLE PRECISION MEMD(1)
      COMPLEX          MEMX(1)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
C------------------------------------------------------------------------------
      INTEGER TYINT
      PARAMETER (TYINT=4)
      INTEGER TYREAL
      PARAMETER (TYREAL=6)
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
C     THIS SECTION IS FOR PARAMETERS RELEVANT TO TABLE I/O.
C
C
C     THESE MAY BE READ BY UTPGTI BUT MAY NOT BE SET:
C
C                                       NUMBER OF ROWS WRITTEN TO
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
C     END IRAF77.INC
C
C LOCAL VARIABLES -------------------------------------------
C
        INTEGER NROWS,COLIDS(4),ROW,SPORD,N
	INTEGER ISTATS(2)
        CHARACTER*3 AP
        CHARACTER*5 GMODE
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(4)
        LOGICAL NULL,APFLAG
        DATA COLNAM/'GRATING','SPORDER','CARPOS','APERTURE'/
C-------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCR8,RDONLY,ID,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCR8 table '//CCR8
                GO TO 999
        ENDIF
C
C get number of rows
C
        CALL UTPGTI(ID,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCR8 table '//CCR8
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(ID,COLNAM,3,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCR8 table '//
     *                  CCR8
                GO TO 999
        ENDIF
C
C see if aperture column is available, if not all apertures are assumed
C to be LSA
C
        CALL UTCFND(ID,COLNAM(4),1,COLIDS(4),ISTAT)
        IF(ISTAT.NE.0)THEN
            CALL ZMSPUT('WARNING: no aperture column in CCR8 table',
     *                  STDOUT,0,ISTAT)
            APFLAG=.FALSE.
            AP='LSA'
          ELSE
            APFLAG=.TRUE.
        ENDIF
C
C Locate rows with correct grating mode, aperture,
C Loop through twice, the first time is just to count the number of rows
C the second is to fill the newly allocated buffers
C
        NFOUND=0
        CONTXT='ERROR reading CCR8 table '//CCR8
        DO 20 ROW=1,NROWS
C
C check grating
C
            CALL UTRGTT(ID,COLIDS(1),1,ROW,GMODE,NULL,ISTAT)
            IF(ISTAT.NE.0)GO TO 999
            IF(GMODE.EQ.GRAT)THEN
C
C check spectral order
C
                CALL UTRGTI(ID,COLIDS(2),1,ROW,SPORD,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(SPORD.EQ.ORDER)THEN
C
C check aperture
C
                    IF(APFLAG)CALL UTRGTT(ID,COLIDS(4),1,ROW,AP,
     *                                  NULL,ISTAT)
                    IF(ISTAT.NE.0)GO TO 999
                    IF(AP.EQ.APER)NFOUND=NFOUND+1
		ENDIF
	    ENDIF
20	CONTINUE
C
C Need at least 1 row of coefficients
C
        IF(NFOUND.EQ.0)THEN
                WRITE(CONTXT,199)GRAT,ORDER,APER
199             FORMAT('ERROR: No rows found in CCR8 for grating ',
     *                                A5,' order',I3,'  ',A3)
                GO TO 999
        ENDIF
C
C allocate memory
C
        CALL UDMGET (NFOUND, TYINT, ROWS, ISTATS(1))
        CALL UDMGET (NFOUND, TYREAL, CARPOS, ISTATS(2))
        IF (ISTATS(1).NE.0.OR.ISTATS(2).NE.0) THEN
            CONTXT='ERROR allocating memory'
            GO TO 999
        ENDIF
C
C fill the buffers
C
        N=0
        DO 10 ROW=1,NROWS 
C
C check grating
C
            CALL UTRGTT(ID,COLIDS(1),1,ROW,GMODE,NULL,ISTAT)
            IF(ISTAT.NE.0)GO TO 999
            IF(GMODE.EQ.GRAT)THEN
C
C check spectral order
C
                CALL UTRGTI(ID,COLIDS(2),1,ROW,SPORD,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(SPORD.EQ.ORDER)THEN
C
C check aperture
C
                    IF(APFLAG)CALL UTRGTT(ID,COLIDS(4),1,ROW,AP,
     *                                  NULL,ISTAT)
                    IF(ISTAT.NE.0)GO TO 999
                    IF(AP.EQ.APER)THEN
			N=N+1
                        MEMI(ROWS+N-1)=ROW
                        CALL UTRGTR(ID,COLIDS(3),1,ROW,
     $				MEMR(CARPOS+N-1),NULL,ISTAT)
                        IF(ISTAT.NE.0)GO TO 999
                     ENDIF
                ENDIF
            ENDIF
10      CONTINUE
C
C sort into ascending order
C
        CALL ZSORTR(NFOUND,MEMR(CARPOS),MEMI(ROWS))
        ISTAT=0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
