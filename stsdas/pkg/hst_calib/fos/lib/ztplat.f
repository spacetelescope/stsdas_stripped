        SUBROUTINE ZTPLAT(TEMPLT,NAME,DATA,N,IDOUT,ISTAT)
*
*  Module number: HRS UTILITY
*
*  Module name: ztplat
*
*  Keyphrase:
*  ----------
*       create data file from template
*  Description:
*  ------------
*       This routine creates an output data file using an input
*       template header.  The input template is modified to
*       change the value of naxis1 to equal the number of values
*       in the data vector.  The output file is left open to
*       allow changes to the group or header parameters
*
*  FORTRAN name: ztplat.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       templt                  I       Template file header
*       name                    O       Name of the output file
*
*  Subroutines Called:
*  -------------------*
*  SDAS:
*       umsput, uimopn, uipl1d, uhdpsi, uimopc, uimclo
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Dec 87  D. Lindler      Designed and coded
*-------------------------------------------------------------------------------
C
C INPUT PARAMETERS
C
C       TEMPLT - name of the template file
C       NAME - name of the output data file
C       DATA - Data vector (REAL*8)
C       N - Number of points in DATA (integer)
C
C OUTPUT PARAMETERS
C
C       IDOUT - descriptor variable for NAME (integer)
C       ISTAT - error status (integer)
C
C------------------------------------------------------------------------------
	IMPLICIT NONE
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
      INTEGER   RDWRIT
      PARAMETER (RDWRIT = 2)
      INTEGER   WRONLY
      PARAMETER (WRONLY = 3)
      INTEGER   APPEND
      PARAMETER (APPEND = 4)
      INTEGER   NEWFIL
      PARAMETER (NEWFIL = 5)
C
C     CODES FOR DATA TYPES
C
      INTEGER   TYBOOL
      PARAMETER (TYBOOL = 1)
      INTEGER   TYCHAR
      PARAMETER (TYCHAR = 2)
      INTEGER   TYINT
      PARAMETER (TYINT = 4)
      INTEGER   TYREAL
      PARAMETER (TYREAL = 6)
      INTEGER   TYDOUB
      PARAMETER (TYDOUB = 7)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
      INTEGER USRLOG
      PARAMETER (USRLOG = 4)
      INTEGER SYSLOG
      PARAMETER (SYSLOG = 8)
C
C     UHDAS HEADER PARM TYPES -- CB, DAO, 5-SEP-87
C
      INTEGER GENHDR
      PARAMETER (GENHDR = 0)
      INTEGER IMSPEC
      PARAMETER (IMSPEC = 1)
C
C     THIS SECTION IS FOR PARAMETERS RELEVANT TO TABLE I/O.
C
C     THESE MAY BE SET BY UTPPTI AND/OR READ BY UTPGTI:
C
C                                       LENGTH OF ROW (UNIT = SIZE OF REAL)
      INTEGER   TBRLEN
      PARAMETER (TBRLEN = 1)
C                                       INCREASE ROW LENGTH
      INTEGER   TBIRLN
      PARAMETER (TBIRLN = 2)
C                                       NUMBER OF ROWS TO ALLOCATE
      INTEGER   TBALLR
      PARAMETER (TBALLR = 3)
C                                       INCREASE ALLOC NUM OF ROWS
      INTEGER   TBIALR
      PARAMETER (TBIALR = 4)
C                                       WHICH TYPE OF TABLE? (ROW OR COLUMN)
      INTEGER   TBWTYP
      PARAMETER (TBWTYP = 5)
C                                       MAXIMUM NUMBER OF USER PARAMETERS
      INTEGER   TBMXPR
      PARAMETER (TBMXPR = 6)
C                                       MAXIMUM NUMBER OF COLUMNS
      INTEGER   TBMXCL
      PARAMETER (TBMXCL = 7)
C                                       TYPE = ROW-ORDERED TABLE
      INTEGER   TBTYPR
      PARAMETER (TBTYPR = 11)
C                                       TYPE = COLUMN-ORDERED TABLE
      INTEGER   TBTYPC
      PARAMETER (TBTYPC = 12)
C
C     THESE MAY BE READ BY UTPGTI BUT MAY NOT BE SET:
C
C                                       NUMBER OF ROWS WRITTEN TO
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
C                                       NUMBER OF COLUMNS DEFINED
      INTEGER   TBNCOL
      PARAMETER (TBNCOL = 22)
C                                       AMOUNT OF ROW USED (UNIT=SIZE OF REAL)
      INTEGER   TBRUSD
      PARAMETER (TBRUSD = 23)
C                                       NUMBER OF USER PARAMETERS
      INTEGER   TBNPAR
      PARAMETER (TBNPAR = 24)
C     END IRAF77.INC
        CHARACTER*64    NAME,TEMPLT
        INTEGER         N,IDOUT,ISTAT
        DOUBLE PRECISION DATA
C
C LOCAL VARIABLES
C
        INTEGER         IDIN,STATUS
        CHARACTER*130   CONTXT
C
C------------------------------------------------------------------------------
C
C OPEN INPUT TEMPLATE FILE
C
        CALL UIMOPN(TEMPLT,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='Error reading template file '//TEMPLT
                GO TO 999
        ENDIF
C
C OPEN OUTPUT FILE USING TEMPLATE FILE
C
        CALL UUIMCP(NAME,TYREAL,1,N,IDIN,IDOUT,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='Error opening output file '//NAME//
     *                 ' From template'
                GO TO 999
        ENDIF
C
C WRITE DATA TO THE OUTPUT FILE
C
        CALL UIPL1D(IDOUT,DATA,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='Error writing to output file '//NAME
                GO TO 999
        ENDIF
C
C CLOSE INPUT TEMPATE AND RETURN
C
        CALL UIMCLO(IDIN,ISTAT)
C                                    --->IGNORE THE NON-ZERO STATUS
        ISTAT=0
        GO TO 1000
999     CALL UMSPUT(CONTXT,STDOUT+USRLOG+STDERR,0,STATUS)
1000    RETURN
        END
