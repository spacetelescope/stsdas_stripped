        SUBROUTINE YGPTRN(IMD,FCHNL,NCHNL,XSTEPS,YSTEPS,YBASE,
     *                  YRANGE,OVRSCN,ISTAT)
*
*  Module number: FOS UTILITY
*
*  Module name: YGPTRN
*
*  Keyphrase:
*  ----------
*       Get FOS pattern information
*  Description:
*  ------------
*       This routine gets fos pattern information from file specified
*       by the input image descriptor
*  FORTRAN name:
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       Keyword values FCHNL, NCHNLS, YBASE, YRANGE, YSTEPS, OVERSCAN,
*               NXSTEPS  are read from the file specified by the
*               input image descriptor.
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  SDAS:
*       usmput, uhdgsi
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*    1          sept 87   D. Lindler    Designed and coded
*-------------------------------------------------------------------------------
C
C INPUT PARAMETER
C
C       IMD - INPUT IMAGE DESCRIPTOR VARIABLE (INTEGER)
C
C OUTPUT PARAMETERS
C
C       FCHNL - FIRST CHANNEL (INTEGER)
C       NCHNL - NUMBER OF CHANNELS (INTEGER)
C       XSTEPS - NUMBER OF XSTEPS (INTEGER)
C       YSTEPS - NUMBER OF YSTEPS (INTEGER)
C       YBASE - Y-BASE (INTEGER)
C       YRANGE - Y-RANGE (INTEGER)
C       OVRSCN - OVERSCAN (INTEGER)
C       ISTAT - ERROR STATUS (INTEGER)
C
C------------------------------------------------------------------------------
	IMPLICIT NONE
C
C     INCLUDE FILE FOR THE IRAF77 FORTRAN INTERFACE TO THE IRAF VOS
C
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
C
C     END IRAF77.INC
        INTEGER IMD,FCHNL,NCHNL,XSTEPS,YSTEPS,YBASE,YRANGE,OVRSCN,ISTAT
C
C LOCAL VARIABLES
C
        CHARACTER*10 NAMES(7)
        INTEGER ISTATS(7),ISTATX,I
        CHARACTER*130 MESS
C
C  DATA DECLARATIONS
C
        DATA NAMES/'FCHNL','NCHNLS','NXSTEPS','YSTEPS','YBASE',
     *                  'YRANGE','OVERSCAN'/
C
C GET VALUES
C
        CALL UHDGSI(IMD,'FCHNL',FCHNL,ISTATS(1))
        CALL UHDGSI(IMD,'NCHNLS',NCHNL,ISTATS(2))
        CALL UHDGSI(IMD,'NXSTEPS',XSTEPS,ISTATS(3))
        CALL UHDGSI(IMD,'YSTEPS',YSTEPS,ISTATS(4))
        CALL UHDGSI(IMD,'YBASE',YBASE,ISTATS(5))
        CALL UHDGSI(IMD,'YRANGE',YRANGE,ISTATS(6))
        CALL UHDGSI(IMD,'OVERSCAN',OVRSCN,ISTATS(7))
C
C CHECK FOR ERRORS
C
        ISTAT=0
        DO 10 I=1,7
                IF(ISTATS(I).NE.0)THEN
                        WRITE(MESS,99)NAMES(I)
99                      FORMAT('Error getting file keyword ',A10)
                        CALL UMSPUT(MESS,STDERR+STDOUT+USRLOG,0,ISTATX)
                        ISTAT=1
                ENDIF
10      CONTINUE
        RETURN
        END
