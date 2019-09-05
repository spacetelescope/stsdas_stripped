C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YGMODE(IMDSCR,DET,FGWA,APER,APERPS,POLAR,PASSDR,
     *                  ISTAT)
*
*  Module number: FOS UTILITY
*
*  Module name:YGMODE
*
*  Keyphrase:
*  ----------
*       get observing parameters
*  Description:
*  ------------
*       This routine will get detector, grating mode, aperture
*       and polarizer id from header of file with specified image
*       descriptor.
*  FORTRAN name: ygmode.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*    From input image with descriptor IMDSCR
*       aper_id                 I       aperture id
*       fgwa_id                 I       filter garting wheel id
*       detector                I       detector
*       polar_id                I       polarizer id
*       pass_dir                I       pass direction
*       aper_pos                I       aperture position
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  SDAS:
*       umsput, uhdgst
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Sept 87   D. Lindler    Designed and coded
*-------------------------------------------------------------------------------
C
C INPUT PARAMETERS
C
C       IMDSCR - IMAGE DESCIPTOR (INTEGER)
C
C OUTPUT PARAMETERS
C
C       DET - DETECTOR (CHAR*5)
C       FGWA - FGWA_ID (CHAR*3)
C       APER - APERTURE ID (CHAR*3)
C       APERPS - APERTURE POSITION (CHAR*6)
C       POLAR - POLARIZER ID (CHAR*1)
C       PASSDR - PASS DIRECTION (INTEGER)
C       ISTAT - ERROR STATUS
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
        INTEGER IMDSCR,ISTAT,PASSDR
        CHARACTER*6 APERPS
        CHARACTER*3 FGWA
        CHARACTER*3 APER
        CHARACTER*1 POLAR
        CHARACTER*5 DET
C
C LOCAL VARIABLES
C
        INTEGER ISTATS(6),ISTATX,I
        CHARACTER*10 NAMES(6)
        CHARACTER*130 MESS
C
C  DATA DECLARATIONS
C
        DATA NAMES/'DETECTOR','FGWA_ID','APER_ID','POLAR_ID','APER_POS',
     *             'PASS_DIR'/
C
C READ PARAMETERS
C
        CALL UHDGST(IMDSCR,'DETECTOR',DET,ISTATS(1))
        CALL UHDGST(IMDSCR,'FGWA_ID',FGWA,ISTATS(2))
        CALL UHDGST(IMDSCR,'APER_ID',APER,ISTATS(3))
        CALL UHDGST(IMDSCR,'POLAR_ID',POLAR,ISTATS(4))
        CALL UHDGST(IMDSCR,'APER_POS',APERPS,ISTATS(5))
        CALL UHDGSI(IMDSCR,'PASS_DIR',PASSDR,ISTATS(6))
        ISTAT=0
        DO 10 I=1,6
                IF(ISTATS(I).NE.0)THEN
                        WRITE(MESS,99)NAMES(I)
99                      FORMAT('Error reading header value ',A10)
                        CALL UMSPUT(MESS,STDOUT+USRLOG,0,ISTATX)
                        ISTAT=1
                ENDIF
10      CONTINUE
        RETURN
        END
