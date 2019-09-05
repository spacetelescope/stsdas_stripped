        SUBROUTINE ZCLREL(ID,ISTAT)
*
*  Module number:
*
*  Module name: zclrel
*
*  Keyphrase:
*  ----------
*       Get reference relation table names
*  Description:
*  ------------
*       This routine gets the reference table names from
*	the header of the .d0h file.  The name for relation
*	<rel> is read from keyword value hrs<rel). For example
*	the table name for the ccr1 file would be obtained from
*	keyword CCR1.
*
*  FORTRAN name: zclrel.for
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       none
*  SDAS:
*       uhdgst, ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jan 88  D. Lindler      Designed and coded
*       2       Feb94   J. Eisenhamer   Added CCRC,CCRD
*       2.1     Oct 96  M. De La Pena   Added FBMD,CCRE
*       2.2     Nov 96  M. De La Pena   Added SAAFIL
*-------------------------------------------------------------------------------
*
* INPUT PARAMETERS:
*	ID - id of unit to read header keyword values from
*
* OUTPUT parameters:
*	ISTAT - error status
        INTEGER ISTAT,ID
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C                       HRSREF
C  Common block containing reference file names and table relation
C  names
C
        CHARACTER*64 CCR1,CCR2,CCR3,CCR4,CCR5,CCR6,CCR7,CCR8,CCR9,
     *                  CCRA,CCRB,CCG2,DIOFIL,PHCFIL,VIGFIL,ABSFIL,
     *                  NETFIL,DQIFIL,CCRC,CCRD,CCRE,SAAFIL
        COMMON /HRSREF/ CCR1,CCR2,CCR3,CCR4,CCR5,CCR6,CCR7,CCR8,CCR9,
     *                  CCRA,CCRB,CCG2,DIOFIL,PHCFIL,VIGFIL,ABSFIL,
     *                  NETFIL,DQIFIL,CCRC,CCRD,CCRE,SAAFIL
C
C
C                       /HRSFLG/
C Common block containing processing flags
C
C Initial values = 'PERFORM' or 'OMIT'
C final values = 'PERFORMED' or 'OMITTED'
C
        CHARACTER*12 FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *                  FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *                  FPLY,FGWC,FBMD
        COMMON /HRSFLG/FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *                  FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *                  FPLY,FGWC,FBMD
C
C			/HRSMOD/
C Common block containing observing mode parameters
C
C   GRAT - grating mode
C   DET - detector
C   SCLAMP - spectral calibration lamp
C   CARPOS - carrousel position
C   OBSMOD - observation mode (DIR, ACC, TAR)
C   APER - aperture (LSA, SSA, SC1, SC2)
C
        CHARACTER*3 OBSMOD,APER
        CHARACTER*5 GRAT
        INTEGER DET,SCLAMP,CARPOS
        COMMON /HRSMOD/ DET,SCLAMP,CARPOS
        COMMON /HRSMD1/ GRAT,OBSMOD,APER
C
C LOCAL VARIABLES
C
        INTEGER NFILES
        PARAMETER(NFILES=15)
        INTEGER ISTATS(NFILES),I
        CHARACTER*80 CONTXT
        CHARACTER*4 NAME(NFILES)
        DATA NAME/'CCR1','CCR2','CCR3','CCR4','CCR5','CCR6',
     &       'CCR7','CCR8','CCR9','CCRA','CCG2','CCRB','CCRC',
     &       'CCRD','CCRE'/
C--------------------------------------------------------------------------
C
C READ TABLE NAMES
C
        CALL UHDGST(ID,NAME(1),CCR1,ISTATS(1))
        CALL UHDGST(ID,NAME(2),CCR2,ISTATS(2))
        CALL UHDGST(ID,NAME(3),CCR3,ISTATS(3))
        CALL UHDGST(ID,NAME(4),CCR4,ISTATS(4))
        CALL UHDGST(ID,NAME(5),CCR5,ISTATS(5))
        CALL UHDGST(ID,NAME(6),CCR6,ISTATS(6))
        CALL UHDGST(ID,NAME(7),CCR7,ISTATS(7))
        CALL UHDGST(ID,NAME(8),CCR8,ISTATS(8))
        CALL UHDGST(ID,NAME(9),CCR9,ISTATS(9))
        CALL UHDGST(ID,NAME(10),CCRA,ISTATS(10))
        CALL UHDGST(ID,NAME(11),CCG2,ISTATS(11))
        CALL UHDGST(ID,NAME(12),CCRB,ISTATS(12))
        CALL UHDGST(ID,NAME(13),CCRC,ISTATS(13))
        CALL UHDGST(ID,NAME(14),CCRD,ISTATS(14))
        CALL UHDGST(ID,NAME(15),CCRE,ISTATS(15))
C
C Check the switches to see if the tables are actually needed.
C
        IF(FMAP.NE.'PERFORM')THEN
           ISTATS(1)=0
           ISTATS(2)=0
        ENDIF
        IF(FADC.NE.'PERFORM')THEN
           ISTATS(5)=0
           ISTATS(6)=0
           ISTATS(7)=0
           ISTATS(13)=0
        ENDIF
        IF(FIAC.NE.'PERFORM')ISTATS(8)=0
        IF(FECH.NE.'PERFORM')THEN
           ISTATS(9)=0
           ISTATS(10)=0
        ENDIF
        IF(FBCK.NE.'PERFORM')ISTATS(12)=0
        IF(FBMD.NE.'PERFORM')ISTATS(15)=0
        IF(FPPC.NE.'PERFORM')ISTATS(11)=0
        IF(FGWC.NE.'PERFORM')ISTATS(13)=0
        IF(FMAP.NE.'PERFORM'.OR.FDQI.NE.'PERFORM')ISTATS(14)=0
C
C Missing the CCRD keyword is not an error.
C
        IF(ISTATS(14).NE.0)THEN
           ISTATS(14)=0
           CCRD=' '
        ENDIF
C
C Error if tables are missing.
C
        DO 10 I=1,NFILES
                IF(ISTATS(I).NE.0)THEN
                   WRITE(CONTXT,99)NAME(I)
 99                FORMAT('Error reading keyword value ',A,
     &                  ' from .d0h file')
                    GO TO 999
                ENDIF
10      CONTINUE
C
        ISTAT=0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
