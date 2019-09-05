        SUBROUTINE ZCLINQ(ISTAT)
*
*  Module number:
*
*  Module name: ZCLINQ
*
*  Keyphrase:
*  ----------
*       Check that the reference relation tables and files exist
*  Description:
*  ------------
*       This routine inquires about the existence of the reference    
*       relation tables and reference files that have been previously   
*       read from the .d0h header. Only those files needed by the
*       calibration switches set to 'PERFORM' are checked.
*
*       If any of the files is not found then the routine returns an 
*       error status.
*
*  FORTRAN name: ZCLINQ.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*  DIOHFILE                     Input   diode response header file
*  PHCHFILE                     Input   photocathode response header file
*  VIGHFILE                     Input   vignetting header file
*  ABSHFILE                     Input   absolute sensitivity file
*  NETHFILE                     Input   wavelengths for ABSHFILE
*  DQIHFILE                     Input   data quality initialization file
*  SAAHFILE                     Input   SAA contour file
*  CCR1                         Input   photocathode line mapping parameters
*  CCR2                         Input   photocathode sample parameters
*  CCR3                         Input   detector parameters
*  CCR4                         Input   wavelength ranges
*  CCR5                         Input   spectral order constants
*  CCR6                         Input   dispersion constants
*  CCR7                         Input   thermal constants
*  CCR8                         Input   incidence angle coefficients
*  CCR9                         Input   Echelle ripple constants
*  CCRA                         Input   More echelle ripple constants
*  CCRB                         Input   scattered light correction factors
*  CCRC                         Input   global coefficients for wavelengths
*  CCRD                         Input   photocathode blemish table
*  CCRE                         Input   background count rate model
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       none
*  SDAS:
*       ZCLACC
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Sep 89      S. Hulbert      Designed and coded
*       2       Feb 94      J. Eisenhamer   Added CCRC,CCRD
*       2.1     Oct 96      M. De La Pena   Added CCRE
*       2.2     Nov 96      M. De La Pena   Added SAAFIL
*-------------------------------------------------------------------------------
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
C LOCAL VARIABLES
C
        INTEGER NFILES
        PARAMETER(NFILES=22)
      INTEGER I, ISTAT, ISTATS(NFILES)
      CHARACTER*5 REFTYPE
      CHARACTER*8 LABEL(NFILES)
      DATA LABEL/ 'CCR1','CCR2','CCR3','CCR4','CCR5','CCR6','CCR7',
     $            'CCR8','CCR9','CCRA','CCG2','DIOHFIL',
     $            'PHCHFIL','VIGHFIL','ABSHFIL','NETHFIL','DQIHFIL',
     $            'CCRB','CCRC','CCRD','CCRE', 'SAAHFIL'/
C--------------------------------------------------------------------------
      DO 10 I=1,NFILES
         ISTATS(I)= 0
   10 CONTINUE
C
C     Look for the TABLES that will be needed according
C     to the calibration switches that are set
C
C     REFTYPE => 'TABLE' for tables
C             => 'FILE ' for files
C
      REFTYPE = 'TABLE'
C
      IF (FMAP .EQ. 'PERFORM') THEN
         CALL ZCLACC(CCR1,LABEL(1),REFTYPE,ISTATS(1))
         CALL ZCLACC(CCR2,LABEL(2),REFTYPE,ISTATS(2))
      ENDIF
C
      CALL ZCLACC(CCR3,LABEL(3),REFTYPE,ISTATS(3))
      CALL ZCLACC(CCR4,LABEL(4),REFTYPE,ISTATS(4))
C
      IF (FADC .EQ. 'PERFORM') THEN
         CALL ZCLACC(CCR5,LABEL(5),REFTYPE,ISTATS(5))
         IF(FGWC .EQ. 'PERFORM') THEN
            CALL ZCLACC(CCRC,LABEL(19),REFTYPE,ISTATS(19))
            ISTATS(6)=0
         ELSE
            CALL ZCLACC(CCR6,LABEL(6),REFTYPE,ISTATS(6))
            ISTATS(19)=0
         ENDIF
         CALL ZCLACC(CCR7,LABEL(7),REFTYPE,ISTATS(7))
      ENDIF
C
      IF (FIAC .EQ. 'PERFORM') THEN
         CALL ZCLACC(CCR8,LABEL(8),REFTYPE,ISTATS(8))
      ENDIF
C
      IF (FECH .EQ. 'PERFORM') THEN
         CALL ZCLACC(CCR9,LABEL(9),REFTYPE,ISTATS(9))
         CALL ZCLACC(CCRA,LABEL(10),REFTYPE,ISTATS(10))
      ENDIF
C
      IF (FPPC .EQ. 'PERFORM') THEN
         CALL ZCLACC(CCG2,LABEL(11),REFTYPE,ISTATS(11))
      ENDIF
C
      IF (FBCK .EQ. 'PERFORM') THEN
         CALL ZCLACC(CCRB,LABEL(18),REFTYPE,ISTATS(18))
      ENDIF
C
      IF (FBMD .EQ. 'PERFORM') THEN
         CALL ZCLACC(CCRE,LABEL(21),REFTYPE,ISTATS(21))
      ENDIF
C
C     Look for the FILES that will be needed according
C     to the calibration switches that are set
C
      REFTYPE = 'FILE '
C
      IF (FDIO .EQ. 'PERFORM') THEN
         CALL ZCLACC(DIOFIL,LABEL(12),REFTYPE,ISTATS(12))
      ENDIF
C
      IF (FPHC .EQ. 'PERFORM') THEN
         CALL ZCLACC(PHCFIL,LABEL(13),REFTYPE,ISTATS(13))
      ENDIF
C
      IF (FVIG .EQ. 'PERFORM') THEN
         CALL ZCLACC(VIGFIL,LABEL(14),REFTYPE,ISTATS(14))
      ENDIF
C
      IF (FFLX .EQ. 'PERFORM') THEN
         CALL ZCLACC(ABSFIL,LABEL(15),REFTYPE,ISTATS(15))
         CALL ZCLACC(NETFIL,LABEL(16),REFTYPE,ISTATS(16))
      ENDIF
C
      IF (FDQI .EQ. 'PERFORM') THEN
         CALL ZCLACC(DQIFIL,LABEL(17),REFTYPE,ISTATS(17))
      ENDIF
C
      IF (FBMD .EQ. 'PERFORM') THEN
         CALL ZCLACC(SAAFIL,LABEL(22),REFTYPE,ISTATS(22))
      ENDIF
C
C     NOTE THAT THE CCRD TABLE IS NOT CHECKED
C
      ISTATS(20) = 0
C
      DO 13 I=1,NFILES
         IF (ISTATS(I) .NE. 0) GO TO 999
   13 CONTINUE
C
      ISTAT = 0
      GOTO 1000
C
  999 ISTAT = 1
C
 1000 RETURN
      END
