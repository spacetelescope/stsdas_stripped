        SUBROUTINE YRDPCP(NAME,MAXLEN,UCORR,QCORR,PHCORR,PEDGRE,
     *                    DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRDPCP
*
*  Keyphrase:
*  ----------
*       read the post-Costar polarimetry correction image
*  Description:
*  ------------
*       This routine reads the post-COSTAR polarimetry correction
*       file and checks its consistency.  It is called only for post-COSTAR 
*       observations as denoted by KYDEPLOY = T.  The Q, U, and phase 
*       corrections are given for the specific grating/waveplate/aperture 
*       combination.  If present in the header of the calibration file,
*       DETECTOR, FGWA_ID, POLAR_ID, and APER_ID must match those of the 
*       observation.
*
*  FORTRAN name: YRDPCP.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                        I/O     Description / Comments
*       NAME                    I      post-Costar polarimetry corrections file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*       uimopn, uhdgst, uhdgsi, uimgid, uuopgr
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Mar 97  M. De La Pena   Original Implemenation
*-------------------------------------------------------------------------------
*
* Input parameters
*       NAME   - image name
*       MAXLEN - correction factor arrays are of size MAXLEN x 3
*        (i.e., there are corrections for Pass 1, Pass 2, and combined.)
*
* Output parameters
*       QCORR  - Stokes Q correction factors
*       UCORR  - Stokes U correction factors
*       PHCORR - Phase angle correction factors
*	PEDGRE - PCPFIL PEDIGREE keyword
*	DESCRP - PCPFIL DESCRIP keyword
*       ISTAT  - error status
*
*-----------------------------------------------------------------------------
*
* The PCPHFILE contains nine groups of length 2064 as follows:
*
* Group 1 - Stokes Q correction for Pass 1
* Group 2 - Stokes Q correction for Pass 2
* Group 3 - Stokes Q correction for combined
* Group 4 - Stokes U correction for Pass 1
* Group 5 - Stokes U correction for Pass 2
* Group 6 - Stokes U correction for combined
* Group 7 - phase correction for Pass 1
* Group 8 - phase correction for Pass 2
* Group 9 - phase correction for combined
*
*-----------------------------------------------------------------------------
C
C Passed parameters
C
        INTEGER      MAXLEN,ISTAT
        CHARACTER*68 PEDGRE,DESCRP
        CHARACTER*64 NAME
        DOUBLE PRECISION UCORR(MAXLEN,3),QCORR(MAXLEN,3),
     *                   PHCORR(MAXLEN,3)
C
C FILE I/O ACCESS MODES
C
        INTEGER   RDONLY
        PARAMETER (RDONLY = 1)
C
C UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
        INTEGER STDOUT
        PARAMETER (STDOUT = 1)
        INTEGER STDERR
        PARAMETER (STDERR = 2)
C
C Common block containing confiquration parameters
C
        CHARACTER*5 DET
        CHARACTER*3 FGWAID,APERID,YTYPE(3)
        CHARACTER*1 POLID
        LOGICAL     KYDPLY
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
C
C Local variables
C
        INTEGER      I,IDIN,DIMEN(8),NAXIS,DTYPE,YMIN,YMAX
        REAL         DMIN,DMAX
C                        --->file I/O parameters
        CHARACTER*80 CONTXT
        CHARACTER*5  CDET
        CHARACTER*3  CFGWA,CAPER,CPOL
C
C------------------------------------------------------------------------------
C
C Open file
C
        CALL UIMOPN(NAME,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR opening post-Costar polarimetry file '//
     *              NAME
            GO TO 999
        ENDIF
C
C Get file size parameters and verify
C
        CALL UIMGID(IDIN,DTYPE,NAXIS,DIMEN,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading post-Costar polarimetry file '//
     *              NAME
            GO TO 998
        ENDIF
        IF(NAXIS.NE.1) THEN
            CONTXT='ERROR: post-Costar polarimetry file has '//
     *             'invalid dimensions '//NAME
            GO TO 998
        ENDIF
        IF(DIMEN(1).GT.MAXLEN)THEN
            CONTXT='ERROR: post-Costar polarimetry file too large'
            GO TO 998
        ENDIF
C
C Get PEDIGREE and DESCRIP header keywords
C
        CALL UHDGST(IDIN,'PEDIGREE',PEDGRE,ISTAT)
        CALL UHDGST(IDIN,'DESCRIP', DESCRP,ISTAT)
C
C Get detector from file and verify
C
        CALL UHDGST(IDIN,'DETECTOR',CDET,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='WARNING: DETECTOR keyword missing from the '//
     *             'post-Costar polarimetry file'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ELSE
            IF((DET.NE.CDET).AND.(CDET.NE.'ANY'))THEN
                CONTXT='ERROR: DETECTOR keyword value in the '//
     *                 'post-Costar polarimetry '
                CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                CONTXT='       calibration file does not match'//
     *                 ' the observation.'
                GO TO 998
            ENDIF
        ENDIF
C
C Get fgwa_id from file and verify
C
        CALL UHDGST(IDIN,'FGWA_ID',CFGWA,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='WARNING: FGWA_ID keyword missing from the '//
     *             'post-Costar polarimetry file'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ELSE
            IF((FGWAID.NE.CFGWA).AND.(CFGWA.NE.'ANY'))THEN
                CONTXT='ERROR: FGWA_ID keyword value in the '//
     *                 'post-Costar polarimetry'
                CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                CONTXT='       calibration file does not match'//
     *                 ' the observation.'
                GO TO 998
            ENDIF
        ENDIF
C
C Get aper_id from file and verify
C
        CALL UHDGST(IDIN,'APER_ID',CAPER,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='WARNING: APER_ID keyword missing from the '//
     *             'post-Costar polarimetry file'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ELSE
            IF((APERID.NE.CAPER).AND.(CAPER.NE.'ANY'))THEN
                CONTXT='ERROR: APER_ID keyword value in the '//
     *                 'post-Costar polarimetry'
                CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                CONTXT='       calibration file does not match'//
     *                 ' the observation.'
                GO TO 998
            ENDIF
        ENDIF
C
C Get polar_id from file and verify
C
        CALL UHDGST(IDIN,'POLAR_ID',CPOL,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='WARNING: POLAR_ID keyword missing from the '//
     *             'post-Costar polarimetry'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ELSE
            IF((POLID.NE.CPOL).AND.(CPOL.NE.'N'))THEN
                CONTXT='ERROR: POLAR_ID keyword value in the '//
     *                 'post-Costar polarimetry'
                CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                CONTXT='       calibration file does not match'//
     *                 ' the observation.'
                GO TO 998
            ENDIF
        ENDIF
C
C Read post-Costar polarimetry correction values
C Read Q corrections
C
        YMIN = 1
        YMAX = 3
        DO 10 I = YMIN, YMAX
           CALL UUOPGR(IDIN,I,DMIN,DMAX,0,ISTAT)
           IF(ISTAT.NE.0)THEN
               CONTXT='ERROR moving to next group of the '//
     *                'calibration file = ' // NAME
               GO TO 998
           ENDIF
           CALL UIGL1D(IDIN,QCORR(1,I),ISTAT)
           IF(ISTAT.NE.0)THEN
               CONTXT='ERROR reading post-Costar polarimetry Q '//
     *                'corrections from'
               CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
               CONTXT='      calibration file = ' // NAME
               GO TO 998
           ENDIF
10      CONTINUE
C
C Read U corrections 
C
        YMIN = YMIN + 3
        YMAX = YMAX + 3
        DO 20 I = YMIN, YMAX
           CALL UUOPGR(IDIN,I,DMIN,DMAX,0,ISTAT)
           IF(ISTAT.NE.0)THEN
               CONTXT='ERROR moving to next group of the '//
     *                'calibration file = ' // NAME
               GO TO 998
           ENDIF
           CALL UIGL1D(IDIN,UCORR(1,I-3),ISTAT)
           IF(ISTAT.NE.0)THEN
               CONTXT='ERROR reading post-Costar polarimetry U '//
     *                'corrections from'
               CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
               CONTXT='      calibration file = ' // NAME
               GO TO 998
           ENDIF
20      CONTINUE 
C
C Read the phase corrections
C
        YMIN = YMIN + 3
        YMAX = YMAX + 3
        DO 30 I = YMIN, YMAX
           CALL UUOPGR(IDIN,I,DMIN,DMAX,0,ISTAT)
           IF(ISTAT.NE.0)THEN
               CONTXT='ERROR moving to next group of the '//
     *                'calibration file = ' // NAME
               GO TO 998
           ENDIF
           CALL UIGL1D(IDIN,PHCORR(1,I-6),ISTAT)
           IF(ISTAT.NE.0)THEN
               CONTXT='ERROR reading post-Costar polarimetry phase'//
     *                ' corrections'
               CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
               CONTXT='      from calibration file = ' // NAME
               GO TO 998
           ENDIF
30      CONTINUE 
C
        CALL UIMCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
C
C ERROR SECTION
C
998     CALL UIMCLO(IDIN,ISTAT)
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
