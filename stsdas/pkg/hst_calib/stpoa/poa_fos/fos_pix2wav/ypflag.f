C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YPFLAG(GRNDMD,PFLAGS,ISTAT)
*
*  Module number:
*
*  Module name: YPFLAG
*
*  Keyphrase:
*  ----------
*       Get FOS processing flags
*
*  Description:
*  ------------
*       This routine reads the FOS processing flags for the input .d0h
*       header and places them into a vector as boolean values
*
*  FORTRAN name: ypflag.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.d0h          I       FOS science data file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*       uhdgst
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jul 89  D. Lindler      Designed and coded
*	1.1	May 90	S. Hulbert	Added GIMP correction 
*	1.2	Aug 90	S. Hulbert	Added scaling of reference background
*	1.3	Apr 93	H. Bushouse	Changed declaration of PFLAG(1) to (*)
*	1.4	Mar 94	H. Bushouse	Added SCT_CORR to PFLAGS and NAMES
*	2	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*					(change PFLAGS to CHAR datatype).
*	3	Oct 94	H. Bushouse	Mods for new aper, focus, flux, and
*					time corrections.
*       4       Feb98   M. De La Pena   PFLAGS(10) - PFLAGS(12) cannot be set
*                                       to PERFORM for spectropolarimetry data.
*-------------------------------------------------------------------------------
*
* Inputs:
*       grndmd - ground mode
*
* Outputs:
*       pflags - boolean vector of flags
*            position           step
*               1       conversion to count rates
*		2	GIMP correction
*               3       paired pulse
*               4       background subtraction
*               5       scattered light subtraction
*               6       flat fielding
*               7       sky subtraction
*               8       wavelengths
*               9       conversion to flux (old method)
*		10	aperture throughput and focus corrections
*		11	conversion to flux (new method)
*		12	sensitivity degradation (time) correction
*               13      error propagation
*               14      mode dependent reductions
*               15      scale reference background
*
*       istat - error status
*
        CHARACTER*18 GRNDMD
        CHARACTER*8 PFLAGS(*)
        INTEGER ISTAT
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Common block containing input/output file descriptions
C
C       IDS - file id numbers
C       GCOUNT - group counts
C       NAXIS - naxis
C       NAXIS1 - first dimensions
C       NAXIS2 - second dimensions
C       FILL - Fill values
C
        INTEGER IDS(30),NAXIS(30),NAXIS1(30),NAXIS2(30),GCOUNT(30)
        REAL FILL(30)
        COMMON /IOCOM/IDS,GCOUNT,NAXIS,NAXIS1,NAXIS2,FILL
C
C Local variables
C
        INTEGER I
        CHARACTER*7 VAL
        CHARACTER*80 CONTXT
        CHARACTER*8 NAMES(15)
        DATA NAMES/'CNT_CORR','OFF_CORR','PPC_CORR','BAC_CORR',
     *		   'SCT_CORR','FLT_CORR','SKY_CORR','WAV_CORR',
     *		   'FLX_CORR','APR_CORR','AIS_CORR','TIM_CORR',
     *		   'ERR_CORR','MOD_CORR','GMF_CORR'/
C
C--------------------------------------------------------------------------
        DO 100 I=1,15
           PFLAGS(I)='OMIT'
           CALL UHDGST(IDS(1),NAMES(I),VAL,ISTAT)
C
           IF(ISTAT.NE.0)THEN
cc modified to circumvent AISHFILE (MR/FK 22.3.2000)
cc           IF(ISTAT.NE.0.OR.(I.GE.10.AND.I.LE.12)) THEN
cc
		IF(I.EQ.5)THEN
		   CONTXT='WARNING: SCT_CORR keyword not '//
     *			  'found in d0h file;'
		   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                   CONTXT='         Scattered light correction will '//
     *                    'be skipped'
		   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		   PFLAGS(5)='OMIT'
		ELSE IF(I.GE.10 .AND. I.LE.12)THEN
		   PFLAGS(I)='OMIT'
		ELSE
		   CONTXT='ERROR: getting d0h processing flag '//
     *			   NAMES(I)
		   GO TO 999
		ENDIF
	   ELSE
		IF(VAL.EQ.'PERFORM')PFLAGS(I)='PERFORM'
           ENDIF
100     CONTINUE
cc modified to circumvent FLATFIELDING (MR/FK 22.3.2000)
cc	PFLAGS(6) = 'OMIT'
cc

C
C	check that the two background-related switches are consistent
C
	IF (PFLAGS(4).EQ.'OMIT') PFLAGS(15)='OMIT'
C
C	check that the various flux calibration switches are consistent:
C	If any of the new (10, 11, 12) switches is on, turn off the
C	old abscal (switch 9).
C       If all three (10, 11, 12) are not all on, issue warning that
C       flux calibration may be incomplete.
C
C       Modified the IF blocks.  If any of PFLAGS(10) - PFLAGS(12) are PERFORM,
C       but the data is spectropolarimetry, these flags are set to SKIPPED, and
C       a warning is issued.
C
	IF (PFLAGS(10).EQ.'PERFORM' .OR. PFLAGS(11).EQ.'PERFORM' .OR.
     *       PFLAGS(12).EQ.'PERFORM') THEN
           IF ((PFLAGS(9).EQ.'PERFORM') .AND. 
     *         (GRNDMD.NE.'SPECTROPOLARIMETRY'))PFLAGS(9)='SKIPPED'
           IF ((PFLAGS(9).EQ.'PERFORM') .AND. 
     *         (GRNDMD.EQ.'SPECTROPOLARIMETRY')) THEN
              WRITE(CONTXT,110)
 110          FORMAT ('WARNING: APR_CORR, AIS_CORR, and',
     *             ' TIM_CORR are not applicable to')
              CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              WRITE(CONTXT,113)
 113          FORMAT (' spectropolarimetry data.')
              CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              PFLAGS(10) = 'SKIPPED'
              PFLAGS(11) = 'SKIPPED'
              PFLAGS(12) = 'SKIPPED'
           ENDIF
        ENDIF
C
	IF (PFLAGS(10).EQ.'PERFORM' .OR. PFLAGS(11).EQ.'PERFORM' .OR.
     *       PFLAGS(12).EQ.'PERFORM') THEN
           IF (PFLAGS(10).NE.'PERFORM' .OR. 
     *          PFLAGS(11).NE.'PERFORM' .OR.
     *          PFLAGS(12).NE.'PERFORM')THEN
              WRITE(CONTXT,111)
 111          FORMAT ('WARNING: APR_CORR, AIS_CORR, and',
     *             ' TIM_CORR are not all on together.')
              CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              WRITE(CONTXT,112)
 112          FORMAT ('         Flux calibration may not',
     *             ' be fully applied.')
              CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           END IF
        END IF
C
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
