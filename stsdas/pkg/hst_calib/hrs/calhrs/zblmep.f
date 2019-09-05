      SUBROUTINE ZBLMEP(ID,COLID,DET,LINE,SAMPLE,DELTAS,
     &     SLINE,BSAMP,BLEM,NS,ISTAT)
*
* Module number:
*
* Module name: ZBLMEP
*
* Keyphrase:
* ---------
*     Determine where blemishes may affect data.
*
* Description:
* -----------
*     Fill arrays describing where photocathode blemishes exist
*     in the science data arrays.
*
* FORTRAN name: ZBLMEP.FOR
*
* Subroutines Called:
* ------------------
*
* History:
* -------
*  Version      Date        Author          Description
*     1         Feb94     J.Eisenhamer    Created
*----------------------------------------------------------------------
*
* Input paramters
*
*     id - CCRD table descriptor.
*     colid - Column descriptors.
*     det - Detector in use.
*     line - Line the spectrum lies on
*     sample - Starting sample position of spectrum
*     deltas - Sample increment
*     ns - Number of pixels
*
* Output parameters
*     sline - Store current line
*     bsamp - Store starting sample
*     blem - The blemish array.
*     istat - Status
*
***********************************************************************
      INTEGER ID,COLID(6),DET,NS,ISTAT
      REAL LINE,SAMPLE,DELTAS,SLINE,BSAMP,BLEM(NS)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
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
C
C Column parameters
C
      INTEGER DETCOL,LINE1,LINE2,SAMPLE1,SAMPLE2,EPS,NCOLS
      PARAMETER(DETCOL=1)
      PARAMETER(LINE1=1)
      PARAMETER(LINE2=2)
      PARAMETER(SAMPLE1=3)
      PARAMETER(SAMPLE2=4)
      PARAMETER(EPS=5)
      PARAMETER(NCOLS=6)
C
C----------------------------------------------------------------------
C LOCAL VARIABLES
C
      INTEGER NROWS,ROW,I,IDET
      LOGICAL NULL(NCOLS)
      CHARACTER*164 CONTXT
      REAL ROWBUF(NCOLS-1),CSAMP
C
C Loop through all the rows of the table.
C
      CALL UTPGTI(ID,TBNROW,NROWS,ISTAT)
      IF(ISTAT.NE.0)THEN
         WRITE(CONTXT,1)
 1       FORMAT('Error:cannot read number of rows from CCRD table')
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,I)
         GO TO 1000
      ENDIF
      DO 2000 ROW = 1, NROWS
C
C Get the detector
C
         CALL UTRGTI(ID,COLID(DETCOL),1,ROW,IDET,NULL,ISTAT)
         IF(ISTAT.NE.0)GO TO 2000
         IF(IDET.NE.DET)GO TO 2000
C
C Get rest of row.
C
         CALL UTRGTR(ID,COLID(2),NCOLS-1,ROW,ROWBUF,NULL,ISTAT)
         IF(ISTAT.NE.0)GO TO 2000
C
C Check that we are on the line.
C
         IF(LINE.LT.ROWBUF(LINE1).OR.
     &        LINE.GT.ROWBUF(LINE2))GO TO 2000
C
C Fill the epsilon array.
C
         CSAMP=SAMPLE
         DO 3000 I = 1, NS
            IF(CSAMP.GT.ROWBUF(SAMPLE2))GO TO 2000
            IF(CSAMP.GE.ROWBUF(SAMPLE1))
     &           BLEM(I)=MAX(BLEM(I),ROWBUF(EPS))
            CSAMP=CSAMP+DELTAS
 3000    CONTINUE
C
 2000 CONTINUE
      SLINE=LINE
      BSAMP=SAMPLE
      ISTAT=0
C
 1000 CONTINUE
      RETURN
      END
