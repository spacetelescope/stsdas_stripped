C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLOPNPOA(POAFILE,SPECNUM,NXSTEP,ISTAT)
*
*  Module number:
*
*  Module name: YCLOPNPOA
*
*  Keyphrase:
*  ----------
*       Open the poafile, read offset values and determine sub-pix offset.
*
*  Description:
*  ------------
*       This tourine opens the poafile (ascii), reads the appropriate 
*       offset value(s) and then determine sub-pix offset need for the
*       pixel table values.  Once the offset is calculated, it is added
*       to the pixel table values.  If this routine is not called, the
*       pixel table values remain as is, and therefore do not get the
*       sub-pixel correction applied.
*
*  FORTRAN name: yclopnpoa.f
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput,yfname
*  SDAS:
*       uimopn, uhdgsr, uimgid
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Oct 01  A. Alexov       Copy of yclopnpix.f w/some changes
*-------------------------------------------------------------------------------
* inputs:
*       POAFILE - name of the file
*
* outputs:
*       ISTAT - error status
*
        CHARACTER*64 POAFILE
        INTEGER ISTAT, SPECNUM, NXSTEP
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)

c
c the PIXTAB related global/common vars
      REAL*8 PIXPOS(3000)
      REAL*4 WAVPOS(3000)
      INTEGER PIXPOS_TOTNUM
      COMMON /PIX_POS/ PIXPOS, WAVPOS, PIXPOS_TOTNUM
C
C Local variables
C
      INTEGER NUM, APPLIED_VAL
      REAL*8  VALUE, AVE_VAL, FINAL_VAL, OFFSET
      CHARACTER*120 BUF
      CHARACTER*21  JUNK
      CHARACTER*80 CONTXT
C
C FILE I/O ACCESS MODES
C
        INTEGER   BADNUM

C---------------------------------------------------------------------------
C
C Open file
      OPEN(29,FILE=POAFILE,STATUS='OLD',ERR=999) 


C read the elements ascii records, pick sub-pix value
C if SPECNUM = INDEF, then take average of all offsets from the file
C if SPECNUM = #, then take that spec value from the file
C if SPECNUM = # larger than file contains, then same as INDEF 
C 
C the diode offset is read off every 38th line of the ascii file;
C to calculate the offset, you need the diode to pixel conversion

      J = 1
      NUM = 0
      BADNUM = 0
      AVE_VAL = 0.0
      FINAL_VAL = 0.0
      OFFSET = 0.0

C loop over the file, until the end, reading every 38th line
C if the specnum is reached, then read that as the final value, exit loop
 100  IF (BADNUM.EQ.0) THEN 
        IF (MOD(J,38).EQ.0) THEN
           READ (29,763,IOSTAT=BADNUM) JUNK, VALUE 
 763       FORMAT(A21,F8.3)
           NUM = NUM+1
           IF (NUM.EQ.SPECNUM) THEN
              FINAL_VAL=VALUE
              BADNUM=1
           ENDIF
           AVE_VAL = AVE_VAL+VALUE
        ELSE
           READ (29,706,IOSTAT=BADNUM) BUF
 706       FORMAT(A120)
        ENDIF
        J = J+1 
        GO TO 100
      ENDIF

c calculate the averate using the sum and the number of spec found
      AVE_VAL = AVE_VAL / NUM

c check if the spectral number indicated is greater than the number 
c of spectra found in the .poa file;  if so, then use the average
c value as the answer;  warn the user
      IF (SPECNUM.GT.NUM) THEN
        FINAL_VAL = AVE_VAL
        CONTXT='WARNING: SPECNUM exceeds number of spectra in file;'
        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
        CONTXT='         using average of all offset values for sub-pix'
        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
      ENDIF

c cut down the number of answers to just variable - AVE_VAL
      IF (FINAL_VAL.NE.0) AVE_VAL=FINAL_VAL

c finally, if it's non-zero, then convert to pixels, and figure out the
c actual offset; add the offset to the pixel position array
      IF (AVE_VAL.NE.0) THEN
         AVE_VAL = NXSTEP*AVE_VAL/4.
         APPLIED_VAL = NINT(AVE_VAL)
         OFFSET= AVE_VAL - APPLIED_VAL

         WRITE(CONTXT, 799), AVE_VAL, APPLIED_VAL
 799     FORMAT('The POA offset was: ',F7.3, ' pix, ',I3, 
     *          '. pix was applied')
         CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
         WRITE(CONTXT, 800), OFFSET
 800     FORMAT('Therefore the calculated sub-pix correction is: ',F7.3)
         CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)

         DO 444 J=1,PIXPOS_TOTNUM
C optional print statement of the old and new pixel values
C to STDOUT
ccc            WRITE(CONTXT, 899), PIXPOS(J), PIXPOS(J)+OFFSET
ccc 899        FORMAT('  PIXPOS ORIG: ', F10.3, ';   NEWPIX: ', F10.3)
ccc            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)

            PIXPOS(J)=PIXPOS(J)+OFFSET
 444     CONTINUE
      ENDIF
      GO TO 1000
      

999     ISTAT=1
        CLOSE(29)
        
1000    RETURN
        END

