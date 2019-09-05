C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLOPNPIX(PIXTAB,ISTAT)
*
*  Module number:
*
*  Module name: YCLOPNPIX
*
*  Keyphrase:
*  ----------
*       open input data pixel location table files
*
*  Description:
*  ------------
*       This routine opens the input data pixel location file.
*
*  FORTRAN name: yclopnpix.for
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
*       1       Jun 01  A. Alexov       Copy of yclopn.f w/some changes
*               Oct 01  A. Alexov       Added check on type double for input 
*                                       column PIXPOS; CONTXT declared
*-------------------------------------------------------------------------------
* inputs:
*       PIXTAB - name of the file
*
* outputs:
*       ISTAT - error status
*
        CHARACTER*64 PIXTAB
        INTEGER ISTAT
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
      CHARACTER*80 CONTXT
c
c the PIXTAB related global/common vars
      REAL*8 PIXPOS(3000)
      REAL*4 WAVPOS(3000)
      INTEGER PIXPOS_TOTNUM
      COMMON /PIX_POS/ PIXPOS, WAVPOS, PIXPOS_TOTNUM
C
C Local variables
C
      INTEGER NN
      REAL*8  N

C
C FILE I/O ACCESS MODES
C
        INTEGER   RDONLY
        PARAMETER (RDONLY = 1)

      INTEGER   TBNROW,CTYPE
      PARAMETER (TBNROW = 21)
      INTEGER IDIN,COLIDS,NROWS
      CHARACTER*6 COLNAM
      CHARACTER*19 CNAM,CUNIT
      CHARACTER*7  CFORMAT
      INTEGER   TYDOUB
      PARAMETER (TYDOUB = 7)

      LOGICAL NULL
      DATA COLNAM/'PIXPOS'/

C---------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(PIXTAB,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening pix table '//PIXTAB
                GO TO 998
        ENDIF

C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading pix table '//PIXTAB
                GO TO 999
        ENDIF
        if(NROWS.GT.2048)THEN
                CONTXT='ERROR pix tab exceeds max num of rows (2048)'
                GO TO 999
        ENDIF
C
C Get column ids.
        CALL UTCFND(IDIN,COLNAM,1,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in pix table '//
     *                  PIXTAB
                GO TO 999
        ENDIF

C
C check to make sure the data type for the pixpos column is 'double'
        CALL UTCINF(IDIN,COLIDS,CNAM,CUNIT,CFORMAT,CTYPE,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in pix table '//
     *                  PIXTAB
                GO TO 999
        ENDIF

        IF (CTYPE.NE.TYDOUB) THEN
                CONTXT='ERROR PIXPOS is not type double in table '//
     *                  PIXTAB
                GO TO 999
        ENDIF

c get the data
        DO 2214 NN = 1, NROWS
              CALL UTRGTD(IDIN,COLIDS,1,NN,N,NULL,ISTAT)
              DO 30 JJ=1
                 IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR reading pix table '//PIXTAB
                    GO TO 999
                 ENDIF
30            CONTINUE

              PIXPOS(NN)=N
C option to write the table entries to STDOUT
CCC              WRITE(CONTXT, 899), PIXPOS(NN), NN
CCC899           FORMAT('  PIXPOS : ', F14.7, ', for ROW=', I4)
CCC              CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)

 2214   CONTINUE      
        PIXPOS_TOTNUM=NN-1

C close the table
        CALL UTTCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
C
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END

