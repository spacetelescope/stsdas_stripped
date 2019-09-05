        SUBROUTINE ZCLMNF(IN,NS,WIDTH,OUT,ISTAT)
*
*  Module number:
*
*  Module name: ZCLMNF
*
*  Keyphrase:
*  ----------
*       Mean filter the background
*
*  Description:
*  ------------
*       This routine performs a mean filter of the background
*       with a user specified filter width.  The edge points
*       are filled with the value of the closest position that
*       can be filtered.
*
*  FORTRAN name: ZCLMNF.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*-------------------------------------------------------------------------------
*
* INPUTS:
*       IN - input array to filter
*       NS - number of elements in IN
*       WIDTH - filter width
*
* OUTPUTS:
*       OUT - filtered array
*       ISTAT - error status
*
*----------------------------------------------------------------
        INTEGER NS,ISTAT,WIDTH
        REAL IN(1),OUT(1)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C local variables
C
        INTEGER N
C                                    --->number of points to filter
        INTEGER HWIDTH
C                                    --->half filter width
        INTEGER CENTER
C                                    --->center of filter
        INTEGER IPOS,IOFF,I,J
C                                    --->Indices
        REAL SUM
        REAL LEFT,RIGHT
C                                    --->Fill values for the ends of the vector
C
C-------------------------------------------------------------------
C
C Check filter size
C
        IF((WIDTH.LT.0).OR.(WIDTH.GT.511))THEN
                CALL ZMSPUT('ERROR: invalid mean filter width',
     *                  STDOUT+STDERR,0,ISTAT)
                ISTAT=0
                GO TO 999
        ENDIF
        IF(WIDTH.LE.1)THEN
                DO 10 I=1,NS
10                  OUT(I)=IN(I)
           ELSE
                HWIDTH=WIDTH/2
                N=NS-HWIDTH*2
C                                    --->Number of positions to filter
                IOFF=0
C                                    --->Offset in input array
                IPOS=HWIDTH+1
C                                    --->First point in output array
                CENTER=HWIDTH+1
C                                    --->Center of the filter
                DO 100 I=1,N
C
C compute sum within the filter range
C
                        SUM=0.0
                        DO 20 J=1,WIDTH
20                              SUM = SUM + IN(J+IOFF)
                        OUT(IPOS)=SUM/WIDTH
C                                    ---> compute average
                        IPOS=IPOS+1
                        IOFF=IOFF+1
100             CONTINUE
C
C fill edge points
C
                LEFT=OUT(CENTER)
                RIGHT=OUT(NS-HWIDTH)
                DO 200 I=1,HWIDTH
                        OUT(I)=LEFT
                        OUT(NS+1-I)=RIGHT
200             CONTINUE
        ENDIF
        ISTAT=0
999     RETURN
        END
