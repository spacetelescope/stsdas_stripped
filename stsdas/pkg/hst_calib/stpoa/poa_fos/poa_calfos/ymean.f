C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YMEAN (
*
*  inputs
*
     :                  DATA, NS, MASK, WIDTH,
*
*  outputs
*
     :                  RESULT, ISTAT)
*
*  Module number:
*
*  Module name:
*
*  Keyphrase:
*  ----------
*  moving average with mask
*
*  Description:
*  ------------
*  Output pixel value is the average of pixel values of WIDTH (must be an odd 
*  number) points of the input data array at the corresponding position.
*  If any of the WIDTH points is masked or outside the array, the averaging
*  only includes symmetric points on both sides of the pixel point before
*  reaching the invalid point(s).  For example, if WIDTH = 5, and if 
*  pixel 6 is masked, then the output pixel value of the 4th point is the
*  average of input points number 3, 4, and 5.
*  
*  FORTRAN name: YMEAN.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       None
*  SDAS:
*       UMSPUT
*  Others:
*       None
*
*  History:
*  --------
*  Version      Date        Author       Description
*     1       06-13-89     J.-C. HSU     coding
*   1.1	      23-Apr-93	   H. Bushouse	 Declare passed arrays as (*), not (1)
*
*-------------------------------------------------------------------------------
*
*== input:
*                                       --input data array
        REAL                    DATA(*)
*                                       --input array size
        INTEGER                 NS,
*                                       --input mask array
     :                          MASK(*),
*                                       --mean filter width
     :                          WIDTH
*
*== output:
*                                       --output array
        REAL                    RESULT(*)
*                                       --error status
        INTEGER                 ISTAT
*
*== local:
*                                       --error message
        CHARACTER*130           CONTXT, MESS
        REAL                    SUM
        INTEGER                 I, J, K, NPTS, STATOK
*
*=========================begin hsp.inc=========================================
*                                       --status return code
        INTEGER         OK, ERRNUM(20)
        INTEGER         DEST, PRIO
        DATA OK /0/
        DATA ERRNUM /701, 702, 703, 704, 705, 706, 707, 708, 709, 710,
     :               711, 712, 713, 714, 715, 716, 717, 718, 719, 720/
*                                       --message destination and priority
        DATA DEST, PRIO /5, 0/
*=========================end hsp.inc===========================================
*------------------------------------------------------------------------------
*
*  initialize the output result array by copying input array to it
*
        DO 10 I = 1, NS
            RESULT(I) = DATA(I)
   10   CONTINUE
*
*  if WIDTH is an even number, issue error and exit
*
        IF ((WIDTH/2)*2 .EQ. WIDTH) THEN
            ISTAT = ERRNUM(1)
            CONTXT = 'even number is specified for average width'
            GO TO 999
        END IF
*
*  proceed only if WIDTH is larger than 1
*
        IF (WIDTH .GT. 1) THEN
            DO 60 I = 1, NS
*
*  continue only if the mask is OK (=0)
*
                IF (MASK(I) .EQ. 0) THEN
                    DO 20 J = 1, WIDTH/2
*
*  decide how many points to be used in the moving average
*  first, for the end points
*
                        IF ((I-J) .LT. 1 .OR. (I+J) .GT. NS .OR.
*
*  second, for the points whose adjacent points are masked out
*
     :                    MASK(I-J) .NE. 0 .OR. MASK(I+J) .NE. 0)
     :                    GO TO 30
   20               CONTINUE
                    NPTS = WIDTH / 2
                    GO TO 40
   30               NPTS = J - 1
   40               SUM = 0.
                    DO 50 K = I-NPTS, I+NPTS
                        SUM = DATA(K) + SUM
   50               CONTINUE
                    RESULT(I) = SUM / REAL(2*NPTS+1)
                END IF
   60       CONTINUE
        END IF
*
        ISTAT = OK
        GO TO 1000
*
  999   MESS = 'YMEAN: ' // CONTXT
        CALL YMSPUT (MESS, DEST, PRIO,    STATOK)
*
 1000   RETURN
        END
