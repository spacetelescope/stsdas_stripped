C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLREP(DTYPE,FILL,N,FRAME,SLICE,DATA,EPS,ISTAT)
*
*  Module number:
*
*  Module name: YCLREP
*
*  Keyphrase:
*  ----------
*       repair bad data values
*
*  Description:
*  ------------
*       This routine repairs bad data values in a background or
*       sky spectra by interpolating between good neighbors.
*       Data points with epsilons < fill are considered good.
*
*  FORTRAN name: zclrep.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*
*  Subroutines Called:
*  -------------------
*       none
*
*  History:
*  --------
*  Version      Date        Author          Description
*    1          Aug 89  D. Lindler      Designed and coded
*  1.1		Apr 93	H. Bushouse	Declare passed arrays as (*), not (1)
*-------------------------------------------------------------------------------
*
* Inputs:
*       dtype - type of data 'sky' or 'background'
*       fill - bad data limit for epsilons
*       n - number of data points
*       frame - frame number
*       slice - slice number
*       eps - epsilon array
*
* Input/output:
*       data - data vector
*
* Outputs:
*       istat - error status
*
*-----------------------------------------------------------------------------
        CHARACTER*10 DTYPE
        REAL FILL,EPS(*),DATA(*)
        INTEGER N,FRAME,SLICE,ISTAT
C
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Local variables
C
        CHARACTER*80 CONTXT
        REAL V1,V2,DIFFI,DIFFV,FRAC
        INTEGER I1,I2,NBAD,II,I
C------------------------------------------------------------------------------
        I1 = 0
C                                    --->Last good point
        I2 = 0
C                                    --->Next good point
        V1 = 0.0
        V2 = 0.0
        NBAD = 0
C                                    --->number of bad points found
C
C LOOP on diodes to find next good point
C
        DO 100 I=1,N
            IF(EPS(I).LT.FILL)THEN
                I2 = I
                V2 = DATA(I)

                IF(I1.EQ.0)THEN
                        V1=V2
                ENDIF
C
C Repair any bad points in between I1 and I2
C
                IF(I2.GT.(I1+1))THEN
                     DIFFI = I2-I1
                     DIFFV = V2-V1

                     DO 50 II=I1+1,I2-1
                        FRAC = (II-I1)/DIFFI
                        DATA(II)=V1 + FRAC*DIFFV
                        NBAD=NBAD+1
50                   CONTINUE

                ENDIF
            I1=I2
            V1=V2
            ENDIF
100     CONTINUE
C
C If last point was bad, propagate last good point
C
        IF(I2.NE.N)THEN
C
C All data bad?
C
            IF(I1.EQ.0)THEN
                WRITE(CONTXT,99)DTYPE,FRAME,SLICE
99              FORMAT('ERROR: All ',A10,' data bad for input group',
     *                  I6,'  slice ',I4)
                CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                CALL YMSPUT('       It will not be subtracted',
     *                  STDOUT+STDERR,0,ISTAT)
                ISTAT=1
                GO TO 1000

              ELSE

                DO 200 II=I1+1,N
                        DATA(II)=V1
200             CONTINUE
            ENDIF

        ENDIF
C
C Tell user about any repair done
C
        IF(NBAD.GT.0)THEN
            WRITE(CONTXT,199)NBAD,DTYPE,FRAME,SLICE
199         FORMAT(I5,' Missing values repaired in ',A10,
     *              ': in group=',I5,' slice=',I3)
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF

        ISTAT=0
1000    RETURN
        END
