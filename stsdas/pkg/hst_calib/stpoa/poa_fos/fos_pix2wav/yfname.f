C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YFNAME(ROOT,QUAL,GROUP,NGROUP,NAME)
*
*  Module number:
*
*  Module name: YFNAME
*
*  Keyphrase:
*  ----------
*       Construct file name
*  Description:
*  ------------
*       This routine constructs an sdas file name containing the
*       group information.  It takes a rootname, qualifier, group number
*       and optional number of groups and creates a name in the form:
*               <rootname>.<qualifer>[<group number>]
*                       or when ngroup greater than zero
*               <rootname>.<qualifer>[<group number>/<ngroup>]
*
*  FORTRAN name: YFNAME.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*       none
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       JULY 89  D. Lindler      Designed and coded
*-------------------------------------------------------------------------------
*
* Input parameters
*       ROOT - root name of the file (character)
*       QUAL - 3 character qualifier for the file
*       GROUP - group number, integer
*       NGROUP - number of groups (for new output file), integer
* Output parameters
*       NAME - file name (character*64)
*------------------------------------------------------------------------------
        CHARACTER*64 ROOT,NAME,FILE
        CHARACTER*3 QUAL
        INTEGER GROUP,NGROUP
C
C LOCAL VARIABLES
C
        INTEGER I,J,J2
        CHARACTER*5 VALUE,VALUE2
C----------------------------------------------------------------------------
C
C CHANGE GROUP TO A STRING
C
        WRITE(VALUE,9)GROUP
9       FORMAT(I5)
        DO 1 J=1,5
1               IF(VALUE(J:J).NE.' ') GO TO 2
C
C CHANGE NGROUP TO A STRING
C
2       WRITE(VALUE2,9)NGROUP
        DO 3 J2=1,5
3               IF(VALUE2(J2:J2).NE.' ') GO TO 4
C
C FIND END OF ROOT (FIRST BLANK)
C
4       DO 10 I=1,64
10              IF( ROOT(I:I) .EQ. ' ') GO TO 20
20      FILE=ROOT
        IF (QUAL(1:1) .NE. ' ') THEN
                 FILE=ROOT(1:I-1)//'.'//QUAL
                 I=I+4
        ENDIF
        IF(NGROUP.EQ.0)THEN
                WRITE(NAME,99)FILE(1:I-1),'[',VALUE(J:5),']'
99              FORMAT(A,A1,A,A1)
           ELSE
                WRITE(NAME,199)FILE(1:I-1),'[',VALUE(J:5),'/',
     *                          VALUE2(J2:5),']'
199             FORMAT(A,A1,A,A1,A,A1)
        ENDIF
        RETURN
        END
