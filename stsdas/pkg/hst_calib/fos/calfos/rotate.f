        SUBROUTINE ROTATE(VECTOR,ANGLE,AXIS)
*
*  Module number:
*
*  Module name: ROTATE
*
*  Keyphrase:
*  ----------
*	Rotate 3-D vector by a specfied angle about a specified axis
*
*  Description:
*  ------------
*	Take a double precsion 3-vector and rotate it by a double
*	precision angle around one axis (axis 1 = x; axis 2 = y;
*	axis 3 = z)
*
*  FORTRAN name: ROTATE.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments

*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 91	    S. Hulbert      Designed and coded
*-------------------------------------------------------------------------------
*
* INPUTS:
*	angle - angle (in degrees) to rotate
*	axis - axis number: 1 = x, 2 = y, 3 = z
*
* INPUT/OUTPUT:
*	vector - three-vector to rotate
*
* OUTPUT:
*	istat - status
*
        DOUBLE PRECISION VECTOR(3)
        DOUBLE PRECISION ANGLE
        INTEGER AXIS
C
        DOUBLE PRECISION DEGRAD
        PARAMETER (DEGRAD = 57.295779513082D0)
C
C local variables
C
        INTEGER I, J
        DOUBLE PRECISION S, C, R(3,3), SUM(3)
C
C-----------------------------------------------------------------------
C
C zero rotation matrix
C
        DO 100 I = 1, 3
            DO 110 J = 1, 3
                R(I,J) = 0.0D0
110             CONTINUE
100         CONTINUE
C
        S = SIN(ANGLE/DEGRAD)
        C = COS(ANGLE/DEGRAD)
C
C set up rotation matrix for the appropriate axis
C
        IF (AXIS .EQ. 1) THEN
            R(1,1) = 1.0D0
            R(2,2) = C
            R(2,3) = -S
            R(3,2) = S
            R(3,3) = C
        ELSE IF (AXIS .EQ. 2) THEN
            R(1,1) = C 
            R(1,3) = S
            R(2,2) = 1.0D0
            R(3,1) = -S
            R(3,3) = C
        ELSE 
            R(1,1) = C
            R(1,2) = -S
            R(2,1) = S
            R(2,2) = C
            R(3,3) = 1.0D0
        ENDIF
C
C rotate
C
        DO 200 I = 1, 3
            SUM(I) = 0.0D0
            DO 210 J = 1,3
                SUM(I) = SUM(I) + R(J,I)*VECTOR(J)
210            CONTINUE
200        CONTINUE
C
C put rotated vector into input vector
C
        DO 300 I = 1, 3
            VECTOR(I) = SUM(I)
300        CONTINUE
C
        RETURN
        END

