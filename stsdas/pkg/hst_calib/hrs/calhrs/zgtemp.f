      SUBROUTINE ZGTEMP(SHPID,TNAMES,TEMPS)
C
C Module name: zgtemp
C
C Keyphrase:
C     Read temperature monitors from the SHP.
C
C Description:
C     Read the specified thermistors or temperature monitors, from the
C     SHP and return the values.  If there is a problem retrieving 
C     the temperatures, just set the temperature to -999.
C
C FORTRAN name: zgtemp.for
C
C Keywords of accessed files and tables:
C     <rootname>.shh           I       Standard header packet
C
C History:
C     9Feb94   J. Eisenhamer  Created.
C----------------------------------------------------------------------
C
C Inputs:
C     shpid - Image descriptor of the SHP.
C     tnames - The thermistor names to read from.
C
C Outputs:
C     temps - The temperatures.  Set to -999 if unknown.
C----------------------------------------------------------------------
      INTEGER SHPID,STDOUT,STDERR
      CHARACTER*8 TNAMES(3)
      DOUBLE PRECISION TEMPS(3)
      PARAMETER (STDOUT = 1)
      PARAMETER (STDERR = 2)
C
C Local Variables
C
      INTEGER I,ISTAT
      CHARACTER*162 CONTXT
C
C Read the temperatures
C
      DO 20 I = 1,3
         TEMPS(I)=0.
         CALL UHDGSD(SHPID,TNAMES(I),TEMPS(I),ISTAT)
         IF(ISTAT.NE.0)THEN
            IF(TNAMES(I).NE.' ')THEN
               WRITE(CONTXT,99)TNAMES(I)
 99            FORMAT('Warning: cannot read temperature ',
     &              a8, ' from .shh file')
               CALL ZMSPUT(CONTXT,STDERR+STDOUT,0,ISTAT)
            ENDIF
            TEMPS(I)=-999.D0
         ENDIF
 20   CONTINUE
C
      RETURN
      END
