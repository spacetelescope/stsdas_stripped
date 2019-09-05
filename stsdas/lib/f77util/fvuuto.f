C+
      SUBROUTINE FVUUTO (IN, NBT, OUT)
C
C This routine replaces tabs with spaces in the input string.
C
C Inputs:
C -------
C Input_String
C Number_of_blanks_per_tab
C
C Outputs:
C --------
C Output_String
C
C C. D. Biemesderfer, STScI, 23-May-86
C------------------------------------------------------------------------------
      CHARACTER *(*) IN
      INTEGER        NBT
      CHARACTER *(*) OUT

      CHARACTER *255 CBUF
      CHARACTER *1   TAB
      INTEGER        OP
      INTEGER        IP
C==============================================================================
      CBUF = ' '
      TAB = CHAR (9)
      OP = 1

      DO 10 IP = 1, MIN (LEN(IN), LEN(OUT))
          IF (IN(IP:IP) .NE. TAB) THEN
              IF (OP .LE. LEN(OUT)) CBUF(OP:OP) = IN(IP:IP)
              OP = OP + 1
          ELSE
              OP = OP + NBT - MOD (OP-1, NBT)
          ENDIF
   10 CONTINUE

      OUT = CBUF(1:LEN(OUT))

      RETURN
      END
