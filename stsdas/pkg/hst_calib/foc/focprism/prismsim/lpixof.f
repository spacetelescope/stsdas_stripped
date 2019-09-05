C**********************************************************************
      REAL FUNCTION LPIXOF (OFFSET, WVDISP, PXDISP, NDISP, STATUS)
C**********************************************************************
C
C Input:  REAL OFFSET = offset from un-dispersed image in pixels
C              [Note: offset measured NEGATIVE in +L direction]
C
C Output: REAL LPIXOF = wavelength in Ang
C     [Note: LPIXOF returned as first or last entry in
C     DISP.DAT table if out of range]
C         INTEGER STATUS = zero if OK
C
C Calls: PIXOFL (assumed initialized from calling program)
C
C Calibration files: (assumed loaded from calling program)
C
C                    F96_FUV_DISP.DAT
C         or         F96_NUV_DISP.DAT 
C         or         F48_FUV_DISP.DAT
C         or         F48_NUV_DISP.DAT
C
C  Written by Dave Bazell, October 9, 1989.
C  Common block removed by Phil Hodge, 1-Nov-1993.
C
C**********************************************************************
      REAL WAVE1, WAVE2, WAVMID, OFFSET, OFFTST, PIXOFL
      INTEGER STATUS
      INTEGER J, K
      REAL WVDISP(*), PXDISP(*)
      INTEGER NDISP

      STATUS = 0
      LPIXOF = 0

C     find bracketing values in PXDISP array

      IF(OFFSET.LE.PXDISP(1))THEN
        LPIXOF=WVDISP(1)
        RETURN
      ELSE IF(OFFSET.GT.PXDISP(NDISP))THEN
        LPIXOF=WVDISP(NDISP)
        RETURN
      ELSE
C        J=2
C        DO WHILE((OFFSET.GE.PXDISP(J)).AND.(J.LT.NDISP))
C           J=J+1
C        END DO
        J = 2
        DO 10 K = 2, NDISP-1
          IF (OFFSET .GE. PXDISP(J)) THEN
            J = J + 1
          ELSE
            GO TO 20
          ENDIF
10      CONTINUE
20      CONTINUE
        WAVE1=WVDISP(J-1)
        WAVE2=WVDISP(J)
      ENDIF

C     check for good offset value

      OFFTST = PIXOFL (WAVE1, WVDISP, PXDISP, NDISP) - OFFSET
      WAVMID = PIXOFL (WAVE2, WVDISP, PXDISP, NDISP) - OFFSET
      IF (OFFTST*WAVMID.GT.0) THEN
C       PAUSE 'Bad offset value in LPIXOF'
        STATUS = 1
        RETURN
      ENDIF

C     check sense of slope
      
      IF (OFFTST.GT.0) THEN
      WAVMID=WAVE2
      WAVE2=WAVE1
      WAVE1=WAVMID
      ENDIF

C     find LPIXOF through bisection

      DO 30 J=1,250
        WAVMID=(WAVE1+WAVE2)/2.
        OFFTST = PIXOFL (WAVMID, WVDISP, PXDISP, NDISP) - OFFSET
        IF (OFFTST.LT.0.) THEN
          WAVE1=WAVMID
        ELSE
          WAVE2=WAVMID
        ENDIF
        IF ((ABS(OFFTST).LT.0.005)
     #    .OR.(ABS(WAVE1-WAVE2).LT.0.01)) THEN
          LPIXOF=WAVMID
          RETURN
        ENDIF
30    CONTINUE
C     PAUSE 'No convergence in in LPIXOF'
      STATUS = 2
      END
