C**********************************************************************
        REAL FUNCTION PIXOFL (LAMBDA, WVDISP, PXDISP, NDISP)
C**********************************************************************
C
C Version 1.0, October 9 1989
C
C Input:   REAL LAMBDA = wavelength in Ang
C
C Output:  REAL PIXOFL = offset from un-dispersed image in pixels
C              [Note: offset measured NEGATIVE in +L direction]
C
C
C Calibration files: (assumed loaded from calling program)
C
C                    F96_FUV_DISP.DAT
C         or         F96_NUV_DISP.DAT 
C         or         F48_FUV_DISP.DAT
C         or         F48_NUV_DISP.DAT
C
C  Written by Dave Bazell.
C  Common block removed by Phil Hodge, 1-Nov-1993.
C
C**********************************************************************
        REAL LAMBDA, OFF, WVDISP(*), PXDISP(*)
        INTEGER NDISP, NTERMS

        NTERMS = 2
        CALL INTERP (WVDISP, PXDISP, NDISP, NTERMS, LAMBDA, OFF)
        PIXOFL = OFF
        RETURN
        END
