        SUBROUTINE ZOGPAR(IDSIN,ISPEC,QOUT,ISTAT)
*
*  Module number:
*
*  Module name: zogpar
*
*  Keyphrase:
*  ----------
*        Write output data set group parameters
*
*  Description:
*  ------------
*        This routine updates group parameters for IDSIN
*
*  FORTRAN name: zogpar.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       IDSIN                   I/O        Input data set
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*  SDAS:
*        uhdas*, uhdgs*
*
*
*  History:
*  --------
*  Version      Date	Author          Description
*        1	May 89	D. Lindler      Designed and coded
*		Sep 90	S. Hulbert	Modified calling sequence and eliminated
*					datamin/datamax update. Allow for old
*					data structure.
*	1.2	May 91	S. Hulbert	Reprocessing headers
*       1.3     Nov 96  M. De La Pena   Added STPTIM to HRSDEF
*-------------------------------------------------------------------------------
*
* Inputs:
*       ids - input image descripter
*       ispec - spectrum number
*       data - data vector
*	qout - extension of image header file
*
* outputs:
*       istat - error status
*
*-------------------------------------------------------------------------
C
	CHARACTER*3 QOUT
        INTEGER IDSIN,ISTAT,ISPEC
C
C-------------------------------------------------------------------------
C
C Header I/O status messages
C
        INTEGER    USHPNF
        PARAMETER (USHPNF =   40)
C
C-------------------------------------------------------------------------
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C                       /HRSMOD/
C Common block containing observing mode parameters
C
C   GRAT - grating mode
C   DET - detector
C   SCLAMP - spectral calibration lamp
C   CARPOS - carrousel position
C   OBSMOD - observation mode (DIR, ACC, TAR)
C   APER - aperture (LSA, SSA, SC1, SC2)
C
        CHARACTER*3 OBSMOD,APER
        CHARACTER*5 GRAT
        INTEGER DET,SCLAMP,CARPOS
        COMMON /HRSMOD/ DET,SCLAMP,CARPOS
        COMMON /HRSMD1/ GRAT,OBSMOD,APER
C
C                        /HRSDEF/
C Deflection pattern common block
C        NBINS - Number of supstep bins
C        NINIT - number of initial deflection pairs
C        IXDEF(5) - initial x-deflections
C        XDEF(7) - x-deflections for each bin
C        YDEF(7) - y-deflections for each bin
C        RCODES(7) - repeat codes for each bin
C        BINIDS(7) - substep bins ids for each bin
C        SAMPLE(7) - starting sample position for each spectra
C        LINE(7) - starting line position for each spectra
C        DELTAS(7) - sample position increment for each spectra
C        HOFF(7),VOFF(7) - horizontal and vertical offsets
C        DOPMAG, DOPZER - dopler magnitude and zero time
C        XDCAL, XDCALP - x-deflection calibration parameters
C        STPTIM - integration time at each step pattern position
C
        INTEGER NBINS,NINIT,IXDEF(5),XDEF(7),YDEF(7),RCODES(7)
        INTEGER BINIDS(7),HOFF(7),VOFF(7)
        REAL SAMPLE(7),LINE(7),DELTAS(7),DOPMAG,XDCAL,XDCALP
        DOUBLE PRECISION DOPZER,STPTIM
        COMMON /HRSDEF/ DOPZER,STPTIM,NBINS,NINIT,IXDEF,XDEF,YDEF,
     *                  RCODES,BINIDS,SAMPLE,LINE,DELTAS,HOFF,VOFF,
     *                  DOPMAG,XDCAL,XDCALP
C
C                         /HRSGPR/
C
C Common block for output group parameter storage
C
C        PKTIME - packet times in MJD
C        ERRCNT - error counts (.d0h)
C        FILLCN - fill counts  (.d0h)
C        SCOEF - sample coefficients
C        EXPO - exposure times
C
        INTEGER ERRCNT(7),FILLCN(7)
        REAL SCOEF(4,7),EXPO(7)
        DOUBLE PRECISION PKTIME(7)
        COMMON /HRSGPR/PKTIME,ERRCNT,FILLCN,SCOEF,EXPO
C
C Local variables
C
        CHARACTER*80 CONTXT
        INTEGER ISTATS(20),I
        CHARACTER*8 GPAR(15)
        DATA GPAR/'FILLCNT','ERRCNT','PKTTIME','YDEF','XDEF',
     *            'SAMPLE','DELTAS','LINE','EXPOSURE','ZSCOEF1',
     *            'ZSCOEF2','ZSCOEF3','ZSCOEF4','BINID',
     *            'CARPOS'/
C
C--------------------------------------------------------------------------
C
C change some of the group parameters
C
        CALL UHDPSI(IDSIN,GPAR(1),FILLCN(ISPEC),ISTATS(1))
        CALL UHDPSI(IDSIN,GPAR(2),ERRCNT(ISPEC),ISTATS(2))
        CALL UHDPSD(IDSIN,GPAR(3),PKTIME(ISPEC),ISTATS(3))
        CALL UHDPSI(IDSIN,GPAR(4),YDEF(ISPEC),ISTATS(4))
        CALL UHDPSI(IDSIN,GPAR(5),XDEF(ISPEC),ISTATS(5))
        CALL UHDPSR(IDSIN,GPAR(6),SAMPLE(ISPEC),ISTATS(6))
        CALL UHDPSR(IDSIN,GPAR(7),DELTAS(ISPEC),ISTATS(7))
        CALL UHDPSR(IDSIN,GPAR(8),LINE(ISPEC),ISTATS(8))
        CALL UHDPSR(IDSIN,GPAR(9),EXPO(ISPEC),ISTATS(9))
        CALL UHDPSR(IDSIN,GPAR(10),SCOEF(1,ISPEC),ISTATS(10))
        CALL UHDPSR(IDSIN,GPAR(11),SCOEF(2,ISPEC),ISTATS(11))
        CALL UHDPSR(IDSIN,GPAR(12),SCOEF(3,ISPEC),ISTATS(12))
        CALL UHDPSR(IDSIN,GPAR(13),SCOEF(4,ISPEC),ISTATS(13))
        CALL UHDPSI(IDSIN,GPAR(14),BINIDS(ISPEC),ISTATS(14))
        CALL UHDPSI(IDSIN,GPAR(15),CARPOS,ISTATS(15))
        DO 10 I=1,15
            IF(ISTATS(I).NE.0) THEN
                CONTXT='ERROR writing group parameter '//GPAR(I)//
     *                        ' to an output '//QOUT//' file'
                GO TO 999
            ENDIF
10      CONTINUE
        ISTAT=0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
