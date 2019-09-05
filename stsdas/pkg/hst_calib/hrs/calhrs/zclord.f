        SUBROUTINE ZCLORD(PASS,CCR5,GRAT,SCLAMP,BINIDS,CARPOS,
     *     YDEFS,FGWC,CAP,ORDER,ISTAT)
*
*  Module number:
*
*  Module name: ZCLORD
*
*  Keyphrase:
*  ----------
*       Compute spectral order.
*  Description:
*  ------------
*       This routine reads the spectral order coefficients from
*       table CCR5 and computes the spectral order.
*       For first order gratings the spectral order is set to 1
*       For 'ECH-A' and 'ECH-B' it is computed by:
*                                    b*A*sin((C-carpos)/B)
*               order = NINT( -----------------------------------)
*                               ydef - a - d*A*sin((C-carpos)/B)
*
*       where:
*               NINT is the nearest integer
*               A,B,C are CAP_A, CAP_B, and CAP_C in table ccr5
*               a,b,d are LIT_A, LIT_B, and LIT_C in table ccr5
*               carpos is the carrousel position
*               ydef is the y-deflection adjusted for the proper
*                       aperture (LSA: 128 added to it, SC1: 128
*                       subtracted from the y-deflection)
*
*  FORTRAN name: zclord.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       ccr5                    I       Spectral order constant table
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zrccr5
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*	1.1	Oct 90	S. Hulbert	Fixed ydef correction to LSA
*	1.2	Sep 91	S. Hulbert	Implemented PASS flag       
*-------------------------------------------------------------------------------
*
* INPUTS:
*       pass - integer variable set to 1 on first call, -1 on last
*       ccr5 - name of spectral order constant table
*       grat - grating mode (character*3)
*       sclamp - spectral cal lamp on (0 - no, 1 or 2)
*       binids - substep bin ids (integer vector)
*       carpos - carrousel position
*       ydefs - ydeflections (integer vector)
*       fwgc  - Flag indicating global wavelength coefficients
*
* OUTPUTS:
*       cap - values of CAP_A and CAP_C
*       order - integer order number
*       istat - error status
*
*------------------------------------------------------------------------------
        INTEGER PASS
        CHARACTER*64 CCR5
        CHARACTER*5 GRAT
        INTEGER SCLAMP,BINIDS(7),YDEFS(7),ORDER,ISTAT,CARPOS
        CHARACTER*12 FGWC
        DOUBLE PRECISION CAP(2)
C
C LOCAL VARIABLES
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
        DOUBLE PRECISION LITA,LITB,LITD,CAPA,CAPB,CAPC,ASINX,YD
C
C----------------------------------------------------------------------------
C
C Read coefficients on first call
C
        IF(PASS.EQ.FIRST)THEN
           CALL ZRCCR5(CCR5,GRAT,CAPA,LITA,CAPB,LITB,CAPC,LITD,
     *          ISTAT)
           IF(ISTAT.NE.0)GO TO 999
           CAP(1)=CAPA
           CAP(2)=CAPC
        ENDIF
C
C Only compute order for echelles
C
        IF( (GRAT.EQ.'ECH-A').OR.(GRAT.EQ.'ECH-B'))THEN
C
C COMPUTE ORDER
C
           YD=YDEFS(1)
           IF (SCLAMP.EQ.1) YD=YD-128
           IF ((SCLAMP .EQ. 0) .AND. (BINIDS(1) .EQ. 2)) 
     &          YD = YD + 128
           ASINX = CAPA*SIN((CAPC-CARPOS)/CAPB)
C     
C     THE FOLLOWING LINE OF CODE USED TO READ AS:
C
C           ORDER = LITB*ASINX / (YD-LITA-LITD*ASINX)
C
C THIS BUG HAD BEEN INTRODUCED TO ALLOW THE CCCR5 TABLE
C TO BE READ AND THE CORRECT WAVELENGTHS TO BE CALCULATED
C FOR A SET OF TEST DATA! THIS BUG IS NOW CORRECTED.
C
           ORDER = LITB*ASINX / (YD-LITA-LITD*ASINX) + 0.5
        ELSE
C
C FIRST ORDER GRATING
C
           ORDER=1
        ENDIF
        ISTAT=0
999     RETURN
        END
