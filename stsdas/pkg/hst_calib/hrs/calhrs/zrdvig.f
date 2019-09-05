        SUBROUTINE ZRDVIG(IDIN,VIGFIL,CARPOS,LINE,NG,APERGRP,
     *     LINES,VCPOS,NS,UCPOS,NU,VRESP,ISTAT)
*
*  Module number:
*
*  Module name: ZRDVIG
*
*  Keyphrase:
*  ----------
*       Read vignetting file
*
*  Description:
*  ------------
*       This routine computes the vignetting vector for a given
*       line and carrousel position.  It call ZRDPHC to compute
*       the vignetting for two carrousel positions and interpolates
*       between the two to obtain a response for the observations
*       carrousel position.
*
*  FORTRAN name: ZRDVIG
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       VIGFIL                  I       Vignettine reference file
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZMSPUT, zrdphc
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Desinged and coded
*-------------------------------------------------------------------------------
*
* INPUTS:
*       IDIN - file id number
*       VIGFIL - name of the reference file
*       carpos - carrousel position of the observation
*       line - photocathode line position for which a response is
*               needed
*       ng - number of groups in VIGFIL
*       APERGRP - Groups related to the current aperture.
*       LINES - line positions for each group in VIGFIL
*       vcpos - carrousel positions for each group in VIGFIL
*       ns - number of samples in each group
*       UCPOS - unique carrousel positions in VIGFIL
*       nu - number of elements in UCPOS
*
* OUTPUT:
*
*       VRESP - vignetting for given line and carrousel position
*       istat - error status
*
*----------------------------------------------------------------------------
        CHARACTER*64 VIGFIL
        INTEGER APERGRP(1)
        REAL LINE,VRESP(1),UCPOS(1),VCPOS(1),LINES(1)
        INTEGER NG,NU,NS,ISTAT,CARPOS,IDIN
C
C Local variables
C
        REAL R1(4800),R2(4800)
C                                    --->response vectors for interpolation
        INTEGER I1,I2
C                                    --->groups to interpolate between
        INTEGER GROUPS(1000)
C                                    --->List of group numbers at
C                                       same carrousel position
        REAL LINE1(1000)
C                                   ---->List of line positions at
C                                       same carrousel position
        INTEGER NGU
        REAL FRAC,C1,C2
        INTEGER I,K
        CHARACTER*7 FTYPE
        DATA FTYPE/'VIGFILE'/
C
C----------------------------------------------------------------------------
C
C Determine which carrousel positions to interpolate
C
        I1=0
        I2=0
        DO 100 I=1,NU
                K = NU - I + 1
C                                    --->Reverse order
                IF(UCPOS(I).LE.CARPOS) I1=I
                IF(UCPOS(K).GT.CARPOS) I2=K
100     CONTINUE
C
C don't extrapolate, use closest group
C
        IF(I1.EQ.0)I1=I2
        IF(I2.EQ.0)I2=I1
C
C exact match ?
C
        IF(UCPOS(I1).EQ.CARPOS)I2=I1
C
C GET response for UCPOS(i1) ----------------------------
C
C Find all groups for that carrousel position
        NGU=0
        C1=UCPOS(I1)
        DO 200 I=1,NG
                IF(VCPOS(I).EQ.C1)THEN
                        NGU=NGU+1
                        GROUPS(NGU)=APERGRP(I)
                        LINE1(NGU)=LINES(I)
                ENDIF
200     CONTINUE
        CALL ZSORTR(NGU,LINE1,GROUPS)
        IF(I1.NE.I2)THEN
           CALL ZRDPHC(IDIN,FTYPE,VIGFIL,LINE,NGU,LINE1,GROUPS,NS,
     *                  R1,ISTAT)
        ELSE
           CALL ZRDPHC(IDIN,FTYPE,VIGFIL,LINE,NGU,LINE1,GROUPS,NS,
     *                  VRESP,ISTAT)
        ENDIF
        IF(ISTAT.NE.0)GO TO 999
C
C get response for UCPOS(i2) if different from i1
C
        IF(I2.NE.I1)THEN
                NGU=0
                C2=UCPOS(I2)
                DO 300 I=1,NG
                        IF(VCPOS(I).EQ.C2)THEN
                                NGU=NGU+1
                                GROUPS(NGU)=I
                                LINE1(NGU)=LINES(I)
                        ENDIF
300             CONTINUE
                CALL ZSORTR(NGU,LINE1,GROUPS)
                CALL ZRDPHC(IDIN,FTYPE,VIGFIL,LINE,NGU,LINE1,GROUPS,NS,
     *                          R2,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
C
C Interpolate
C
                FRAC = (CARPOS-C1) / (C2-C1)
                DO 400 I=1,NS
                        VRESP(I) = R1(I) + FRAC * (R2(I)-R1(I))
400             CONTINUE
        ENDIF
        ISTAT=0
999     RETURN
        END
