        SUBROUTINE YTASCL
*
*  Module number: 14.10.1
*
*  Module name: YTASCL
*
*  Keyphrase:
*  ----------
*       FOS target acquisition
*  Description:
*  ------------
*       This routine performs a scale factor and offset correction
*       to aperture parameters found in a table created by ztamod
*       x, y and flux are scaled by:
*               x' = (xold - xoffset) * xscale
*               y' = (yold - yoffset) * yscale
*               xnew = x'*cos(theta) - y'*sin(theta)
*		ynew = x'*sin(theta) + y'*cos(theta)
*               fnew = (fold - foffset) * fscale
*
*  FORTRAN name: ytascl.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       intable                 I       input table containing apreture
*                                       information (created by ztamod)
*                                          table columns (all required)
*                                               aper_id
*                                               aper_pos
*						time
*                                               left_edge
*                                               right_edge
*                                               upper_edge
*                                               lower_edge
*                                               x_center
*                                               y_center
*                                               x_centroid
*                                               y_centroid
*                                               area
*                                               flux
*       outtable                O       output table with scale change
*                                       same columns as intable.
* CL PARAMTETERS:
* ---------------
*       xoffset                 I       offset in x
*       yoffset                 I       offset in y
*       foffset                 I       flux offset
*       xscale                  I       x scale factor
*       yscale                  I       y scale factor
*       fscale                  I       flux scale factor
*	theta			I	rotation angle
*       xunits                  I       string giving units for relavent
*                                       table columns
*       yunits                  I       string giving units for y columns
*       funits                  I       string giving units for flux
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  SDAS:
*       uclgs*, uttopn, utpgti, utcfnd, uttinc, uttcre, utcnit,
*       utrgt*, utrpt*, uttclo, uthgtt, uthad*
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       D. Lindler  D. Lindler  Designed and coded
*	2	Aug 88	    D. Lindler  Added rotation angle
*-------------------------------------------------------------------------------
C     INCLUDE FILE FOR THE IRAF77 FORTRAN INTERFACE TO THE IRAF VOS
C
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
      INTEGER   RDWRIT
      PARAMETER (RDWRIT = 2)
      INTEGER   WRONLY
      PARAMETER (WRONLY = 3)
      INTEGER   APPEND
      PARAMETER (APPEND = 4)
C
C     CODES FOR DATA TYPES
C
      INTEGER   TYBOOL
      PARAMETER (TYBOOL = 1)
      INTEGER   TYCHAR
      PARAMETER (TYCHAR = 2)
      INTEGER   TYINT
      PARAMETER (TYINT = 4)
      INTEGER   TYREAL
      PARAMETER (TYREAL = 6)
      INTEGER   TYDOUB
      PARAMETER (TYDOUB = 7)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
      INTEGER USRLOG
      PARAMETER (USRLOG = 4)
C
C     UHDAS HEADER PARM TYPES -- CB, DAO, 5-SEP-87
C
      INTEGER GENHDR
      PARAMETER (GENHDR = 0)
      INTEGER IMSPEC
      PARAMETER (IMSPEC = 1)
C
C     THIS SECTION IS FOR PARAMETERS RELEVANT TO TABLE I/O.
C
C     THESE MAY BE SET BY UTPPTI AND/OR READ BY UTPGTI:
C
C                                       LENGTH OF ROW (UNIT = SIZE OF REAL)
      INTEGER   TBRLEN
      PARAMETER (TBRLEN = 1)
C                                       INCREASE ROW LENGTH
      INTEGER   TBIRLN
      PARAMETER (TBIRLN = 2)
C                                       NUMBER OF ROWS TO ALLOCATE
      INTEGER   TBALLR
      PARAMETER (TBALLR = 3)
C                                       INCREASE ALLOC NUM OF ROWS
      INTEGER   TBIALR
      PARAMETER (TBIALR = 4)
C                                       WHICH TYPE OF TABLE? (ROW OR COLUMN)
      INTEGER   TBWTYP
      PARAMETER (TBWTYP = 5)
C                                       MAXIMUM NUMBER OF USER PARAMETERS
      INTEGER   TBMXPR
      PARAMETER (TBMXPR = 6)
C                                       MAXIMUM NUMBER OF COLUMNS
      INTEGER   TBMXCL
      PARAMETER (TBMXCL = 7)
C                                       TYPE = ROW-ORDERED TABLE
      INTEGER   TBTYPR
      PARAMETER (TBTYPR = 11)
C                                       TYPE = COLUMN-ORDERED TABLE
      INTEGER   TBTYPC
      PARAMETER (TBTYPC = 12)
C
C     THESE MAY BE READ BY UTPGTI BUT MAY NOT BE SET:
C
C                                       NUMBER OF ROWS WRITTEN TO
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
C
C     END IRAF77.INC
C
C TABLE PARAMETERS
C
        CHARACTER*19 XCOLS(5),YCOLS(5)
        LOGICAL NULLX(5),NULLY(5),NULLA,NULLAP,NULLF,NULLT
        DOUBLE PRECISION X(5),Y(5),FLUX,AREA
        CHARACTER*6 FGWA
        CHARACTER*3 APER
        CHARACTER*6 APERPS
        CHARACTER*5 DET
        CHARACTER*24 TIME
        INTEGER IDXIN(5),IDXOUT(5),IDYIN(5),IDYOUT(5),IDAIN,IDAOUT
        INTEGER IDAPIN,IDAPOU,IDFIN,IDFOUT,NROWS,IROW,IDPOSI,IDPOSO
        INTEGER IDTIN,IDTOUT
C
C CL PARAMETERS
C
        DOUBLE PRECISION XSCALE,YSCALE,XOFF,YOFF,FSCALE,FOFF,THETA
        CHARACTER*19 XUNITS,YUNITS,FUNITS
C
C FILE PARAMETERS
C
        CHARACTER*64 INPUT,OUTPT
        INTEGER IDIN,IDOUT,ISTATS(30),ISTAT
C
C OTHERS
C
        CHARACTER*130 CONTXT
        INTEGER I
        DOUBLE PRECISION T,XNEW
C
C  DATA DECLARATIONS
C
        DATA XCOLS/'left_edge','right_edge','x_center','x_centroid',
     *                  'x_crosscor'/
        DATA YCOLS/'upper_edge','lower_edge','y_center','y_centroid',
     *                  'y_crosscor'/
C
C--------------------------------------------------------------------------
C
C GET CL PARAMETERS
C
        CALL UCLGST('intable',INPUT,ISTATS(1))
        CALL UCLGST('outtable',OUTPT,ISTATS(2))
        CALL UCLGSD('xoffset',XOFF,ISTATS(3))
        CALL UCLGSD('xscale',XSCALE,ISTATS(4))
        CALL UCLGSD('yoffset',YOFF,ISTATS(5))
        CALL UCLGSD('yscale',YSCALE,ISTATS(6))
        CALL UCLGSD('theta',THETA,ISTATS(12))
        CALL UCLGSD('foffset',FOFF,ISTATS(7))
        CALL UCLGSD('fscale',FSCALE,ISTATS(8))
        CALL UCLGST('xunits',XUNITS,ISTATS(9))
        CALL UCLGST('yunits',YUNITS,ISTATS(10))
        CALL UCLGST('funits',FUNITS,ISTATS(11))
        CONTXT='Error Reading CL parameter'
        DO 10 I=1,12
                IF(ISTATS(I).NE.0)GO TO 999
10      CONTINUE
C
C OPEN INPUT TABLE AND GET COLIDS
C
        CALL UTTOPN(INPUT,RDONLY,IDIN,ISTATS(1))
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTATS(2))
        CALL UTCFND(IDIN,XCOLS,5,IDXIN,ISTATS(3))
        CALL UTCFND(IDIN,YCOLS,5,IDYIN,ISTATS(4))
        CALL UTCFND(IDIN,'aper_id',1,IDAPIN,ISTATS(5))
        CALL UTCFND(IDIN,'area',1,IDAIN,ISTATS(6))
        CALL UTCFND(IDIN,'flux',1,IDFIN,ISTATS(7))
        CALL UTCFND(IDIN,'aper_pos',1,IDPOSI,ISTATS(8))
        CALL UTCFND(IDIN,'time',1,IDTIN,ISTATS(9))
        CONTXT='Error Reading input table'
        DO 20 I=1,9
                IF(ISTATS(I).NE.0) GO TO 999
20      CONTINUE
C
C CREATE NEW OUTPUT TABLE USING INPUT AS A TEMPLATE
C
        CALL UTTINC(OUTPT,IDIN,IDOUT,ISTATS(1))
        CALL UTTCRE(IDOUT,ISTATS(2))
        IF((ISTATS(1).NE.0).OR.(ISTATS(2).NE.0))THEN
                CONTXT='Error opening output table'
                GO TO 999
        ENDIF
C
C GET COLUMN IDS FOR OUTPUT TABLE
C
        CALL UTCFND(IDOUT,XCOLS,5,IDXOUT,ISTATS(1))
        CALL UTCFND(IDOUT,YCOLS,5,IDYOUT,ISTATS(2))
        CALL UTCFND(IDOUT,'aper_id',1,IDAPOU,ISTATS(3))
        CALL UTCFND(IDOUT,'area',1,IDAOUT,ISTATS(4))
        CALL UTCFND(IDOUT,'flux',1,IDFOUT,ISTATS(5))
        CALL UTCFND(IDOUT,'aper_pos',1,IDPOSO,ISTATS(6))
        CALL UTCFND(IDOUT,'time',1,IDTOUT,ISTATS(7))
C
C CHANGE OUTPUT COLUMN UNITS
C
        CALL UTCNIT(IDOUT,IDFOUT,FUNITS,ISTATS(8))
        DO 25 I=1,5
                CALL UTCNIT(IDOUT,IDXOUT(I),XUNITS,ISTATS(8+I))
                CALL UTCNIT(IDOUT,IDYOUT(I),YUNITS,ISTATS(13+I))
25      CONTINUE
        CALL UTCNIT(IDOUT,IDAOUT,' ',ISTATS(19))
        CONTXT='Error creating output table'
        DO 30 I=1,19
                IF(ISTATS(I).NE.0) GO TO 999
30      CONTINUE
C
C LOOP ON ROWS IN INPUT TABLE
C
        DO 100 IROW=1,NROWS
C
C READ ROW FROM INPUT TABLE
C
                CALL UTRGTT(IDIN,IDAPIN,1,IROW,APER,NULLAP,ISTATS(1))
                CALL UTRGTD(IDIN,IDXIN,5,IROW,X,NULLX,ISTATS(2))
                CALL UTRGTD(IDIN,IDYIN,5,IROW,Y,NULLY,ISTATS(3))
                CALL UTRGTD(IDIN,IDAIN,1,IROW,AREA,NULLA,ISTATS(4))
                CALL UTRGTD(IDIN,IDFIN,1,IROW,FLUX,NULLF,ISTATS(5))
                CALL UTRGTT(IDIN,IDPOSI,1,IROW,APERPS,NULLAP,ISTATS(6))
                CALL UTRGTT(IDIN,IDTIN,1,IROW,TIME,NULLT,ISTATS(7))
                CONTXT='Error Reading input table'
                DO 40 I=1,7
                        IF(ISTATS(I).NE.0) GOTO 999
40              CONTINUE
C
C SCALE DATA IF NOT NULL
C
                DO 50 I=1,5
                    IF(.NOT.NULLX(I))X(I)=(X(I)-XOFF)*XSCALE
                    IF(.NOT.NULLY(I))Y(I)=(Y(I)-YOFF)*YSCALE
C
C ROTATE THE APERTURE CENTER POSITIONS (NOT EDGE POSITIONS)
C
                    IF((I.GT.2).AND.(THETA.NE.0.0))THEN
                        IF(NULLX(I).OR.NULLY(I))THEN
                             IF(NULLX(I))Y(I)=X(I)
                             IF(NULLY(I))X(I)=Y(I)
                          ELSE
                             T=THETA*3.14159/180.0
                             XNEW=X(I)*DCOS(T)-Y(I)*DSIN(T)
                             Y(I)=X(I)*DSIN(T)+Y(I)*DCOS(T)
                             X(I)=XNEW
                         ENDIF
                    ENDIF
50              CONTINUE
                IF(.NOT.NULLF)FLUX=(FLUX-FOFF)*FSCALE
                IF(.NOT.NULLA)AREA=AREA*XSCALE*YSCALE
C
C WRITE DATA TO OUTPUT TABLE
C
                CALL UTRPTT(IDOUT,IDAPOU,1,IROW,APER,ISTATS(1))
                CALL UTRPTD(IDOUT,IDXOUT,5,IROW,X,ISTATS(2))
                CALL UTRPTD(IDOUT,IDYOUT,5,IROW,Y,ISTATS(3))
                CALL UTRPTD(IDOUT,IDAOUT,1,IROW,AREA,ISTATS(4))
                CALL UTRPTD(IDOUT,IDFOUT,1,IROW,FLUX,ISTATS(5))
                CALL UTRPTT(IDOUT,IDPOSO,1,IROW,APERPS,ISTATS(6))
                CALL UTRPTT(IDOUT,IDTOUT,1,IROW,TIME,ISTATS(7))
                CONTXT='Error writing to output table'
                DO 60 I=1,7
                        IF(ISTATS(I).NE.0)GO TO 999
60              CONTINUE
100     CONTINUE
C
C PLACE HEADER PARAMETERS IN OUTPUT TABLE
C
        CALL UTHGTT(IDIN,'fgwa_id',FGWA,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='FGWA_ID parameter missing from intable'
                GO TO 999
        ENDIF
        CALL UTHGTT(IDIN,'detector',DET,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='Detector parameter missing from intable'
                GO TO 999
        ENDIF
        CALL UTHADT(IDOUT,'fgwa_id',FGWA,ISTATS(1))
        CALL UTHADT(IDOUT,'detector',DET,ISTATS(8))
        CALL UTHADD(IDOUT,'xoffset',XOFF,ISTATS(2))
        CALL UTHADD(IDOUT,'xscale',XSCALE,ISTATS(3))
        CALL UTHADD(IDOUT,'yoffset',YOFF,ISTATS(4))
        CALL UTHADD(IDOUT,'yscale',YSCALE,ISTATS(5))
        CALL UTHADD(IDOUT,'foffset',FOFF,ISTATS(6))
        CALL UTHADD(IDOUT,'fscale',FSCALE,ISTATS(7))
        CALL UTHADD(IDOUT,'theta',THETA,ISTATS(9))
        DO 200 I=1,9
                IF(ISTATS(I).NE.0) THEN
                        CONTXT='Error writing output table'
                        GO TO 999
                ENDIF
200     CONTINUE
C
C ALL DONE, CLEAN UP
C
        CALL UTTCLO(IDIN,ISTAT)
        CALL UTTCLO(IDOUT,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='Error closing output table'
                GO TO 999
        ENDIF
        GO TO 1000
999     CALL UMSPUT(CONTXT,STDERR+STDOUT,0,ISTAT)
1000    RETURN
        END
