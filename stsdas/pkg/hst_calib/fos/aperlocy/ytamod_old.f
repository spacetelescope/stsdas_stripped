        SUBROUTINE YTAMOD
*
*  Module number: 13.13.2.1
*
*  Module name: YTAMOD
*
*  Keyphrase:
*  ----------
*       FOS aperture location and sizes
*  Description:
*  ------------
*       This routine computes the aperture locations and sizes
*       in an FOS map.
*
*  FORTRAN name: YTAMOD.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       input                   I       FOS Y-map (file template)
*       table                   O       output table of edge locations
*                                       and sizes.  Columns in the table
*                                       are:
*                                               aper_id - aperture name
*                                               aper_pos - upper, lower, center
*                                               left_edge
*                                               right_edge
*                                               upper_edge
*                                               lower_edge
*                                               x_center
*                                               y_center
*                                               x_centroid
*                                               y_centroid
*                                               x_crosscor
*                                               y_crosscor
*                                               area
*                                               flux
*       ybases                  O       vector of y-positions observed
*       profile                 O       Vector of y-profile
*
*  CL parameters
*  -------------
*       tabstat         output table status (write or append)
*       aperpos         specification of which aperture of pair is present
*                       both, upper, or lower
*       ymiddle         y-position of midpoint between apertures
*       yfirst          starting y-position to use
*       ylast           last y-position to use
*       d1              first diode to use
*       d2              last diode to use
*       twidthx         cross correlation template width in in sample units
*                               for the x-directions
*       twidthy         cross correlation template width for the y-direction
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ygptrn, ygmode, tabinv, tainfo
*  SDAS:
*       uclgs*, uimotp, uimxtp, uimgid, uigl1d, uimclo, uttinn,
*       utppti, utcdef, uttopn, uttcre, utcfnd, utpgti, utrpt*,
*       uthadt, uttclo, umsput
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Sept 87   D. Lindler    Designed and coded
*	2	AUG 88	  D. Lindler	Added time to output table
*					Corrected upper/lower reversal
*					changed to use YPOS group param.
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
C LOCAL VARIABLES
C
C  HEADER KEYWORD VALUES
C
        CHARACTER*6 FGWA
        CHARACTER*3 APER
        CHARACTER*5 DET
        CHARACTER*1 POLAR
        CHARACTER*6 APERPS
        CHARACTER*6 APERP
        CHARACTER*24 TIME
        INTEGER PASSDR
        INTEGER XSTEPS,OVRSCN,FCHNL,NCHNL
C
C FILE I/O PARAMETERS
C
        INTEGER IDIN,IDOUT,NAXIS,DIMEN(7),DTYPE,NROWS,TEMPD
        CHARACTER*64 IMNAME
C
C OUTPUT TABLE PARAMETERS
C
        CHARACTER*19 COLNAM(15),CUNITS(15)
        CHARACTER*5 CFORM(15)
        INTEGER CTYPE(15),COLIDS(15)
C
C CL PARAMETERS
C
        INTEGER TWIDX,TWIDY
        CHARACTER*64 INPUT,OUTPUT
        CHARACTER*6 TBSTAT
        DOUBLE PRECISION YFIRST,YLAST,YMID,D1,D2
C
C INPUT AREA TO PROCESS
C
        INTEGER AREA(4)
C
C COMPUTED RESULTS
C
        DOUBLE PRECISION EDGES(4),ECENT(2),CTROID(2),CCENT(2),FLUX
C
C OTHERS
C
        INTEGER ISTATS(21),ISTAT,I,IAPER,NAPER,S1,S2,L1,L2
        INTEGER L3
        LOGICAL PAIRED
        CHARACTER*130 CONTXT
        DOUBLE PRECISION A,RPOS,LOOKUP
        CHARACTER*3 PAIR(4)
C
C IMAGE DEFINITION
C
        DOUBLE PRECISION IMAGE(51200),YPOS(500)
        INTEGER NS,NL,LINE
C
C  DATA DECLARATIONS
C
        DATA COLNAM/
     *          'aper_id','aper_pos','left_edge','right_edge',
     *          'upper_edge','lower_edge','x_center','y_center',
     *          'x_centroid','y_centroid','x_crosscor',
     *          'y_crosscor','area','flux','time'/
        DATA CUNITS/' ',' ','diodes','diodes','deflection steps',
     *          'deflection steps','diodes','deflection steps',
     *          'diodes','deflection steps','diodes',
     *          'deflection steps',' ',' ',' '/
        DATA CFORM/'A4','A7',10*'F12.6',' ',' ',' '/
        DATA CTYPE/-3,-6,12*TYREAL,-24/
        DATA AREA/4*1/
        DATA PAIR/'A-2','A-3','A-4','C-1'/
C
C---------------------------------------------------------------------
C
C GET INPUT CL PARAMETERS
C
        CALL UCLGST('input',INPUT,ISTATS(11))
        CALL UCLGST('table',OUTPUT,ISTATS(1))
        CALL UCLGST('tabstat',TBSTAT,ISTATS(2))
        CALL UCLGSD('yfirst',YFIRST,ISTATS(3))
        CALL UCLGSD('ymiddle',YMID,ISTATS(4))
        CALL UCLGSD('ylast',YLAST,ISTATS(5))
        CALL UCLGSD('d1',D1,ISTATS(6))
        CALL UCLGSD('d2',D2,ISTATS(7))
        CALL UCLGST('aperpos',APERPS,ISTATS(8))
        CALL UCLGSI('twidthx',TWIDX,ISTATS(9))
        CALL UCLGSI('twidthy',TWIDY,ISTATS(10))
        DO 10 I=1,11
                IF(ISTATS(I).NE.0) THEN
                        CONTXT='Error getting CL parameter'
                        GO TO 999
                ENDIF
10      CONTINUE
C
C --------------------------------------------------
C LOOP ON INPUT IMAGES
C
        LINE = 0
        CALL UIMOTP(INPUT,TEMPD,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='error opening input file template'
                GO TO 999
        ENDIF
C
C EXPAND TEMPLATE
C
200     CALL UIMXTP(TEMPD,IMNAME,ISTAT)
        IF(ISTAT.LT.0)THEN
                CALL UIMCTP(TEMPD,ISTAT)
                GO TO 500
        ENDIF
        IF(ISTAT.NE.0)THEN
                CONTXT='Error in input file name'
                GO TO 999
        ENDIF
C
C OPEN NEXT IMAGE
C
        CALL UIMOPN(IMNAME,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='Error opening input image  '//IMNAME
                GO TO 999
        ENDIF
C
C GET HEADER INFO
C
        CALL YGMODE(IDIN,DET,FGWA,APER,APERP,POLAR,PASSDR,ISTAT)
        IF(ISTAT.NE.0) THEN
                CONTXT='ERROR in image '//IMNAME
                GO TO 999
        ENDIF
        CALL YXPTRN(IDIN,FCHNL,NCHNL,XSTEPS,OVRSCN,ISTAT)
        IF(ISTAT.NE.0) THEN
                CONTXT='ERROR in image '//IMNAME
                GO TO 999
        ENDIF
C
C IF FIRST LINE GET OBSERVATIONS TIME
C
        IF(LINE.EQ.0)THEN
            CALL UHDGST(IDIN,'FPKTTIME',TIME,ISTAT)
            IF(ISTAT.NE.0)THEN
               CONTXT='Error getting FPKTIME keyword from '//IMNAME
               GO TO 999
            ENDIF
        ENDIF
C
C COMPUTE Y-POSITION
C
        LINE=LINE+1
        IF(LINE.GT.500)THEN
                CONTXT='Maximum of 500 image lines allowed'
                GO TO 999
        ENDIF
        CALL UHDGSD(IDIN,'YPOS',YPOS(LINE),ISTAT)
        IF(ISTAT.NE.0)THEN
          CONTXT='Error getting YPOS value from input '//IMNAME
          GO TO 999
        ENDIF
C
C READ IMAGE
C
        CALL UIMGID(IDIN,DTYPE,NAXIS,DIMEN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='Error reading input image '//IMNAME
                GO TO 999
        ENDIF
        IF(NAXIS.NE.1)THEN
                CONTXT='Input data must be 1-dimensional'
                GO TO 999
        ENDIF
        IF(LINE.GT.1)THEN
            IF(DIMEN(1).NE.NS)THEN
                CONTXT='All input data must have same length'
                GO TO 999
            ENDIF
            IF(YPOS(LINE).LT.YPOS(LINE-1))THEN
                CONTXT='Input images must be in increasing y-pos. order'
                GO TO 999
            ENDIF
        ENDIF
        NS=DIMEN(1)
        CALL UIGL1D(IDIN,IMAGE((LINE-1)*NS+1),ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='Error Reading input image '//IMNAME
                GO TO 999
        ENDIF
C
C GO GET NEXT LINE
C
        CALL UIMCLO(IDIN,ISTAT)
        GO TO 200
C
C--------------------------------------------------------------------
C SET UP TO DO USEFUL THINGS
C
500     NL=LINE
        IF(NL.EQ.0)THEN
              CONTXT='Error: No data found in input'//
     *                   ' template '//INPUT
              GO TO 999
        ENDIF
        NAPER=1
        PAIRED=.FALSE.
        DO 510 I=1,4
                IF(APER.EQ.PAIR(I))PAIRED=.TRUE.
510     CONTINUE
        IF(.NOT.PAIRED)APERPS='single'
        IF(PAIRED.AND.(APERPS.EQ.'both'))THEN
                APERPS='lower'
                NAPER=2
        ENDIF
C
C DEFAULT AREA
C
        IF(YLAST.EQ.0.0)YLAST=YPOS(NL)
        IF(YFIRST.EQ.0.0)YFIRST=YPOS(1)
        IF(YMID.EQ.0.0)YMID=(YFIRST+YLAST)/2.0
C
C CONVERT AREA TO IMAGE LINE AND SAMPLE COORDINATES
C
        CALL TABINV(YPOS,NL,YFIRST,RPOS)
        L1=RPOS
        CALL TABINV(YPOS,NL,YMID,RPOS)
        L2=RPOS
        CALL TABINV(YPOS,NL,YLAST,RPOS)
        L3=RPOS
        S1=(D1-FCHNL)*XSTEPS+1
        S2=(D2-FCHNL+1)*XSTEPS-1
C
C MAKE SURE THERE IN THE IMAGE
C
        IF(S1.LT.1)S1=1
        IF(S2.GT.NS)S2=NS
        IF(L1.LT.1)L1=1
        IF(L3.GT.NL)L3=NL
        IF((S1.GE.S2).OR.(L1.GE.L3).OR.
     *          ((NAPER.EQ.2).AND.((L2.LE.L1).OR.(L2.GE.L3))))THEN
                CONTXT='Invalid image region specified'
                GO TO 999
        ENDIF
C
C CHECK FOR VALID TEMPLATE WIDTHS
C
                IF((TWIDX.GE.(S2-S1-3)).OR.
     *          ((NAPER.EQ.1).AND.(TWIDY.GE.(L3-L1-3))).OR.
     *          ((NAPER.EQ.2).AND.(TWIDY.GE.(L2-L1-3))).OR.
     *          ((NAPER.EQ.2).AND.(TWIDY.GE.(L3-L2-3))))THEN
                        CONTXT='twidx or twidy too big for image size'
                        GO TO 999
                ENDIF
C-----------------------------------------------------------------------
C
C CREATE TABLE IF TBSTAT NE 'APPEND'
C
        IF(TBSTAT.NE.'append')THEN
C
                CALL UTTINN(OUTPUT,IDOUT,ISTATS(1))
                CALL UTPPTI(IDOUT,TBALLR,4,ISTATS(2))
                CALL UTPPTI(IDOUT,TBMXCL,15,ISTATS(3))
                CALL UTCDEF(IDOUT,COLNAM,CUNITS,CFORM,CTYPE,15,COLIDS,
     *                          ISTATS(4))
                CALL UTTCRE(IDOUT,ISTATS(5))
                CONTXT='Error creating output table'
                DO 600 I=1,5
                        IF(ISTATS(I).NE.0) GO TO 999
600             CONTINUE
                NROWS=0
           ELSE
C
C OPEN EXISTING TABLE
C
                CALL UTTOPN(OUTPUT,RDWRIT,IDOUT,ISTATS(1))
                CALL UTCFND(IDOUT,COLNAM,15,COLIDS,ISTATS(2))
                CALL UTPGTI(IDOUT,TBNROW,NROWS,ISTATS(3))
                CONTXT='Error opening output table for appending'
                DO 700 I=1,3
                        IF(ISTATS(I).NE.0) GO TO 999
700             CONTINUE
        ENDIF
C
C PROCESS EACH APERATURE
C
        DO 800 IAPER=1,NAPER
C
C INSERT REGION FOR APERTURE IN AREA
C
                AREA(1)=S1
                AREA(2)=S2
                AREA(3)=L1
                AREA(4)=L3
                IF((NAPER.EQ.2).AND.(IAPER.EQ.1))AREA(4)=L2
                IF(IAPER.EQ.2)THEN
                        AREA(3)=L2
                        APERPS='upper'
                ENDIF
                CALL TAINFO(IMAGE,NS,NL,TWIDX,TWIDY,AREA,
     *                          EDGES,CTROID,ECENT,CCENT,FLUX,ISTAT)
                IF(ISTAT.NE.0)THEN
                        CONTXT='Error processing field map'
                        GO TO 999
                ENDIF
C
C WRITE INFO TO THE OUTPUT TABLES
C
              DO 710 I=1,21
710                   ISTATS(I)=0
              NROWS=NROWS+1
              CALL UTRPTT(IDOUT,COLIDS(15),1,NROWS,TIME,ISTATS(1))
              CALL UTRPTT(IDOUT,COLIDS(1),1,NROWS,APER,ISTATS(2))
              CALL UTRPTT(IDOUT,COLIDS(2),1,NROWS,APERPS,ISTATS(3))
              IF(EDGES(1).NE.0.0) CALL UTRPTD(IDOUT,COLIDS(3),1,NROWS,
     *                  FCHNL+(EDGES(1)-1)/XSTEPS,ISTATS(4))
              IF(EDGES(2).NE.0.0) CALL UTRPTD(IDOUT,COLIDS(4),1,NROWS,
     *                  FCHNL+(EDGES(2)-1)/XSTEPS,ISTATS(5))
              IF(EDGES(3).NE.0.0) CALL UTRPTD(IDOUT,COLIDS(5),1,NROWS,
     *                  LOOKUP(YPOS,NL,EDGES(4)),ISTATS(6))
              IF(EDGES(4).NE.0.0) CALL UTRPTD(IDOUT,COLIDS(6),1,NROWS,
     *                  LOOKUP(YPOS,NL,EDGES(3)),ISTATS(7))
              IF(ECENT(1).NE.0.0) CALL UTRPTD(IDOUT,COLIDS(7),1,NROWS,
     *                  FCHNL+(ECENT(1)-1)/XSTEPS,ISTATS(8))
              IF(ECENT(2).NE.0.0) CALL UTRPTD(IDOUT,COLIDS(8),1,NROWS,
     *                  LOOKUP(YPOS,NL,ECENT(2)),ISTATS(9))
              IF(CTROID(1).NE.0.0) CALL UTRPTD(IDOUT,COLIDS(9),1,NROWS,
     *                  (CTROID(1)-1)/XSTEPS+FCHNL,ISTATS(10))
              IF(CTROID(2).NE.0.0) CALL UTRPTD(IDOUT,COLIDS(10),1,NROWS,
     *                  LOOKUP(YPOS,NL,CTROID(2)),ISTATS(11))
              IF(CCENT(1).NE.0.0) CALL UTRPTD(IDOUT,COLIDS(11),1,NROWS,
     *                  (CCENT(1)-1)/XSTEPS+FCHNL,ISTATS(12))
              IF(CCENT(2).NE.0.0) CALL UTRPTD(IDOUT,COLIDS(12),1,NROWS,
     *                  LOOKUP(YPOS,NL,CCENT(2)),ISTATS(13))
              IF((EDGES(1).NE.0.0) .AND. (EDGES(2).NE.0.0) .AND.
     *           (EDGES(3).NE.0.0) .AND. (EDGES(4).NE.0.0))THEN
                      A=(EDGES(2)-EDGES(1))/XSTEPS
                      A=A*(LOOKUP(YPOS,NL,EDGES(4))-
     *                         LOOKUP(YPOS,NL,EDGES(3)))
                      CALL UTRPTD(IDOUT,COLIDS(13),1,NROWS,A,ISTATS(14))
              ENDIF
              CALL UTRPTD(IDOUT,COLIDS(14),1,NROWS,FLUX,ISTATS(15))
                CALL UTHADT(IDOUT,'detector',DET,ISTATS(16))
                CALL UTHADT(IDOUT,'fgwa_id',FGWA,ISTATS(17))
              DO 720 I=1,17
                   IF(ISTATS(I).NE.0)THEN
                        CONTXT='Error writing to output table'
                        GO TO 999
                   ENDIF
720           CONTINUE
800     CONTINUE
        CALL UTTCLO(IDOUT,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='Error writing to output table'
                GO TO 999
        ENDIF
C
C DONE
C
        GO TO 1000
999     CALL UMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
1000    RETURN
        END
