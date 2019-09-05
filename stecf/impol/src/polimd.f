      SUBROUTINE POLIMD
C
C     This FORTRAN program produces a polarization vector 
C     plot from an image of the polarization and the
C     polarization position angle on a requested graphics 
C     device. Optionally the polarization and position 
C     angle errors may be supplied to overplot the error
C     extent. Various labelling options may also be 
C     exercized.
C
C     The linear polarization and polarization position angle 
C     files are read. The linear polarization and 
C     polarization position angle error files can also
C     be read. The binning of the data is required to plot on
C     the correct grid (such as for overlaying on an image).
C     A global shift to the position angles can be specified.
C
C     Written by  J. R. Walsh, ST-ECF, ESO (jwalsh@eso.org)  December 1999
C     Version 1.00
C
      IMPLICIT NONE

      INTEGER STAT
      INTEGER STAT1
      INTEGER STAT2
      INTEGER STAT3
      INTEGER STATP

      INTEGER IMDSCR1
      INTEGER IMDSCR2
      INTEGER IMDSCR3
      INTEGER IMDSCR4

      INTEGER NAXISI
      INTEGER DIMEN(7)
      INTEGER DTYPE

      INTEGER P1
      INTEGER P2
      INTEGER Q1
      INTEGER Q2
      INTEGER PE1
      INTEGER PE2
      INTEGER QE1
      INTEGER QE2
      INTEGER NELEM

      INTEGER LPOL
      INTEGER POPA
      INTEGER LPOLER
      INTEGER POPAER
 
      INTEGER XYBIN
      INTEGER XYSAM
  
      INTEGER GRDSCR

      REAL POSCAL
      REAL POLOW
      REAL POHIGH
      REAL PAOFF
      REAL VPXY(4)

      REAL MEMR(1)

      CHARACTER*32 INPOL
      CHARACTER*32 INPA
      CHARACTER*32 INPOER
      CHARACTER*32 INPAER

      CHARACTER*16 DEVICE

      CHARACTER*16 XLAB
      CHARACTER*16 YLAB
      CHARACTER*64 LABEL

      CHARACTER*132 OUTEXT

      LOGICAL LOER
      LOGICAL LLAB
      LOGICAL LERASE

      COMMON/MEM/MEMR
C
C     Get name of the linear polarization data set name 
C
100   CALL UCLGST('inpol',INPOL,STAT)
      CALL UIMOPN(INPOL,1,IMDSCR1,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,105) INPOL
105     FORMAT(' Error opening data file ',A32)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMGID(IMDSCR1,DTYPE,NAXISI,DIMEN,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,107) INPOL
107     FORMAT(' Error reading data file header ',A32)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      IF (NAXISI.NE.2) THEN
        WRITE(OUTEXT,109) 
109     FORMAT(' Data file is not 2-dimensional')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      P1=DIMEN(1)
      P2=DIMEN(2)
C
C     Allocate dynamic memory for the 2-D input image
C     of dimensions P1 by P2 (real number)
C
      NELEM=P1*P2
      CALL UDMGET(NELEM,6,LPOL,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,111) 
111     FORMAT(' Unable to assign memory for internal 2-D array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read the input image to LPOL
C
      CALL UIGS2R(IMDSCR1,1,P1,1,P2,MEMR(LPOL),STAT3)
      IF (STAT3.NE.0) THEN
        WRITE(OUTEXT,113) INPOL
113     FORMAT(' Error reading image into internal 2-D array ',A32)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get name of the polarization position angle data set 
C     name 
C
120   CALL UCLGST('inpopa',INPA,STAT)
      CALL UIMOPN(INPA,1,IMDSCR2,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,105) INPA
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMGID(IMDSCR2,DTYPE,NAXISI,DIMEN,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,107) INPA
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      IF (NAXISI.NE.2) THEN
        WRITE(OUTEXT,109) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      Q1=DIMEN(1)
      Q2=DIMEN(2)
C
C     Check input dimensions of polarization position angle
C     image same as polarization image
C
      IF (Q1.NE.P1.OR.Q2.NE.P2) THEN
        WRITE(OUTEXT,125) 
125     FORMAT(' Dimensions of input intensity and polarization
     : position angle images not equal')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the 2-D input image
C     of dimensions Q1 by Q2 (real number)
C
      NELEM=Q1*Q2
      CALL UDMGET(NELEM,6,POPA,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,111) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read the input image to POPA
C
      CALL UIGS2R(IMDSCR2,1,Q1,1,Q2,MEMR(POPA),STAT3)
      IF (STAT3.NE.0) THEN
        WRITE(OUTEXT,113) INPA
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Close the input polarization and position angle images
C
      CALL UIMCLO(IMDSCR1,STAT)
      CALL UIMCLO(IMDSCR2,STAT)
C
C     Get the switch to determine if imagers of the 
C     polarization error and position angle error are
C     to be input
C
      CALL UCLGSB('poi_err',LOER,STAT)
      IF (.NOT.LOER) THEN
C
C       Need to allocate dynamic memory for input arrays of
C       errors even if not used
C
        PE1=P1 
        PE2=P2
        QE1=Q1
        QE2=Q2
      ENDIF
      IF (LOER) THEN
C
C       Get name of the linear polarization error name 
C
200     CALL UCLGST('inpoer',INPOER,STAT)
        CALL UIMOPN(INPOER,1,IMDSCR3,STAT1)
        IF (STAT1.NE.0) THEN
          WRITE(OUTEXT,105) INPOER
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMGID(IMDSCR3,DTYPE,NAXISI,DIMEN,STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,107) INPOER
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        IF (NAXISI.NE.2) THEN
          WRITE(OUTEXT,109) 
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        PE1=DIMEN(1)
        PE2=DIMEN(2)
      ENDIF
C
C     Allocate dynamic memory for the 2-D input image
C     of dimensions PE1 by PE2 (real number)
C
      NELEM=PE1*PE2
      CALL UDMGET(NELEM,6,LPOLER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,111) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read the input image to LPOLER
C
      IF (LOER) THEN
        CALL UIGS2R(IMDSCR3,1,PE1,1,PE2,MEMR(LPOLER),STAT3)
        IF (STAT3.NE.0) THEN
          WRITE(OUTEXT,113) INPOL
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
      ENDIF
C
C     Get name of the polarization position angle error  
C     name 
C
220   IF (LOER) THEN
        CALL UCLGST('inpaer',INPAER,STAT)
        CALL UIMOPN(INPAER,1,IMDSCR4,STAT1)
        IF (STAT1.NE.0) THEN
          WRITE(OUTEXT,105) INPAER
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMGID(IMDSCR4,DTYPE,NAXISI,DIMEN,STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,107) INPAER
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        IF (NAXISI.NE.2) THEN
          WRITE(OUTEXT,109) 
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        QE1=DIMEN(1)
        QE2=DIMEN(2)
C
C       Check input dimensions of polarization position angle
C       error image same as polarization error image
C
        IF (QE1.NE.PE1.OR.QE2.NE.PE2) THEN
          WRITE(OUTEXT,125) 
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
      ENDIF
C
C     Allocate dynamic memory for the 2-D input image
C     of dimensions QE1 by QE2 (real number)
C
      NELEM=QE1*QE2
      CALL UDMGET(NELEM,6,POPAER,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,111) 
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read the input image to POPAER
C
      IF (LOER) THEN
        CALL UIGS2R(IMDSCR4,1,Q1,1,Q2,MEMR(POPAER),STAT3)
        IF (STAT3.NE.0) THEN
          WRITE(OUTEXT,113) INPAER
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
      ENDIF
C
C     Close the input polarization and position angle error
C     images
C
      IF (LOER) THEN
        CALL UIMCLO(IMDSCR3,STAT)
        CALL UIMCLO(IMDSCR4,STAT)
      ENDIF
C
C     Get the pixel binning factor (same in X and Y) used to 
C     form the polarization and position angle images
C
      CALL UCLGSI('xybin',XYBIN,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,311)
311     FORMAT(' Failed to get binning factor')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the scalar to multiply the polarization values
C     to produce useful vector lengths
C
      CALL UCLGSR('poscal',POSCAL,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,315)
315     FORMAT(' Failed to get scalar value')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the low cut-off in polarization below which not to
C     plot the vectors
C
      CALL UCLGSR('polow',POLOW,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,317)
317     FORMAT(' Failed to get cut-off polarization')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the upper cut-off in polarization above which not to
C     plot the vectors
C
      CALL UCLGSR('pohigh',POHIGH,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,319)
319     FORMAT(' Failed to get cut-off polarization')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the value of the offset angle to add to all
C     the position angles
C
      CALL UCLGSR('pacorr',PAOFF,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,321)
321     FORMAT(' Failed to get position angle offset')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the number of values to sample in X and Y when
C     plotting for clarity of presentation (expressed as
C     number of values to skip in parameter file)
C
      CALL UCLGSI('xysam',XYSAM,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,323)
323     FORMAT(' Failed to get spatial sampling step')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      XYSAM=XYSAM+1
C
C     Get the switch to determine if the vector map is
C     to be fully labelled
C
      CALL UCLGSB('polab',LLAB,STAT)
C
C     Get a label, an X-label and a Y-label for the plot 
C     if applicable
C
      IF (LLAB) THEN
        CALL UCLGST('title',LABEL,STAT)
        CALL UCLGST('xlabel',XLAB,STAT)
        CALL UCLGST('ylabel',YLAB,STAT)
      ENDIF
C
C     Get the name of the graphics device
C
      CALL UCLGST('device',DEVICE,STAT)
C
C     Get the 4 values of the viewport X minimum
C     and maximum, Y minumum and maximum values for
C     rescaling the plot
C
       CALL UCLGSR('left',VPXY(1),STAT2)
       CALL UCLGSR('right',VPXY(2),STAT2)
       CALL UCLGSR('bottom',VPXY(3),STAT2)
       CALL UCLGSR('top',VPXY(4),STAT2)

      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,333)
333     FORMAT(' Failed to get array of viewport limits')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the switch to determine whether to erase the 
C     graphics device
C
      CALL UCLGSB('erase',LERASE,STAT)
      IF (DEVICE(:7).EQ.'stdplot') THEN
        LERASE=.FALSE.
      ENDIF
C
C     Attempt to open the graphics station. If noerase
C     then open in append mode
C
400   IF (LERASE) THEN
        CALL UGOPEN(5,DEVICE,GRDSCR,STAT)
      ENDIF
      IF (.NOT.LERASE) THEN
        CALL UGOPEN(4,DEVICE,GRDSCR,STAT)
      ENDIF
      IF (STAT.NE.0) THEN
        CALL UMSPUT(' Failed to open graphics station',3,0,STAT)
        GO TO 990
      ENDIF
C
C     Clear the screen if required
C
      IF (LERASE) THEN
        CALL UGCLRS(GRDSCR,STAT)
        IF (STAT.NE.0) THEN
          CALL UMSPUT(' Error clearing graphics station',3,0,STAT)
          GO TO 990
        ENDIF
      ENDIF
C
C     Call the subroutine to plot the polarization vector map 
C     on the graphics station
C
      CALL PLOTPOPA(P1,P2,MEMR(LPOL),MEMR(POPA),LOER,
     :              MEMR(LPOLER),MEMR(POPAER),GRDSCR,
     :              XYBIN,POSCAL,POLOW,POHIGH,PAOFF,
     :              XYSAM,LLAB,XLAB,YLAB,LABEL,VPXY,STATP)
C
C     Free the dynamic memory allocated which is no longer 
C     required
C
      CALL UDMFRE(LPOL,6,STAT)
      CALL UDMFRE(POPA,6,STAT)
      CALL UDMFRE(LPOLER,6,STAT)
      CALL UDMFRE(POPAER,6,STAT)
C
C     Close the graphics device
C
      CALL UGCLOS(GRDSCR,STAT)

      IF (STATP.EQ.0) THEN
        GO TO 999
      ENDIF

990   WRITE(OUTEXT,991)
991   FORMAT(' Program failed. No vector plot')
      CALL UMSPUT(OUTEXT,1,0,STAT)

999   END

      SUBROUTINE PLOTPOPA(N1,N2,POL,PA,LOER,POLER,PAER,
     :                    GRDSCR,XYBIN,POSCAL,POLOW,POHIGH,
     :                    PAOFF,XYSAM,LLAB,XLAB,YLAB,LABEL,
     :                    VPXY,STATO)
C
C     This subroutine plots using the F77VOS graphics routines
C     the polarization vector field given by the array of
C     polarization POL and position angle PA. If LOER is TRUE
C     then the vectors formed with the polarization error and 
C     the position angle error included are overplotted in 
C     dashed lines. If the data were binned (XYBIN) to form 
C     the polarization then the original grid of the data is 
C     reproduced. The polarization vector length is scaled 
C     by POSCAL. Vectors are plotted for all polarization
C     values greater than POLOW. A global offset to the 
C     position angle is given by PAOFF. Every XYSAM 
C     polarization vector can be plotted to reduce crowding.
C     Full labelling is specified by POLAB TRUE
C
C     Input parameters:
C
C       N1 X dimension of input arrays
C       N2 Y dimension of input arrays
C       POL(N1,N2) input array of linear polarization (%)
C       PA(N1,N2) input array of position angle (degrees)
C       LOER logical flag to indicate whether to plot error vectors
C       POLER(N1,N2) input array of linear polarization error (%)
C       PAER(N1,N2) input array of position angle error (degrees)
C       GRDSCR graphics device descriptor
C       XYBIN integer binning factor for X and Y used for polarization images
C       POSCAL scalar to apply to polarization values to plot as pixel lengths
C       POLOW low limit of polarization to plot
C       POHIGH high limit of polarization to plot
C       PAOFF additive correction to apply to PA
C       XYSAM factor to determine plotting every XYSAM vector (in X and Y)
C       LLAB logical flag to indicate whether to label and annotate plot
C       XLAB label for the X axis
C       YLAB label for the Y axis
C       LABEL text to label the plot
C       VWXY(4) array of the viewport limits X min and max, Y min and max
C       STATO status return - returned non-zero if plotting problem occurred
C
      IMPLICIT NONE
      INTEGER N1
      INTEGER N2
      INTEGER GRDSCR
      INTEGER XYBIN
      INTEGER XYSAM
      INTEGER STATO

      REAL POL(N1,N2)
      REAL PA(N1,N2)
      REAL POLER(N1,N2)
      REAL PAER(N1,N2)
      REAL POSCAL
      REAL POLOW
      REAL POHIGH
      REAL PAOFF
      REAL VPXY(4)

      CHARACTER*16 XLAB
      CHARACTER*16 YLAB
      CHARACTER*64 LABEL

      LOGICAL LOER
      LOGICAL LLAB
C
C     Local variables
C
      INTEGER I,J
      INTEGER STAT
      INTEGER STAT2
      INTEGER NPT
      INTEGER LINE
      INTEGER GPLTYP

      REAL TYPSIZ
      REAL RESIZ
      REAL VPMINX
      REAL VPMAXX
      REAL VPMINY
      REAL VPMAXY
      REAL WMINX
      REAL WMAXX
      REAL WMINY
      REAL WMAXY
      REAL ANG
      REAL XLFRAC
      REAL YLFRAC

      REAL SINRD
      REAL COSRD

      REAL POSX(2)
      REAL POSY(2)

      CHARACTER*3 PCLAB
      CHARACTER*80 OUTEXT

      STATO=0
C
C     Set the viewport limits of the graphics device
C
      VPMINX=VPXY(1)
      VPMINY=VPXY(3)
      VPMAXX=VPXY(2)
      VPMAXY=VPXY(4)
      CALL UGPVPT(GRDSCR,VPMINX,VPMAXX,VPMINY,VPMAXY,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,21)
21      FORMAT(' Error setting viewport limits of graphics window')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        STATO=1
        GO TO 990
      ENDIF
C
C     Set the World Coordinates of the data window
C
      WMINX=1.0
      WMAXX=REAL(N1*XYBIN)
      WMINY=1.0
      WMAXY=REAL(N2*XYBIN)
      CALL UGPWND(GRDSCR,WMINX,WMAXX,WMINY,WMAXY,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,31)
31      FORMAT(' Error setting World Coordinates of graphics window')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        STATO=1
        GO TO 990
      ENDIF
C
C     Get the character height and reset in proportion to the 
C     viewport size
C
      CALL UGGGPR(GRDSCR,39,TYPSIZ,STAT)
      RESIZ=ABS(VPXY(2)-VPXY(1) + VPXY(4)-VPXY(3))
      TYPSIZ=RESIZ*TYPSIZ
      CALL UGPGPR(GRDSCR,39,TYPSIZ,STAT)
C
C     Plot the axis box and label
C
      IF (LLAB) THEN
        CALL UGLABL(GRDSCR,LABEL,XLAB,YLAB,STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,41)
41        FORMAT(' Error displaying and labelling plot frame')
          CALL UMSPUT(OUTEXT,3,0,STAT)
          STATO=1
          GO TO 990
        ENDIF
      ENDIF
C
C     Plot the polarization vectors on the unbinned grid at
C     the required sampling with a solid line
C
      NPT=2
      LINE=1 ! Solid line
      CALL UGPGPI(GRDSCR,GPLTYP,LINE,STAT2)
      DO I=XYSAM,N1-XYSAM,XYSAM
        DO J=XYSAM,N2-XYSAM,XYSAM
          IF (POL(I,J).GT.POLOW.AND.POL(I,J).LT.POHIGH) THEN
            ANG=-PA(I,J) + PAOFF 
            POSX(1)=REAL(I*XYBIN) - POSCAL*0.5*POL(I,J)*SINRD(ANG)
            POSY(1)=REAL(J*XYBIN) - POSCAL*0.5*POL(I,J)*COSRD(ANG)
            POSX(2)=REAL(I*XYBIN) + POSCAL*0.5*POL(I,J)*SINRD(ANG)
            POSY(2)=REAL(J*XYBIN) + POSCAL*0.5*POL(I,J)*COSRD(ANG)
            CALL UGLINE(GRDSCR,POSX,POSY,NPT,STAT2)
          ENDIF
        ENDDO
      ENDDO
C
C     If error plotting required plot the vector with the
C     polarization - error at position angle - error and
C     the vector with the polarization + error at position 
C     angle + error
C  
      IF (LOER) THEN
        LINE=2 ! Dashed line
        CALL UGPGPI(GRDSCR,GPLTYP,LINE,STAT2)
        DO I=XYSAM,N1-XYSAM,XYSAM
          DO J=XYSAM,N2-XYSAM,XYSAM
            IF (POL(I,J).GT.POLOW.AND.POL(I,J).LT.POHIGH) THEN
              ANG=-PA(I,J) - PAER(I,J) + PAOFF
              POSX(1)=REAL(I*XYBIN) - 
     :               POSCAL*0.5*ABS(POL(I,J)-POLER(I,J))*SINRD(ANG)
              POSY(1)=REAL(J*XYBIN) - 
     :               POSCAL*0.5*ABS(POL(I,J)-POLER(I,J))*COSRD(ANG)
              POSX(2)=REAL(I*XYBIN) + 
     :                POSCAL*0.5*ABS(POL(I,J)-POLER(I,J))*SINRD(ANG)
              POSY(2)=REAL(J*XYBIN) + 
     :                POSCAL*0.5*ABS(POL(I,J)-POLER(I,J))*COSRD(ANG)
              CALL UGLINE(GRDSCR,POSX,POSY,NPT,STAT2)
            ENDIF
          ENDDO
        ENDDO
        DO I=XYSAM,N1-XYSAM,XYSAM
          DO J=XYSAM,N2-XYSAM,XYSAM
            IF (POL(I,J).GT.POLOW.AND.POL(I,J).LT.POHIGH) THEN
              ANG=-PA(I,J) + PAER(I,J) + PAOFF
              POSX(1)=REAL(I*XYBIN) - 
     :               POSCAL*0.5*ABS(POL(I,J)+POLER(I,J))*SINRD(ANG)
              POSY(1)=REAL(J*XYBIN) - 
     :               POSCAL*0.5*ABS(POL(I,J)+POLER(I,J))*COSRD(ANG)
              POSX(2)=REAL(I*XYBIN) + 
     :                POSCAL*0.5*ABS(POL(I,J)+POLER(I,J))*SINRD(ANG)
              POSY(2)=REAL(J*XYBIN) + 
     :                POSCAL*0.5*ABS(POL(I,J)+POLER(I,J))*COSRD(ANG)
              CALL UGLINE(GRDSCR,POSX,POSY,NPT,STAT2)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
C     Plot a marker to indicate the polarization vector length
C     as 50% in the lower right corner
C
      LINE=1 ! Solid line
      PCLAB='50%'
      CALL UGPGPI(GRDSCR,GPLTYP,LINE,STAT2)
      XLFRAC=0.85
      YLFRAC=0.15
      POSX(1)=XLFRAC*REAL(N1*XYBIN) - 50.0*0.50*POSCAL
      POSX(2)=XLFRAC*REAL(N1*XYBIN) + 50.0*0.50*POSCAL
      POSY(1)=YLFRAC*REAL(N2*XYBIN)
      POSY(2)=YLFRAC*REAL(N2*XYBIN) 
      CALL UGLINE(GRDSCR,POSX,POSY,NPT,STAT2)
      POSX(1)=XLFRAC*REAL(N1*XYBIN) - 50.0*0.50*POSCAL
      POSY(1)=1.04*YLFRAC*REAL(N2*XYBIN)
      CALL UGTEXT(GRDSCR,POSX(1),POSY(1),PCLAB,STAT2)

990   CONTINUE

999   END

