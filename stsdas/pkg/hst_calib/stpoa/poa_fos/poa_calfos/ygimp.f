C 	Copyright restrictions apply - see stsdas$copyright.stsdas , also ST-ECF copyrights
C 
      SUBROUTINE YGIMP(XSCALE,YSCALE,XOFF,YOFF,NSPEC,GRNDMD,FRAME,ISTAT)
*
*  Module number:
*
*  Module name: YGIMP
*
*  Keyphrase:
*  ----------
*       Perform FOS GIMP correction
*
*  Description:
*  ------------
*       This routine calculates and applies the GIMP correction.
*       Uses spherical harmonics model of earth's magnetic field.
*	The output is an offset in the fos x-direction.
*
*  FORTRAN name: YGIMP.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments

*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 91	S. Hulbert      Designed and coded
*     1.1       Jun 91	S. Hulbert      added y-offset     
*     1.2       Aug 91	S. Hulbert      change sign of y-offset     
*     1.3       Jul 92  D. Bazell       Fix bug in dyn. mem. deallocation
*     1.4	Apr 93	H. Bushouse	Declare local variables
*     1.5       Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*     /////////////////////////////////////////////////////////////////////
*            Start of Post Operational Archive versions (poa_calfos)
*     /////////////////////////////////////////////////////////////////////
*     1.0       Mar 00  M. Rosa         Streamline to new POA concept
*               May 00  M. Rosa         Report magnetic info (shell, alng)
*               Jun 00  M. Rosa         Updated hardwired parameters
*     1.1       Dec 00  A. Alexov       Added PVX, PVY, PVZ to POA global
*-------------------------------------------------------------------------------
*
* INPUTS:
*	xscale - scale factor for x-offsets as a function of detector
*	yscale - scale factor for y-offsets as a function of detector
*	nspec - number of spectra in a frame
*
* INPUT/OUTPUT:
*
* OUTPUT:
*	xoff - array of offsets in fos x-direction
*	yoff - array of offsets in fos y-direction
*       istat - error status
*
*----------------------------------------------------------------------------
       IMPLICIT NONE 
       INTEGER NSPEC, ISTAT, ISTATS(10)
       REAL XSCALE, YSCALE, XOFF(NSPEC), YOFF(NSPEC)
C------------------------------------------------------------------------------
C Get IRAF MEM common into main program.
C
      LOGICAL          MEMB(1)
      INTEGER*2        MEMS(1)
      INTEGER*4        MEMI(1)
      INTEGER*4        MEML(1)
      REAL             MEMR(1)
      DOUBLE PRECISION MEMD(1)
      COMPLEX          MEMX(1)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
C------------------------------------------------------------------------------
      INTEGER TYDOUB
      PARAMETER (TYDOUB = 7)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.1415926535898)
C
C Common block containing confiquration parameters
C
        CHARACTER*18 GRNDMD
        CHARACTER*5 DET
        CHARACTER*3 FGWAID,APERID,YTYPE(3)
        CHARACTER*1 POLID
        INTEGER FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LIVETM
        LOGICAL HEADER,TRAILR,DEFDDT,KYDPLY
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
        COMMON /CONFG2/FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LIVETM,HEADER,TRAILR,
     *          DEFDDT
        INTEGER DEADTM
        COMMON /CONFG5/DEADTM
        INTEGER FRAME, II, COUNTER

C MR
        REAL*8 PMIDTIM,RAV1,DECV1,PAV3,HSTHORB 
c the POA orbital computation yields new group parameters 
      REAL*8 MIDTIMP,POAXP,POAYP,POAZP,VXP,VYP,VZP,BNP,BEP,BDP,
     a       BV1P,BV2P,BV3P,BDXP,BDYP,BDZP,YGMPXSCL,YGMPYSCL,
     b       YOFFXP,YOFFYP,YYBASE0,YYBSXSCL,YMEANTMP,YTMPXSCL,YAPGRTX0,
     c       YXCEN,GMSTP,GLNGP,GLATP,MLNGP,MLATP,ALNGP,ALATP,LSHP,
     d       PVX,PVY,PVZ
      COMMON /POA_P/ MIDTIMP,POAXP,POAYP,POAZP,VXP,VYP,VZP,BNP,BEP,BDP,
     *       BV1P,BV2P,BV3P,BDXP,BDYP,BDZP,YGMPXSCL,YGMPYSCL,
     *       YOFFXP,YOFFYP,YYBASE0,YYBSXSCL,YMEANTMP,YTMPXSCL,YAPGRTX0,
     *       YXCEN,GMSTP,GLNGP,GLATP,MLNGP,MLATP,ALNGP,ALATP,LSHP,
     *       PVX,PVY,PVZ
c the POA header keys get placed into an ascii txt output file
      CHARACTER*64 POA_TXT   
      COMMON /POA_T/ POA_TXT
      INTEGER GLOBAL_COUNT
      COMMON /GCOUNT/ GLOBAL_COUNT

C Common block containing input/output file descriptions
C
C       IDS - file id numbers
C       GCOUNT - group counts
C       NAXIS - naxis
C       NAXIS1 - first dimensions
C       NAXIS2 - second dimensions
C       FILL - Fill values
C
        INTEGER IDS(30),NAXIS(30),NAXIS1(30),NAXIS2(30),GCOUNT(30)
        REAL FILL(30)
        COMMON /IOCOM/IDS,GCOUNT,NAXIS,NAXIS1,NAXIS2,FILL
C
C LOCAL VARIABLES ------------------------------------------------------
        CHARACTER*80 CONTXT
        DOUBLE PRECISION FPKTIM
        DOUBLE PRECISION EXPYST, EXPRDT
C	REAL BX, BY, BEFFX, BEFFY, THETA
        INTEGER MIDTIM   !, XPOS, YPOS, ZPOS, LAT, LNG, ST, BCOMPS
	INTEGER I, ID, OFFSET_ARRAY_PIX, JJ, STR_LEN
        REAL*8 POAXP_KM,POAYP_KM,POAZP_KM,VXP_KM,VYP_KM,VZP_KM
C
C first element for amber detector
C second element for blue detector
C angle b: rotation about y-axis
C angle c: rotation about x-axis
C angle d: rotation about z-axis
C angle e: EXB electronoptic drift angle
C xfactor: motion (diodes/gauss)   (from ccs7)
C yfactor: motion (ybase units/gauss)  (from ccs7)
C
c        DOUBLE PRECISION ANGLEB(2), ANGLEC(2), ANGLED(2), ANGLEE(2) 
C        DOUBLE PRECISION XFACTOR(2), YFACTOR(2)
c        DATA ANGLEB / -8.0,  8.0/
c        DATA ANGLEC /-23.0,-23.0/
c        DATA ANGLED /135.0,135.0/
c        DATA ANGLEE / 17.6, 17.6/
C        DATA XFACTOR / 2.95,  0.7/
C        DATA YFACTOR /189.0, 45.0/
C
C Common block containing orbital/positional parameters
C
        DOUBLE PRECISION EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI
        DOUBLE PRECISION ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3
        DOUBLE PRECISION FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER
        DOUBLE PRECISION RASCASCN,SINEINCL,SEMILREC
        DOUBLE PRECISION PSANGLV3,RTASCNV1,DECLNV1
        COMMON /ORBPAR/ EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI,
     $  ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3,
     $  FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER,
     $  RASCASCN,SINEINCL,SEMILREC,
     $  PSANGLV3,RTASCNV1,DECLNV1
C-----------------------------------------------------------------------
C
C open the POA header text file
        OPEN(29,FILE=POA_TXT,STATUS='OLD',ACCESS='APPEND')
C
C get an index based on the detector for use later
        IF (DET .EQ. 'AMBER') THEN
            ID = 1
        ELSE IF (DET .EQ. 'BLUE ') THEN
            ID = 2
        ELSE
            CONTXT = 'Invalid detector encountered during deGIMPing'
            GO TO 999
        ENDIF
CCC       WRITE(CONTXT,97) DET,ID
CCC 97    FORMAT(' POA ygimp: det,id: ',A5,2X,I4)
CCC       CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C get HSTORBIT from the shp file
        CALL UHDGSD(IDS(6),'HSTHORB',HSTHORB,ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR - HSTHORB not there'
            GO TO 999
        ENDIF
        RAV1 = RTASCNV1
        DECV1 = DECLNV1
        PAV3 = PSANGLV3
C get first packet time from the d0h file
        CALL UHDGSD(IDS(1),'FPKTTIME',FPKTIM,ISTAT)
C
C determine exposure time for each ystep and readout in days
C
        EXPYST = (LIVETM+DEADTM)*7.8125e-6*OVERSN*NXSTEP*
     $                NPAT*INTS/24./3600.
        EXPRDT = EXPYST * NSPEC
C
C compute observation time midpoint of each ystep/slice
        CALL UDMGET (NSPEC, TYDOUB, MIDTIM, ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR allocating dynamic memory'
                GO TO 999
        ENDIF

C
C Don says: "if npat greater then 1 then all ysteps and slices
C for a readout will be at the midpoint of the readout. If npat=1 then
C the readout will be divided into YSTEPS*SLICES intervals"
        DO 100 I = 1, NSPEC
c                WRITE(CONTXT,9876) NSPEC,I
c 9876           FORMAT('loop NSPEC,I: ',I6,1X,I6)
            IF (NPAT .GT. 1) THEN
                MEMD(MIDTIM+I-1) = FPKTIM - EXPRDT/2.
            ELSE
                MEMD(MIDTIM+I-1) = FPKTIM - EXPRDT + 
     $                        (2.*I-1.)/2. * EXPYST
            ENDIF
c*mr*  compute the poagimp parameters for this particular midpoint      
            PMIDTIM = MEMD(MIDTIM+I-1)
            CALL  POAGIMP(PMIDTIM,RAV1,DECV1,PAV3,ID,ISTAT)
            IF(ISTAT.NE.0)THEN
                GO TO 999
            ENDIF
c*mr*  determine offsets depending on detector (A,B was tested already) 
            CALL  POAOFFX(ID,XOFF(I),YOFF(I),ISTAT)
            IF(ISTAT.NE.0)THEN
                GO TO 999
            ENDIF
            GLOBAL_COUNT=GLOBAL_COUNT+1

C the POA value must be written here, since the values change
C per NSPEC loop;  write to POA header ascii file and STDOUT
       WRITE(CONTXT,98) GRNDMD,I,XOFF(I)
 98    FORMAT(' POA ygimp: mode,grp/ystp,xoff: ',A18,2X,I4,2X,F8.3)
       CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C convert units from m -> km
         POAXP_KM = POAXP / 1000.0
         POAYP_KM = POAYP / 1000.0
         POAZP_KM = POAZP / 1000.0
C convert units from m/sec -> km/sec
         VXP_KM = VXP / 1000.0
         VYP_KM = VYP / 1000.0
         VZP_KM = VZP / 1000.0
         IF ( FRAME .NE. 1 ) THEN
            COUNTER = FRAME
         ELSE
            COUNTER = I
         ENDIF
         II=GLOBAL_COUNT
4        DO 10 JJ=1,64
10              IF( POA_TXT(JJ:JJ) .EQ. ' ') GO TO 20
20       STR_LEN=JJ

         WRITE(29,130) GRNDMD(1:5),"=",II,", ","MIDTIMP=",MIDTIMP
 130     FORMAT(A5,A1,I4,A2,A8,f17.9)
         WRITE(29,131) GRNDMD(1:5),"=",II,", ","POAXP=",POAXP_KM
 131     FORMAT(A5,A1,I4,A2,A6,f11.3)
         WRITE(29,132) GRNDMD(1:5),"=",II,", ","POAYP=",POAYP_KM
 132     FORMAT(A5,A1,I4,A2,A6,f11.3)
         WRITE(29,133) GRNDMD(1:5),"=",II,", ","POAZP=",POAZP_KM
 133     FORMAT(A5,A1,I4,A2,A6,f11.3)
         WRITE(29,134) GRNDMD(1:5),"=",II,", ","VXP=",VXP_KM
 134     FORMAT(A5,A1,I4,A2,A4,f10.5)
         WRITE(29,135) GRNDMD(1:5),"=",II,", ","VYP=",VYP_KM
 135     FORMAT(A5,A1,I4,A2,A4,f10.5)
         WRITE(29,136) GRNDMD(1:5),"=",II,", ","VZP=",VZP_KM
 136     FORMAT(A5,A1,I4,A2,A4,f10.5)
         WRITE(29,137) GRNDMD(1:5),"=",II,", ","BNP=",BNP
 137     FORMAT(A5,A1,I4,A2,A4,f13.8)
         WRITE(29,138) GRNDMD(1:5),"=",II,", ","BEP=",BEP
 138     FORMAT(A5,A1,I4,A2,A4,f13.8)
         WRITE(29,139) GRNDMD(1:5),"=",II,", ","BDP=",BDP
 139     FORMAT(A5,A1,I4,A2,A4,f13.8)
         WRITE(29,140) GRNDMD(1:5),"=",II,", ","BV1P=",BV1P
 140     FORMAT(A5,A1,I4,A2,A5,f13.8)
         WRITE(29,141) GRNDMD(1:5),"=",II,", ","BV2P=",BV2P
 141     FORMAT(A5,A1,I4,A2,A5,f13.8)
         WRITE(29,142) GRNDMD(1:5),"=",II,", ","BV3P=",BV3P
 142     FORMAT(A5,A1,I4,A2,A5,f13.8)
         WRITE(29,143) GRNDMD(1:5),"=",II,", ","BDXP=",BDXP
 143     FORMAT(A5,A1,I4,A2,A5,f13.8)
         WRITE(29,144) GRNDMD(1:5),"=",II,", ","BDYP=",BDYP
 144     FORMAT(A5,A1,I4,A2,A5,f13.8)
         WRITE(29,145) GRNDMD(1:5),"=",II,", ","BDZP=",BDZP
 145     FORMAT(A5,A1,I4,A2,A5,f13.8)
         WRITE(29,146) GRNDMD(1:5),"=",II,", ","YGMPXSCL=",YGMPXSCL
 146     FORMAT(A5,A1,I4,A2,A9,f10.4)
         WRITE(29,147) GRNDMD(1:5),"=",II,", ","YGMPYSCL=",YGMPYSCL
 147     FORMAT(A5,A1,I4,A2,A9,f10.4)
         WRITE(29,148) GRNDMD(1:5),"=",II,", ","YOFFXP=",YOFFXP
 148     FORMAT(A5,A1,I4,A2,A7,f13.8)
         WRITE(29,149) GRNDMD(1:5),"=",II,", ","YOFFYP=",YOFFYP
 149     FORMAT(A5,A1,I4,A2,A7,f13.8)
         WRITE(29,150) GRNDMD(1:5),"=",II,", ","YYBASE0=",YYBASE0
 150     FORMAT(A5,A1,I4,A2,A8,f8.0)
         WRITE(29,151) GRNDMD(1:5),"=",II,", ","YYBSXSCL=",YYBSXSCL
 151     FORMAT(A5,A1,I4,A2,A9,f6.2)
         WRITE(29,152) GRNDMD(1:5),"=",II,", ","YMEANTMP=",YMEANTMP
 152     FORMAT(A5,A1,I4,A2,A9,f13.8)
         WRITE(29,153) GRNDMD(1:5),"=",II,", ","YTMPXSCL=",YTMPXSCL
 153     FORMAT(A5,A1,I4,A2,A9,f7.3)
         WRITE(29,154) GRNDMD(1:5),"=",II,", ","YAPGRTX0=",YAPGRTX0
 154     FORMAT(A5,A1,I4,A2,A9,f8.4)
         WRITE(29,155) GRNDMD(1:5),"=",II,", ","GMSTP=",GMSTP
 155     FORMAT(A5,A1,I4,A2,A6,f18.13)
         WRITE(29,156) GRNDMD(1:5),"=",II,", ","GLNGP=",GLNGP
 156     FORMAT(A5,A1,I4,A2,A6,f10.5)
         WRITE(29,157) GRNDMD(1:5),"=",II,", ","GLATP=",GLATP
 157     FORMAT(A5,A1,I4,A2,A6,f10.5)
         WRITE(29,158) GRNDMD(1:5),"=",II,", ","MLNGP=",MLNGP
 158     FORMAT(A5,A1,I4,A2,A6,f11.5)
         WRITE(29,159) GRNDMD(1:5),"=",II,", ","MLATP=",MLATP
 159     FORMAT(A5,A1,I4,A2,A6,f10.5)
         WRITE(29,170) GRNDMD(1:5),"=",II,", ","ALNGP=",ALNGP
 170     FORMAT(A5,A1,I4,A2,A6,f11.5)
         WRITE(29,171) GRNDMD(1:5),"=",II,", ","ALATP=",ALATP
 171     FORMAT(A5,A1,I4,A2,A6,f10.5)
         WRITE(29,172) GRNDMD(1:5),"=",II,", ","LSHP=",LSHP
 172     FORMAT(A5,A1,I4,A2,A5,f10.5)
         WRITE(29,174) GRNDMD(1:5),"=",II,", ","PVX=",PVX
 174     FORMAT(A5,A1,I4,A2,A4,f10.5)
         WRITE(29,175) GRNDMD(1:5),"=",II,", ","PVY=",PVY
 175     FORMAT(A5,A1,I4,A2,A4,f10.5)
         WRITE(29,176) GRNDMD(1:5),"=",II,", ","PVZ=",PVZ
 176     FORMAT(A5,A1,I4,A2,A4,f10.5)
         WRITE(29,129) GRNDMD(1:5),"=",II,", ","OFF_DIOD=",XOFF(I)
 129     FORMAT(A5,A1,I4,A2,A9,f8.3) 
         OFFSET_ARRAY_PIX = NINT(NXSTEP*XOFF(I)/4.)
         WRITE(29,173) GRNDMD(1:5),II,OFFSET_ARRAY_PIX
 173     FORMAT(A5,'=',I4,', ','OFF_PIX=',I4)
                   
100     CONTINUE
C
C compute spacecraft position at midpoint 
C
c        CALL UDMGET (NSPEC, TYDOUB, XPOS, ISTATS(1))
c        CALL UDMGET (NSPEC, TYDOUB, YPOS, ISTATS(2))
c        CALL UDMGET (NSPEC, TYDOUB, ZPOS, ISTATS(3))
c        CALL UDMGET (NSPEC, TYDOUB, LNG, ISTATS(4))
c        CALL UDMGET (NSPEC, TYDOUB, LAT, ISTATS(5))
c        CALL UDMGET (NSPEC, TYDOUB, ST, ISTATS(6))
c        DO 150 I = 1, 6
c            IF(ISTATS(I).NE.0)THEN
c                CONTXT='ERROR allocating dynamic memory'
c                GO TO 999
c            ENDIF
c150        CONTINUE
c
c        CALL HSTPOS(MEMD(MIDTIM),NSPEC,MEMD(XPOS),MEMD(YPOS),
c     $                MEMD(ZPOS),MEMD(LNG),MEMD(LAT),MEMD(ST),ISTAT)
C
C compute earth's magnetic field at spacecraft positions
C BCOMPS is a (3,nspec) array
C BCOMPS(1,I)=V2, BCOMPS(2,I)=V3, BCOMPS(3,I)=V1 for the Ith spectrum
C
c        CALL UDMGET (3*NSPEC, TYDOUB, BCOMPS, ISTAT)
c        IF(ISTAT.NE.0)THEN
c            CONTXT='ERROR allocating dynamic memory'
c            GO TO 999
c        ENDIF
c        CALL YMAGFD(MEMD(MIDTIM),MEMD(XPOS),MEMD(YPOS),MEMD(ZPOS),
c     $          MEMD(LNG),MEMD(LAT),MEMD(ST),NSPEC,MEMD(BCOMPS),ISTAT)
C convert from v1/v2/v3 to fos coordinates
C convert anglee to radians for use later
c        THETA = ANGLEE(ID) * PI/180.
C
c        CONTXT='here'
c        CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
c
c        DO 200 I = 1, NSPEC
C
C convert to FOS coordinates
c                WRITE(CONTXT,9876) NSPEC,I
c 9876           FORMAT('loop NSPEC,I: ',I6,1X,I6)
c        CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
c            CALL ROTATE (MEMD(BCOMPS+(I-1)*3),-ANGLED(ID),3)
c            CALL ROTATE (MEMD(BCOMPS+(I-1)*3),ANGLEC(ID),1)
c            CALL ROTATE (MEMD(BCOMPS+(I-1)*3),ANGLEB(ID),2)
C
C rotate for EXB electron optical drift
c            BX = MEMD(BCOMPS+(I-1)*3)
c            BY = MEMD(BCOMPS+(I-1)*3+1)
c            BEFFX = COS(THETA) * BX - SIN(THETA) * BY
c            BEFFY = COS(THETA) * BY + SIN(THETA) * BX
c            XOFF(I) = XSCALE * BEFFX
C
C calculate y offset (flip sign because somewhere we goofed)
c            YOFF(I) = -YSCALE * BEFFY
c200     CONTINUE
C  - End of old ground GIMP correction
c        CONTXT='here in ygimp after old'
c        CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
C
C AA 21/10/99 - add flight software and poa predictors here
C 
C check to make sure NSPEC = 1
c        IF(NSPEC.NE.1)THEN
c            CONTXT='ERROR - FSW code currently works on single spectra'
c            GO TO 999
c        ENDIF
C
C call the FSW GIMP code      
c        CALL  FSWGIMP (FMIDTIM,RAV1,DECV1,PAV3,HSTHORB,ID,ISTAT)

C call the POA GIMP code      
c        PMIDTIM = MEMD(MIDTIM)
c        RAV1 = RTASCNV1
c        DECV1 = DECLNV1
c        PAV3 = PSANGLV3
c        CALL  POAGIMP (PMIDTIM,RAV1,DECV1,PAV3,ID,ISTAT)
C
C free memory
C
        CALL UDMFRE (MIDTIM, TYDOUB, ISTATS(1))
c        CALL UDMFRE (XPOS, TYDOUB, ISTATS(2))
c        CALL UDMFRE (YPOS, TYDOUB, ISTATS(3))
c        CALL UDMFRE (ZPOS, TYDOUB, ISTATS(4))
c        CALL UDMFRE (LNG, TYDOUB, ISTATS(5))
c        CALL UDMFRE (LAT, TYDOUB, ISTATS(6))
c        CALL UDMFRE (ST, TYDOUB, ISTATS(7))
          DO 160 I = 1, 1
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR deallocating dynamic memory'
                GO TO 999
            ENDIF
160        CONTINUE

C close the POA header text file
         CLOSE(29)
C
C free buffer
c        CALL UDMFRE (BCOMPS, TYDOUB, ISTAT)
C
        ISTAT=0
        GOTO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
