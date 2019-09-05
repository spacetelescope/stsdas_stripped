C 	Copyright restrictions apply - see stsdas$copyright.stsdas , also ST-ECF copyrights
C 
        SUBROUTINE POAOFFX(ID,PXOFF,PYOFF,ISTAT)
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  ------------------------------------------------------------------------
c*
*  Description:
*  ------------
*       This routine calculates the offsets in dispersion direction
*       stemming from GIMP, GIMP-onboard fix, YBASE updates and FGWA
*       temperature flexure.
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1.0        Jun 00   M. Rosa         Designed and coded
*               28 Jun 00   M. Rosa         Updated hardwired parameters
*       1.1        Dec 00   A. Alexov       Added PVX, PVY, PVZ to POA global
*                  Feb 01   A. Alexov       Fix bug with L15 typo in FGWLIST
*-------------------------------------------------------------------------------
*
* INPUTS:
*	id - detector ID (1=AMBER, 2=BLUE)
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

      INTEGER ID,ISTAT
      REAL    PXOFF,PYOFF
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C MR
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

c
C Common block containing input/output file descriptions
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
        LOGICAL YFGIMPEN 
        CHARACTER*3 FGWAID,APERID,FGWLIST(10)
        CHARACTER*18 GRNDMD
        CHARACTER*80 CONTXT
        DOUBLE PRECISION GXOFF,YBXOFF,TWXOFF
        DOUBLE PRECISION ypmfatmp, ypamatmp, yapertmp, ydoortmp
        DOUBLE PRECISION yariutmp, yfgmatmp, y1oatmp 
        INTEGER YBASE
	INTEGER ID, IL, GRATNUM
        INTEGER YB0BSA(10), YB0BSB(10), YB0BIA(10), YB0BIB(10)
        REAL YBXSCS(10),YBXSCI(10),TWSCLS(10),TWSCLI(10)
        REAL XZPNTS(10),XZPNTI(10)
        DOUBLE PRECISION  DGTORD,PI
C
        DATA PI /3.14159265358979323846D+00/
C
C first element for amber detector , second element for blue detector
C xfactor: motion (diodes/gauss)   (from ccs7)
C yfactor: motion (ybase units/gauss)  (from ccs7)
c        DOUBLE PRECISION XFACTOR(2), YFACTOR(2)
c        DATA XFACTOR / 2.95,  0.7/
c        DATA YFACTOR /189.0, 45.0/

C L15,PRI and CAM not implemented ! (space holder backwards L15, PRI, CAM)
        DATA FGWLIST /'H27','H19','H57','H13','H40','L15','L65',
     $    'PRI','H78','CAM'/
C Ybase0 for the various grndmd (SPECTROSCOPY, IMAGE (RR)), apertures and
C gratings (H13,19,27,40,57,L65)   
C B-3, B-1, A-1 are identical to B-2; A-3 and A-4 are merged
C no correction available for C-apertures
        DATA YB0BSA /-1925,-1322,-54,-941,-11,9999,-980,9999,9999,9999/
        DATA YB0BSB /-1661,-1058,210,-677,253,9999,-716,9999,9999,9999/

        DATA YB0BIB /-1937,-1334,-66,-953,-23,9999,-992,9999,9999,9999/
        DATA YB0BIA /-2048,-1070,-330,-689,-287,9999,
     $               -1256,9999,9999,9999/

        DATA YBXSCS /0.977,0.520,0.591,0.794,1.231,0.,1.148,0.,0.,0./
        DATA YBXSCI /0.430,0.367,0.622,0.415,0.355,0.,0.796,0.,0.,0./

        DATA TWSCLS /0.173,0.090,0.062,0.069,0.042,0.,-0.051,0.,0.,0./
        DATA TWSCLI /0.050,0.047,0.035,0.000,0.000,0.,-0.023,0.,0.,0./

        DATA XZPNTS /2.286,1.238,2.097,2.762,2.150,0.,2.733,0.,0.,0./        
        DATA XZPNTI /3.479,2.161,2.320,2.439,2.413,0.,2.649,0.,0.,0./        
C
C-----------------------------------------------------------------------
C
        IF (ID .EQ. 1) THEN
          ISTAT = 0
          CONTXT=' WARNING: corrections for FOS/RD not yet applicable!'
          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
          GOTO 1000
        ENDIF

C get on-board GIMP info
        CALL UHDGSB(IDS(1),'YFGIMPEN',YFGIMPEN,ISTAT)
c In case the keyword is not in the header, need to set a 
c value for YFGIMPEN; bool, setting to TRUE
        IF (ISTAT.NE.0) THEN
           YFGIMPEN=.FALSE.
        ENDIF
c        CALL UHDGST(IDS(1),'YFGIMPER',YFGIMPER,ISTAT)
c       NOTE: if you use YFGIMPER, clarify the 'type';  it is
c             a string YES/NO, and must be used correctly 
c
C get info for grating, aperture, op-mode and ybase
        CALL UHDGST(IDS(1),'APER_ID',APERID,ISTAT)
        CALL UHDGST(IDS(1),'FGWA_ID',FGWAID,ISTAT)
        CALL UHDGST(IDS(1),'GRNDMODE',GRNDMD,ISTAT)
        CALL UHDGSI(IDS(1),'YBASE',YBASE,ISTAT)

C get some temperatures from the shp file and calculate mean temperature
        CALL UHDGSD(IDS(6),'YPMFATMP',YPMFATMP,ISTAT)
        CALL UHDGSD(IDS(6),'YPAMATMP',YPAMATMP,ISTAT)
        CALL UHDGSD(IDS(6),'YAPERTMP',YAPERTMP,ISTAT)
        CALL UHDGSD(IDS(6),'YDOORTMP',YDOORTMP,ISTAT)
        CALL UHDGSD(IDS(6),'YARIUTMP',YARIUTMP,ISTAT)
        CALL UHDGSD(IDS(6),'YFGMATMP',YFGMATMP,ISTAT)
        CALL UHDGSD(IDS(6),'Y1OATMP',Y1OATMP,ISTAT)

        YMEANTMP = ypmfatmp+(ypamatmp+0.52)/1.07+(yapertmp-13.71)/0.506+
     $            (ydoortmp-10.45)/0.596+(yfgmatmp-14.60)/0.307+
     $            (yariutmp-3.424)/1.536+(y1oatmp-9.544)/0.630
        YMEANTMP = YMEANTMP/7.

   
c compute the corrections on FOS/BLUE side       
c first see if ON-BOARD GIMP was enabled and correct
            IF (YFGIMPEN) THEN           
c scale the GIMP according to detector for subtraction (approx 0.15 for Blue)
               IF (GRNDMD(1:3).eq.'SPE'.or.GRNDMD(1:3).eq.'RAP') 
     $             GXOFF = YOFFXP*0.15
               IF (GRNDMD(1:3).eq.'IMA') GXOFF = YOFFXP*0.15
            ELSE
c on BLUE side there was no measurable GIMP effect
               GXOFF = 0.0 
               YOFFXP = 0.0 
            ENDIF

c now correct for YBASE adjustments
c determine FGW list entry
            GRATNUM = 0
            DO IL = 1,10 
              IF (FGWAID.eq.FGWLIST(IL)) GRATNUM = IL
            ENDDO
c select according to grndmode and aperture  
            YYBASE0 = 9999
            IF (GRNDMD(1:3).eq.'SPE'.or.GRNDMD(1:3).eq.'RAP') THEN
              YAPGRTX0 = XZPNTS(GRATNUM)
              YYBSXSCL = YBXSCS(GRATNUM) 
              YTMPXSCL = TWSCLS(GRATNUM) 
              IF (APERID(1:1).eq.'A'.and.APERID(1:3).ne.'A-1') THEN
                YYBASE0 = YB0BSA(GRATNUM)
              ELSE IF (APERID(1:1).eq.'B'.or.APERID(1:3).eq.'A-1') THEN
                YYBASE0 = YB0BSB(GRATNUM)
              ENDIF
            ELSE IF (GRNDMD(1:3).eq.'IMA') THEN
              YAPGRTX0 = XZPNTI(GRATNUM)
              YYBSXSCL = YBXSCI(GRATNUM) 
              YTMPXSCL = TWSCLI(GRATNUM) 
              IF (APERID(1:1).eq.'A'.and.APERID(1:3).ne.'A-1') THEN
                YYBASE0 = YB0BIA(GRATNUM)
              ELSE IF (APERID(1:1).eq.'B'.or.APERID(1:3).eq.'A-1') THEN
                YYBASE0 = YB0BIB(GRATNUM)
              ENDIF
            ENDIF
          IF (ID .EQ. 1 .OR. APERID(1:1).eq.'C') THEN
            ISTAT = 99
            CONTXT='ERROR - POAXOFF not found det,fgwa,aper combination' 
            GOTO 999
          ENDIF
C        
          DGTORD = PI / 180.0D0  
          YBXOFF = YYBSXSCL*(YBASE-YYBASE0)*sin(DGTORD*(17.6))/16.

C and correct for temperature effect on FGWA + a zeropoint
          TWXOFF = YTMPXSCL*YMEANTMP
C so now XOFF is in "raw data pixels" (1/4 diode for SPEC, 1 diode for IMA) 
          PXOFF = -(YAPGRTX0 + GXOFF + YBXOFF + TWXOFF) 

100     CONTINUE

        ISTAT=0
        GOTO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
