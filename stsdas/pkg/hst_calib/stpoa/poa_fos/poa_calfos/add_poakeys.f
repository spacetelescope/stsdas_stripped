
        SUBROUTINE ADD_POAKEYS(FRAME, POA_TXT, ISTAT)

*  Module number:
*
*  Module name: add_poakeys
*
*  Keyphrase:
*  ----------
*       Update group header keys
*
*  Description:
*  ------------
*       This routine adds POA related group header keywords to
*       the group parameters of the output c1h file.  It has
*       two input parameters:
*               frame - the "group" number
*               isat - the error status
*
*  History:
*  --------
*  Version   Date        Author          Description
*      1.0       Jul 00  M. Rosa         POA calibration code
*      1.1       MAR 01  A. Alexov       Added PVX,PVY,PVZ
* *******************************************************************
      INTEGER ISTAT
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C Common block containing input/output file descriptions
C
C       IDS - file id numbers
C       GCOUNT - group counts
C       NAXIS - naxis
C       NAXIS1 - first dimensions
C       NAXIS2 - second dimensions
C       FILL - Fill values

        INTEGER IDS(30),NAXIS(30),NAXIS1(30),NAXIS2(30),GCOUNT(30)
        REAL FILL(30)
        COMMON /IOCOM/IDS,GCOUNT,NAXIS,NAXIS1,NAXIS2,FILL

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

C Local variables
C
        CHARACTER*80 CONTXT
        INTEGER II, OUTPUT_IDS(20)


C build a list of the output ID's which have file pointers;
C for those ID's header keys will be filled in 

c        WRITE(CONTXT, 887) 
c        CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
 
        WRITE(CONTXT, 888) FRAME
 888    FORMAT('Updating POA related new KEYWORDS in GEIS GROUP ', I5)
        CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
 
        DO 555 II=11, 20
           IF ((II .EQ. 11) .AND. (IDS(II) .GT. 0)) THEN
              OUTPUT_IDS(II) = 1
           ELSE IF ((II .EQ. 12) .AND. (IDS(II) .GT. 0)) THEN
              OUTPUT_IDS(II) = 1
           ELSE IF ((II .EQ. 13) .AND. (IDS(II) .GT. 0)) THEN
              OUTPUT_IDS(II) = 1
           ELSE IF ((II .EQ. 14) .AND. (IDS(II) .GT. 0)) THEN
              OUTPUT_IDS(II) = 1
           ELSE IF ((II .EQ. 16) .AND. (IDS(II) .GT. 0)) THEN
              OUTPUT_IDS(II) = 1
           ELSE IF ((II .EQ. 17) .AND. (IDS(II) .GT. 0)) THEN
              OUTPUT_IDS(II) = 1
           ELSE IF ((II .EQ. 18) .AND. (IDS(II) .GT. 0)) THEN
              OUTPUT_IDS(II) = 1
           ELSE IF ((II .EQ. 19) .AND. (IDS(II) .GT. 0)) THEN
              OUTPUT_IDS(II) = 1
           ELSE IF ((II .EQ. 20) .AND. (IDS(II) .GT. 0)) THEN
              OUTPUT_IDS(II) = 1
           ELSE
              OUTPUT_IDS(II) = 0
           ENDIF

555      CONTINUE

C convert units from m -> km
         POAXP = POAXP / 1000.0
         POAYP = POAYP / 1000.0
         POAZP = POAZP / 1000.0
C convert units from m/sec -> km/sec
         VXP = VXP / 1000.0
         VYP = VYP / 1000.0
         VZP = VZP / 1000.0

C for every output id, put the new header keys (id = 11-14, 16-20)
         DO 666 II=11, 20
            IF (OUTPUT_IDS(II) .EQ. 1) THEN
                IF (IDS(II) .GT. 0) THEN
                    CALL UHDPSD(IDS(II),'MIDTIMP',MIDTIMP,ISTAT)
                    CALL UHDPSD(IDS(II),'POAXP',POAXP,ISTAT)
                    CALL UHDPSD(IDS(II),'POAYP',POAYP,ISTAT)
                    CALL UHDPSD(IDS(II),'POAZP',POAZP,ISTAT)
                    CALL UHDPSD(IDS(II),'VXP',VXP,ISTAT)
                    CALL UHDPSD(IDS(II),'VYP',VYP,ISTAT)
                    CALL UHDPSD(IDS(II),'VZP',VZP,ISTAT)
                    CALL UHDPSD(IDS(II),'BNP',BNP,ISTAT)
                    CALL UHDPSD(IDS(II),'BEP',BEP,ISTAT)
                    CALL UHDPSD(IDS(II),'BDP',BDP,ISTAT)
                    CALL UHDPSD(IDS(II),'BV1P',BV1P,ISTAT)
                    CALL UHDPSD(IDS(II),'BV2P',BV2P,ISTAT)
                    CALL UHDPSD(IDS(II),'BV3P',BV3P,ISTAT)
                    CALL UHDPSD(IDS(II),'BDXP',BDXP,ISTAT)
                    CALL UHDPSD(IDS(II),'BDYP',BDYP,ISTAT)
                    CALL UHDPSD(IDS(II),'BDZP',BDZP,ISTAT)
                    CALL UHDPSD(IDS(II),'YGMPXSCL',YGMPXSCL,ISTAT)
                    CALL UHDPSD(IDS(II),'YGMPYSCL',YGMPYSCL,ISTAT)
                    CALL UHDPSD(IDS(II),'YOFFXP',YOFFXP,ISTAT)
                    CALL UHDPSD(IDS(II),'YOFFYP',YOFFYP,ISTAT) 
                    CALL UHDPSD(IDS(II),'YYBASE0',YYBASE0,ISTAT)
                    CALL UHDPSD(IDS(II),'YYBSXSCL',YYBSXSCL,ISTAT)
                    CALL UHDPSD(IDS(II),'YMEANTMP',YMEANTMP,ISTAT)
                    CALL UHDPSD(IDS(II),'YTMPXSCL',YTMPXSCL,ISTAT)
                    CALL UHDPSD(IDS(II),'YAPGRTX0',YAPGRTX0,ISTAT)
                    CALL UHDPSD(IDS(II),'GMSTP',GMSTP,ISTAT)
                    CALL UHDPSD(IDS(II),'GLNGP',GLNGP,ISTAT)
                    CALL UHDPSD(IDS(II),'GLATP',GLATP,ISTAT)
                    CALL UHDPSD(IDS(II),'MLNGP',MLNGP,ISTAT)
                    CALL UHDPSD(IDS(II),'MLATP',MLATP,ISTAT)
                    CALL UHDPSD(IDS(II),'ALNGP',ALNGP,ISTAT)
                    CALL UHDPSD(IDS(II),'ALATP',ALATP,ISTAT)
                    CALL UHDPSD(IDS(II),'LSHP',LSHP,ISTAT)
C                   IF(ISTAT.NE.0)THEN
C                      CONTXT='ERROR updating OFFXF keyword'
C                      GO TO 999
C                   END IF
                END IF
             END IF
666       CONTINUE

cCCC print the information to the screen
c          WRITE(CONTXT, 887) 
c          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
c          WRITE(CONTXT, 899), MIDTIMP
c 899      FORMAT('  Time stamp MIDTIMP: ', F14.7)
c          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
c          WRITE(CONTXT, 887) 
c          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
c          WRITE(CONTXT, 900), POAXP, POAYP, POAZP
c 900      FORMAT('  Spacecraft LOC (km):',
c     *           ' POAX=', F10.3, ', POAY=',F10.3, ', POAZ=', F10.3)
c          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)


c          WRITE(CONTXT, 887) 
c          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
c
c          WRITE(CONTXT, 903), VXP, VYP, VZP
c 903      FORMAT('  Spacecraft VEL (km/sec):',
c     *           ' VX=', F7.3, ', VY=',F7.3, ', VZ=', F7.3)
c          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)


c          WRITE(CONTXT, 887) 
c          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
c          WRITE(CONTXT, 905), BNP, BEP, BDP
c 905      FORMAT('  GEOMAG FIELD [Gauss]:',
c     *           ' BN=', F7.3, ', BE=',F7.3, ', BD=', F7.3)
c          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)


c          WRITE(CONTXT, 887) 
c          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
c          WRITE(CONTXT, 909), YOFFXP, YOFFYP
c 909      FORMAT('  Predicted OFFSETS:   YOFFX=', F6.2, ', YOFFY=',F6.2)
c          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)


c          WRITE(CONTXT, 887) 
c          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)

c 887    FORMAT('')

       END 
