       SUBROUTINE SEEING  !I
CM     PROGRAM MSEEING    !M
C++
C
C SEEING - create a simulated seeing PSF using the method
C          described by Saglia et al, MNRAS,264,961
C
C This is an IRAF application written using the F77/VOS.
C
C Purpose:
C
C Create a seeing disc simulation using user specified
C values of FWHM and gamma, the exponent. The PSF is circularly
C symmetric.
C
C Note:
C
C This code doesn't work well if the PSF is very large or small
C relative to the frame. In rough terms the minimum in the "input
C image" should be small (eg, 10-8) and so should the minimum in
C the output, relative to the peak (eg, a factor of 10-6 lower).
C
C History:
C
C First version, Richard Hook, ST-ECF, Apr 94
C Modified to have different FWHM in X and Y, Jan 95
C Modified to use a different "b" expression, Apr 96
C Modified to use lower-case parameters names, April 2000
C (also release as part of the stecf package)
C--
      IMPLICIT NONE
                   
C Iraf global space
      DOUBLE PRECISION MEMD(1) !I
      COMMON /MEM/MEMD         !I

C MIDAS global storage
CM    INTEGER MEMD(1)          !M
CM    COMMON /VMR/MEMD         !M

C Local variables
      INTEGER NX,NY,DIMS(2),ID,ISTAT,IPNT,TEMP
      DOUBLE PRECISION SCRATCH(1024)
      CHARACTER*80 IMAGE
      DOUBLE PRECISION BX,BY,FWHMX,FWHMY,GAMMA
C++

C Start of code

C Midas initialisation
CM    CALL STSPRO('MSEEING') !M

C Announce the version
      CALL UMSPUT('+ SEEING Version 1.3 (April 2000)', !I
     :            1,0,ISTAT)                                      !I
CM    CALL UMSPUT('+ MSEEING Version 1.3 (April 2000)', !M
CM   :            1,0,ISTAT)                                      !M
                                       
C Get the name of the output image
      CALL UCLGST('output',IMAGE,ISTAT)
                                       
C and its dimensions
      CALL UCLGSI('xdim',NX,ISTAT)
      CALL UCLGSI('ydim',NY,ISTAT)
                                  
      DIMS(1)=NX
      DIMS(2)=NY
                
C and the size and gamma parameters
C X and Y are separate
      CALL UCLGSD('fwhmx',FWHMX,ISTAT)
      CALL UCLGSD('fwhmy',FWHMY,ISTAT)
                                              
      CALL UCLGSD('gamma',GAMMA,ISTAT)
                                    
C we convert this value to "b" values in the two directions
C
C The full formula is:
C
C b =    FWHM * pi
C     ---------------
C     N.2.sqrt(ln(2))       [N is the number of pixels in X or Y]
C
C In version  1.2 this is modified to:
C
C b =    FWHM * pi
C     ---------------
C     N.2.(ln(2)**(1/gamma))  [N is the number of pixels in X or Y]
C
C This expression is more or less a guess (which is certainly correct
C when gamma=2)

C
CC      BX=FWHMX*3.141592653/(DBLE(NX)*1.6651092)
CC      BY=FWHMY*3.141592653/(DBLE(NY)*1.6651092)
      BX=FWHMX*3.141592653/(DBLE(NX)*2.0*(LOG(2.0)**(1.0/GAMMA)))
      BY=FWHMY*3.141592653/(DBLE(NY)*2.0*(LOG(2.0)**(1.0/GAMMA)))

C Create the image
      CALL UIMCRE(IMAGE,7,2,DIMS,ID,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT('Unable to create output image',
     :               1,0,ISTAT)
         GO TO 99
      ENDIF
           
C Allocate memory for the image
      CALL UDMGET(NX*NY,7,IPNT,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT('Unable to allocate memory for data',
     :               1,0,ISTAT)
         GO TO 99
      ENDIF
           
C Allocate memory for the temp image
      CALL UDMGET(NX*NY*2,7,TEMP,ISTAT)
      IF(ISTAT.NE.0) THEN
         CALL UMSPUT('Unable to allocate memory for FFT',
     :               1,0,ISTAT)
         GO TO 99
      ENDIF
           
C Fill the FFT images
      CALL SEEFFT(NX,NY,BX,BY,GAMMA,MEMD(TEMP))
                                               
C Take the inverse FFT to give the output image
      CALL DFOURT(MEMD(TEMP),DIMS,2,1,1,SCRATCH)
                                                
C Quadrant swap to put the peak in the middle
      CALL QUAD(MEMD(TEMP),NX,NY,MEMD(IPNT))
                                            
C Write out the result
      CALL UIPS2D(ID,1,NX,1,NY,MEMD(IPNT),ISTAT)
                                                
 99   CONTINUE
      CALL UIMCLO(ID,ISTAT)
      CALL UDMFRE(IPNT,7,ISTAT)
      CALL UDMFRE(TEMP,7,ISTAT)
                               
C Close down MIDAS
CM    CALL STSEPI    !M

      END
         
      SUBROUTINE SEEFFT(NX,NY,BX,BY,GAMMA,TEMP)
C
C Fill an array with an exponential disc of the
C form: v=exp[-(kb)**gamma]
C
C where k is the radial distance from the specified point
C
C This routine allows the b coefficient to be different
C in X and Y. b is scaled so that k can be in pixel coordinates
C in the FFT arrays
C
      IMPLICIT NONE
                   
      INTEGER NX,NY
      DOUBLE PRECISION BX,BY,GAMMA
      DOUBLE PRECISION TEMP(NX*NY*2),IMIN,IMAX,DX,DY
      INTEGER I,J,II,JJ,IP,ISTAT
      CHARACTER*80 CHARS
                        
      IP=1
          
      DO J=-NY/2+1,NY/2
          DO I=-NX/2+1,NX/2
             DX=DBLE(I-1)
             DY=DBLE(J-1)
             II=I
             JJ=J
             IF(I.LT.1) II=I+NX
             IF(J.LT.1) JJ=J+NY
             IP=(JJ-1)*NX*2+II*2-1
             TEMP(IP)=DEXP(-(SQRT((DX*BX)**2
     :                          +(DY*BY)**2))**GAMMA)
             TEMP(IP+1)=0.0D0
          ENDDO
      ENDDO
           
      IMIN=1.0E30
      IMAX=-IMIN
                
      DO I=1,2*NX*NY-1,2
         IF(TEMP(I).LT.IMIN) IMIN=TEMP(I)
         IF(TEMP(I).GT.IMAX) IMAX=TEMP(I)
      ENDDO
           
      WRITE(CHARS,'(''-Extreme values in input array: '',
     :              2G12.4)') imin,imax
      CALL UMSPUT(CHARS,1,0,ISTAT)
                                  
      RETURN
      END
         
      SUBROUTINE QUAD(WORK,NX,NY,OUT)
C
C Quadrant swap the results of the inverse FFT
C
      INTEGER NX,NY
      DOUBLE PRECISION WORK(NX*NY*2),OUT(NX,NY),FAC
      DOUBLE PRECISION IMIN,IMAX
      INTEGER ISTAT
      CHARACTER*80 CHARS
                        
      FAC=DFLOAT(NX*NY)
      IP=1
          
      NX2=MAX(1,NX/2)
      NY2=MAX(1,NY/2)
                     
C Check the imaginary range
      IMIN=1.0E30
      IMAX=-IMIN
                
      DO I=2,2*NX*NY,2
         IF(WORK(I).LT.IMIN) IMIN=WORK(I)
         IF(WORK(I).GT.IMAX) IMAX=WORK(I)
      ENDDO
           
           
      WRITE(CHARS,'(
     :   ''-Extreme values in output imaginary part: '',
     :              2G12.4)') IMIN,IMAX
      CALL UMSPUT(CHARS,1,0,ISTAT)
                                  
      IMIN=1.0E30
      IMAX=-IMIN
                
      DO I=1,2*NX*NY-1,2
         IF(WORK(I).LT.IMIN) IMIN=WORK(I)
         IF(WORK(I).GT.IMAX) IMAX=WORK(I)
      ENDDO
           
      WRITE(CHARS,'(
     :   ''-Extreme values in output real part: '',
     :              2G12.4)') IMIN,IMAX
      CALL UMSPUT(CHARS,1,0,ISTAT)
                                  
       DO J=1,NY2
          DO I=1,NX2
             OUT(I+NX/2,J+NY/2)=WORK(IP)/FAC
             IP=IP+2
          ENDDO
          IP=IP+NX
       ENDDO
            
       IP=NX+1
       DO J=1,NY2
          DO I=1,NX2
             OUT(I,J+NY/2)=WORK(IP)/FAC
             IP=IP+2
          ENDDO
          IP=IP+NX
       ENDDO
            
       IP=NX*NY+1
       DO J=1,NY2
          DO I=1,NX2
             OUT(I+NX/2,J)=WORK(IP)/FAC
             IP=IP+2
          ENDDO
          IP=IP+NX
       ENDDO
            
       IP=NX*NY+NX+1
       DO J=1,NY2
          DO I=1,NX2
             OUT(I,J)=WORK(IP)/FAC
             IP=IP+2
          ENDDO
          IP=IP+NX
       ENDDO
            
      RETURN
      END
