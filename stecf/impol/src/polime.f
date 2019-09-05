      SUBROUTINE POLIME
C
C     This FORTRAN program reads an intensity image and
C     produces simulated images to correspond to linear 
C     polarization and position angle. The polarization 
C     is specified by a central value, position of centre
C     a radial exponent and a scaling radius, whilst the 
C     position angle is specified by a central value, 
C     position of centre, radial exponent, a scaling radius 
C     and an offset angle. Random errors can be applied to 
C     the output polarization and position angle images.
C
C     The total intensity image provides a template for
C     the linear polarization and polarization position 
C     angle images to be produced. A table of the required
C     central values, X,Y positions and radial exponents is
C     read. The offset angle to be applied to the output
C     position angles is prompted. Gaussian random errors 
C     can be applied to the output polarization and position
C     angle values specified as fraction of the values.
C     The linear polarization and position angle images
C     are output.
C
C     Written by  J. R. Walsh, ST-ECF, ESO (jwalsh@eso.org)  December 1999
C     Version 1.00
C
      IMPLICIT NONE

      INTEGER STAT
      INTEGER STAT1
      INTEGER STAT2
      INTEGER STAT3

      INTEGER IMDSCR1
      INTEGER IMDSCR2
      INTEGER IMDSCR3
      INTEGER TBSCR

      INTEGER NAXISI
      INTEGER DIMEN(7)
      INTEGER ODIMEN(7)
      INTEGER DTYPE

      INTEGER N1
      INTEGER N2
      INTEGER O1
      INTEGER O2

      INTEGER TOTI
      INTEGER MODPO
      INTEGER MODPA
 
      INTEGER COLID1
      INTEGER COLID2
      INTEGER COLID3
      INTEGER COLID4
      INTEGER COLID5
      INTEGER NUMR
      INTEGER LBOO

      INTEGER COKEY
      INTEGER SEED
      INTEGER NELEM

      INTEGER PMOD1
      INTEGER PMOD2
      INTEGER PMOD3
      INTEGER PMOD4
      INTEGER PMOD5

      REAL PAOFF
      REAL CLIPI
      REAL PORAN
      REAL PARAN

      INTEGER HOLD1
      INTEGER HOLD2

      REAL MEMR(1)

      CHARACTER*32 INTOIN
      CHARACTER*32 ONAMPO
      CHARACTER*32 ONAMPA
      CHARACTER*32 TABNAM

      CHARACTER*132 OUTEXT

      LOGICAL TEXIST
      LOGICAL LRAN

      LOGICAL MEMB(1)

      COMMON/MEM/MEMR
      EQUIVALENCE (MEMR,MEMB)
C
C     Get name of the total intensity data set name 
C
100   CALL UCLGST('inint',INTOIN,STAT)
      CALL UIMOPN(INTOIN,1,IMDSCR1,STAT1)
      IF (STAT1.NE.0) THEN
        WRITE(OUTEXT,105) INTOIN
105     FORMAT(' Error opening data file ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMGID(IMDSCR1,DTYPE,NAXISI,DIMEN,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,107) INTOIN
107     FORMAT(' Error reading data file header ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      IF (NAXISI.NE.2) THEN
        WRITE(OUTEXT,109) 
109     FORMAT(' Data file is not 2-dimensional')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      N1=DIMEN(1)
      N2=DIMEN(2)
      O1=N1
      O2=N2
C
C     Allocate dynamic memory for the 2-D input image
C     of dimensions N1 by N2 (real number)
C
      NELEM=N1*N2
      CALL UDMGET(NELEM,6,TOTI,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,121) 
121     FORMAT(' Unable to assign memory for internal 2-D array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Read the input image to TOTI
C
      CALL UIGS2R(IMDSCR1,1,N1,1,N2,MEMR(TOTI),STAT3)
      IF (STAT3.NE.0) THEN
        WRITE(OUTEXT,123) INTOIN
123     FORMAT(' Error reading data from file ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C        
C     Open the table for the list of central values,
C     centre coordinates and exponents for the model
C     polarization and position angle images
C
200   CALL UCLGST('simlis',TABNAM,STAT)
      CALL UTTACC(TABNAM,TEXIST,STAT)
      IF (.NOT.TEXIST) THEN
        WRITE(OUTEXT,211) 
211     FORMAT(' Table file does not exist ')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UTTOPN(TABNAM,1,TBSCR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,213)
213     FORMAT(' Table could not be opened')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the number of rows in the table
C
      CALL UTPGTI(TBSCR,21,NUMR,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,215)
215     FORMAT(' Failed to find no. rows in table')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the array PMOD1 -
C     PMOD5 of the values, x and y cordinates, exponents
C     and scaling radii
C
      CALL UDMGET(NUMR,6,PMOD1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,219)
219     FORMAT(' Unable to assign memory for internal 1-D array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NUMR,6,PMOD2,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,219)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NUMR,6,PMOD3,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,219)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NUMR,6,PMOD4,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,219)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NUMR,6,PMOD5,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,219)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NUMR,1,LBOO,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,221)
221     FORMAT(' Unable to assign memory for internal 1-D',
     :' Boolean array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     For each column get the values and read into PMOD1 - 
C     PMOD5
C
      CALL UTCNUM(TBSCR,1,COLID1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,223)
223     FORMAT(' Failed to find column ID in table')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCGTR(TBSCR,COLID1,1,NUMR,MEMR(PMOD1),MEMB(LBOO),STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,225)
225     FORMAT(' Failed to read column data from table')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF

      CALL UTCNUM(TBSCR,2,COLID2,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,223)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCGTR(TBSCR,COLID2,1,NUMR,MEMR(PMOD2),MEMB(LBOO),STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,225)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF

      CALL UTCNUM(TBSCR,3,COLID3,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,223)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCGTR(TBSCR,COLID3,1,NUMR,MEMR(PMOD3),MEMB(LBOO),STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,225)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF

      CALL UTCNUM(TBSCR,4,COLID4,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,223)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCGTR(TBSCR,COLID4,1,NUMR,MEMR(PMOD4),MEMB(LBOO),STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,225)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF

      CALL UTCNUM(TBSCR,5,COLID5,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,223)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UTCGTR(TBSCR,COLID5,1,NUMR,MEMR(PMOD5),MEMB(LBOO),STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,225)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF

      CALL UTTCLO(TBSCR,STAT)
C
C     If more than one polarization model is computed
C     (i.e. NUMR>1) theN need to prompt for the method to
C     use for conincident points from more than one model
C     Allowed values are:
C         1 = take maximum
C         2 = take minimum
C         3 = take vector sum
C
      CALL UCLGSI('coincid',COKEY,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,301)
301     FORMAT(' Failed to get value for coindidence key')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      IF (COKEY.GT.3) THEN
        WRITE(OUTEXT,311)
311     FORMAT(' Maximum value of parameter exceeded')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the value of the minimum intensity value for
C     which to produce model polarization and position
C     angle values
C
      CALL UCLGSR('intclip',CLIPI,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,321)
321     FORMAT(' Failed to get value for minimum intensity to
     : model')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Get the value of the offset angle to add to all
C     the position angles
C
      CALL UCLGSR('pacorr',PAOFF,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,326)
326     FORMAT(' Failed to get position angle offset')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Determine if random errors are to be applied to the
C     polarization and position angle values
C
      CALL UCLGSB('random',LRAN,STAT2)
      IF (STAT2.NE.0) THEN
        LRAN=.FALSE.
      ENDIF
C
C     Get the integer value of the seed for the random
C     number generator
C
      IF (LRAN) THEN
        CALL UCLGSI('seed',SEED,STAT)
        IF (STAT.NE.0) THEN
          WRITE(OUTEXT,331)
331       FORMAT(' Failed to get value for seed for random
     : number generation')
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UCLGSR('polran',PORAN,STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,311)
341       FORMAT(' Failed to get error fraction for polarization')
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UCLGSR('paran',PARAN,STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,351)
351       FORMAT(' Failed to get error fraction for position angle')
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
      ENDIF
C
C     Allocate dynamic memory for the arrays required by
C     POLMOD
C     Real arrays: MODPO, MODPA
C
400   NELEM=N1*N2
      CALL UDMGET(NELEM,6,MODPO,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,411)
411     FORMAT(' Unable to assign memory for internal 2-D',
     :' real array')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NELEM,6,MODPA,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,411)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Allocate dynamic memory for the arrays HOLD1 and
C     HOLD2 for holding intermediate values of polarization
C     and position angle in POLMOD
C
      CALL UDMGET(NUMR,6,HOLD1,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,219)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UDMGET(NUMR,6,HOLD2,STAT)
      IF (STAT.NE.0) THEN
        WRITE(OUTEXT,219)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Call the subroutine to compute the model 
C     polarization and position angle images corresponding
C     to the input intensity image 
C
      CALL POLMOD(N1,N2,MEMR(TOTI),NUMR,MEMR(PMOD1),
     :            MEMR(PMOD2),MEMR(PMOD3),MEMR(PMOD4),
     :            MEMR(PMOD5),CLIPI,PAOFF,COKEY,LRAN,
     :            SEED,PORAN,PARAN,MEMR(HOLD1),MEMR(HOLD2),
     :            MEMR(MODPO),MEMR(MODPA))
C
C     Free the dynamic memory allocated which is no longer 
C     required
C
      CALL UDMFRE(TOTI,6,STAT)
      CALL UDMFRE(PMOD1,6,STAT)
      CALL UDMFRE(PMOD2,6,STAT)
      CALL UDMFRE(PMOD3,6,STAT)
      CALL UDMFRE(PMOD4,6,STAT)
      CALL UDMFRE(PMOD5,6,STAT)
      CALL UDMFRE(HOLD1,6,STAT)
      CALL UDMFRE(HOLD2,6,STAT)
      CALL UDMFRE(LBOO,1,STAT)
C
C     Open the output file for the linear polarization 
C
600   ODIMEN(1)=O1
      ODIMEN(2)=O2
      CALL UCLGST('modpol',ONAMPO,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,611) 
611     FORMAT(' Failed to get name of output polarization image')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMCRE(ONAMPO,6,2,ODIMEN,IMDSCR2,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCR1,IMDSCR2,STAT2)
        CALL UIPS2R(IMDSCR2,1,O1,1,O2,MEMR(MODPO),STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,621) ONAMPO
621       FORMAT(' Failed to copy internal array to output file ',A24)
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCR2,STAT3)
      ELSE
        WRITE(OUTEXT,631) ONAMPO
631     FORMAT(' Failed to create output file ',A24)
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Open the output file for the polarization 
C     position angle
C
      CALL UCLGST('modpa',ONAMPA,STAT2)
      IF (STAT2.NE.0) THEN
        WRITE(OUTEXT,641) 
641     FORMAT(' Failed to get name of output position angle image')
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
      CALL UIMCRE(ONAMPA,6,2,ODIMEN,IMDSCR3,STAT1)
      IF (STAT1.EQ.0) THEN
        CALL UHDCPY(IMDSCR1,IMDSCR3,STAT2)
        CALL UIPS2R(IMDSCR3,1,O1,1,O2,MEMR(MODPA),STAT2)
        IF (STAT2.NE.0) THEN
          WRITE(OUTEXT,621) ONAMPA
          CALL UMSPUT(OUTEXT,3,0,STAT)
          GO TO 990
        ENDIF
        CALL UIMCLO(IMDSCR3,STAT3)
      ELSE
        WRITE(OUTEXT,631) ONAMPA
        CALL UMSPUT(OUTEXT,3,0,STAT)
        GO TO 990
      ENDIF
C
C     Close the input intensity image
C
      CALL UIMCLO(IMDSCR1,STAT)
      GO TO 999

990   WRITE(OUTEXT,991)
991   FORMAT(' Program failed. No output')
      CALL UMSPUT(OUTEXT,1,0,STAT)

999   END

      SUBROUTINE POLMOD(N1,N2,MODIM,NM,CV,CX,CY,REX,RSC,
     :                  CLIPI,PAOFF,COKEY,LRAN,SEED,PORAN,
     :                  PARAN,HOLD1,HOLD2,MODPO,MODPA)
C
C     This subroutine computes a model image of the 
C     polarization (%) and the position angle (degrees)
C     of the polarization vector based on a model intensity
C     image MODIM. The model polarization(s) are computed 
C     from the parameters CV, CX, CY, REX and RSC 
C     as follows:
C     POL = CV(I)*(R/RSC(I))**REX(I)
C     where R = SQRT( (X-CX(I))**2 + (Y-CY(I))**2)
C     and there can be any number of such models.
C     Coincident points are fitted by the largest, smallest,
C     sum or mean of the values at that point according to
C     the value of COKEY.
C     The position angle is computed as the perpendicular
C     to the line joining the point to CX,CY.
C     If random errors are selected (LRAN) then a random
C     error is added to the polarization or position angle value
C     specified as the normally distributed random error for
C     the a sigma of POL*PORAN for polarization and PA*PARAN
C     for position angle
C
      IMPLICIT NONE
      INTEGER N1 ! 1st dimension of input array MODIM
      INTEGER N2 ! 2nd dimension of input array MODIM
      INTEGER NM ! No. of polarization models (dimension of CV)
      INTEGER COKEY ! Key to combining coincient point from different models
      INTEGER SEED ! Seed for random number generator

      REAL MODIM(N1,N2) ! Input intensity array 
      REAL CV(NM) ! Array of central values of polarization models 
      REAL CX(NM) ! Array of X values of centre of polarization models 
      REAL CY(NM) ! Array of Y values of centre of polarization models
      REAL REX(NM) ! Array of radial exponent of polarization models 
      REAL RSC(NM) ! Array of scaling radius of polarization models       
      REAL CLIPI ! Value of minimum intensity to model for output
      REAL PAOFF ! Position angle offset to apply
      REAL PORAN ! Factor times polarization for sigma of random errors 
      REAL PARAN ! Factor times position angle for sigma of random errors 
      REAL HOLD1(NM) ! Holding array for model polarization values
      REAL HOLD2(NM) ! Holding array for model position angle values

      REAL MODPO(N1,N2) ! Output model polarization array (%)
      REAL MODPA(N1,N2) ! Output model position angle array (degrees)

      LOGICAL LRAN ! Whether to apply random errors to polarization and position angle
C
C     Local variables
C
      INTEGER I,J,K

      REAL VAL
      REAL RAN1
      REAL R
      REAL PALINE
      REAL POVAL
      REAL PAVAL
      REAL NORDEV

C      
C     Initialize random number generator
C
      VAL=RAN1(-SEED)
C
C     Loop through all pixels in input image. If intensity
C     >0 then produce the polarization and position angle 
C     values at that point. If more than one model then
C     combine values according to COKEY and write to output
C     polarization and position angle values
C
      DO J=1,N2,1
        DO I=1,N1,1
          IF (MODIM(I,J).GE.CLIPI) THEN
C
C         Produce the arrays of the model polarization and
C         position angle values
C
            DO K=1,NM,1
              R=SQRT( (REAL(I)-CX(K))**2. + (REAL(J)-CY(K))**2. )
              IF (R.NE.0.0) THEN
                IF (RSC(K).NE.0.0) THEN
                   POVAL=CV(K)*(R/RSC(K))**(REX(K))
                   PAVAL=PALINE(I,J,CX(K),CY(K))
                ELSE
                  POVAL=CV(K)
                  PAVAL=0.0 + PAOFF
                ENDIF
              ELSE
                POVAL=CV(K)
                PAVAL=0.0 + PAOFF
              ENDIF
              IF (LRAN) THEN
                HOLD1(K)=NORDEV(SEED,POVAL,PORAN*POVAL)
                HOLD2(K)=NORDEV(SEED,PAVAL,PARAN*PAVAL)
              ELSE
                HOLD1(K)=POVAL
                HOLD2(K)=PAVAL
              ENDIF
            ENDDO
C
C           Copy the single values of polarization and position 
C           or the combined value (using COKEY) to the output
C           arrays. For sum and mean values the vector sum
C           of the polarization vectors is made
C
            CALL POPACOMB(NM,HOLD1,HOLD2,COKEY,PAOFF,N1,N2,
     :                    I,J,MODPO,MODPA)
C
C           Finally check that angles positive and < 180degrees
C
            IF (MODPA(I,J).LT.0.0) THEN
              MODPA(I,J)=180.0 + MODPA(I,J)
            ENDIF
            IF (MODPA(I,J).GT.180.0) THEN
              MODPA(I,J)=180.0 - MODPA(I,J)
            ENDIF
          ELSE
            MODPO(I,J)=0.0
            MODPA(I,J)=0.0
          ENDIF
        ENDDO
      ENDDO

99    END

      REAL FUNCTION PALINE(IA1,IA2,C1,C2)
C
C     This function subprogram returns the position 
C     angle (degrees) of the polarization vector at 
C     integer position IA1,IA2 by scattering from the 
C     (real) position C1,C2
C
      IMPLICIT NONE
      INTEGER IA1 ! X integer position for scattering
      INTEGER IA2 ! Y integer position for scattering

      REAL C1 ! X position of scattering centre
      REAL C2 ! Y position of scattering centre

      REAL X
      REAL Y
      REAL PA
 
      X=REAL(IA1)-C1
      Y=REAL(IA2)-C2
      PA=(180.0/3.1415926535898)*ATAN2(Y,X)
      IF (PA.LT.0.0) THEN
        PA=180.0 + PA
      ENDIF
      IF (PA.GT.180.0) THEN 
        PA=PA-180.0
      ENDIF
      PALINE=PA

99    END

      SUBROUTINE POPACOMB(N,ARRPO,ARRPA,IKEY,PAOFF,N1,N2,
     :                    IS1,IS2,OUTPO,OUTPA)
C
C     This subroutine writes the values of the combined 
C     model polarization and position angle to the output
C     arrays OUTPO and OUTPA. The input sets of polarization
C     ARRPO and position angle ARRPA are cobined according
C     to the value of IKEY as follows:
C       IKEY=1 maximum of array values
C       IKEY=2 minimum of array values
C       IKEY=3 vecotor sum of array values
C     If sum or mean then a vector sum is performed
C
      IMPLICIT NONE
      INTEGER N ! Dimension of array ARRY
      INTEGER IKEY ! Key value to determine operation on ARRY
      INTEGER N1 ! 1st dimension of output arrays
      INTEGER N2 ! 2nd dimension of output arrays
      INTEGER IS1 ! X point in output arrays to set
      INTEGER IS2 ! Y point in output arrays to set

      REAL ARRPO(N) ! Input array of polarization values
      REAL ARRPA(N) ! Input array of position angle values
      REAL PAOFF ! Position angle offset to apply
      REAL OUTPO(N1,N2) ! Output array of combined polarization
      REAL OUTPA(N1,N2) ! Output array of combined position angle
C
C     Local variables
C
      INTEGER I
      REAL POMAX
      REAL POMIN
      REAL PAMAX
      REAL PAMIN
      REAL QSUM
      REAL USUM
      REAL PA
      REAL SINRD
      REAL COSRD

      IF (N.EQ.1) THEN
        OUTPO(IS1,IS2)=ARRPO(N)
        OUTPA(IS1,IS2)=ARRPA(N) + PAOFF
        GO TO 99
      ENDIF

      IF (IKEY.EQ.1.AND.N.GT.1) THEN
        POMAX=-1.0E32
        PAMAX=-1.0E32
        DO I=1,N,1
          IF (ARRPO(I).GE.POMAX) THEN
            POMAX=ARRPO(I)
            PAMAX=ARRPA(I)
          ENDIF
        ENDDO
        OUTPO(IS1,IS2)=POMAX
        OUTPA(IS1,IS2)=PAMAX + PAOFF
      ENDIF

      IF (IKEY.EQ.2.AND.N.GT.1) THEN
        POMIN=1.0E32
        PAMIN=1.0E32
        DO I=1,N,1
          IF (ARRPO(I).LE.POMIN) THEN
            POMIN=ARRPO(I)
            PAMIN=ARRPA(I)
          ENDIF
        ENDDO
        OUTPO(IS1,IS2)=POMIN
        OUTPA(IS1,IS2)=PAMIN + PAOFF
      ENDIF

      IF (IKEY.EQ.3.AND.N.GT.1) THEN
        QSUM=0.0
        USUM=0.0
        DO I=1,N,1
          QSUM=QSUM + ARRPO(I)*COSRD(2.*ARRPA(I))
          USUM=USUM + ARRPO(I)*SINRD(2.*ARRPA(I))
        ENDDO
        IF (IKEY.EQ.3) THEN
          OUTPO(IS1,IS2)=SQRT( (QSUM*QSUM) + (USUM*USUM) )
        ENDIF
        PA=(180.0/3.1415926535898)*ATAN2(USUM,QSUM)
        IF (PA.LT.0.0) THEN
          PA=180.0 + PA
        ENDIF
        IF (PA.GT.180.0) THEN 
          PA=PA-180.0
        ENDIF
        OUTPA(IS1,IS2)=PA + PAOFF
      ENDIF

99    END                   


