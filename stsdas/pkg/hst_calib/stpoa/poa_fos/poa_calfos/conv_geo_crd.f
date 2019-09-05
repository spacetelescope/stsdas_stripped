      SUBROUTINE CONV_GEO_CRD(LATIN,LONIN,HEIGHT,
     $             LATOUT,LONOUT,IFLAG,IERROR)
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NEWBILCAL" program to produce IGRF geomagnetic model (GSFC)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c  ------------------------------------------------------------------------
C
C Here comes a subroutine from SuperDARN to compute Altitude-corrected 
C geomagnetic coordinates
C
C How do I convert a geographic position to AACGM (or vice versa)?
C There are two ways to do this. The direct call to the new AACGM software is:
C 
C [in FORTRAN]
C       CALL CONV_GEO_CRD(x_in_lat, x_in_long, x_height,
C                         x_out_lat, x_out_long, i_flag, i_err)
C where:
C   x_in_lat  =  input latitude (in degrees)(real single precision)
C   x_in_long =  input longitude (in degrees) (real single precision)
C   x_height  =  input altitude (in km) (real single precision)
C   x_out_lat =  returned magnetic latitude (real single precision)
C   x_out_long = returned magnetic longitude (real single precision)
C   i_flag    =  1 (32-bit integer) [flag indicating geo->AACGM conversion]
C   i_err     =  returned integer error code (0 = OK)
C
C latitude must be in the range -90 to +90.
C longitude must be in the range 0 to 360
C height must be in the range 0 to 2000
C out_lat is returned in the range -90 to + 90
C out_long is returned in the range 0 to 360
C i_flag must be 1 (geo->AACGM) or 2 (AACGM->geo)
C i_err (see below)
C
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Purpose:
C                                                                    
C         This subroutine uses a set of spherical harmonic coefficients to
C         perform a coordinate conversion between geographic and corrected
C         geomagnetic coordinates (or vice versa). 
C
C         The spherical harmonics for the geographic to corrected geomagnetic
C         coordinate system correspond to a conversion from geographic to
C         the corrected geomagnetic coordinates (expressed in terms of 
C         centered dipole coordinates at the input altitude), and are then  
C         transformed to ground level.  
C
C         The spherical harmonic coefficients used for the inverse are
C         computed relative to centered dipole coordinates at the input
C         altitude. The input CGM coordinates are converted to equivalent 
C         values in this coordinate sytem using the inverse altitude 
C         algorithm before the evaluation of the geographic spherical 
C         harmonic expansion.   
C          
C     Method:  
C
C         This subroutine uses a five-step process in converting a position
C         in one coordinate system to a position in the other. 
C         The five steps are as follows:
C
C         1.  The appropriate spherical harmonic coefficients for the  
C             coordinate conversion are computed for the input altitude.
C
C         2.  The appropriate coordinates for use in the spherical harmonic
C             expansion are computed. For the geographic ==> corrected
C             geomagnetic coordinate version, these are geographic colatitude
C             and longitude. For the inverse coordinate conversion, the 
C             input coordinates are first converted into equivalent dipole
C             coordinates at the input altitude. 
C
C         3.  The Cartesian coordinates of the unit vector in the desired
C             coordinate system are then computed using the appropriate
C             spherical harmonic expansion.
C
C         4.  For the geographic ==> corrected geomagnetic coordinate
C             conversion, the dipole-equivalent coordinates at input
C             altitude are converted to their cgm cartesian coordinates.
C
C         5.  Standard trigonometric identities are used to compute the 
C             latitude and longitude in the desired output coordinate system.
C
C     Input Arguments:
C
C         LATIN        - REAL*4 - The input latitude in degrees. 
C                        This could be either geographic latitude
C                        or corrected geomagnetic latitude.  The
C                        acceptable range of values is (-90 to +90
C                        degrees).
C
C         LONIN        - REAL*4 - The input longitude in degrees east.     
C                        This could be either geographic longitude
C                        or corrected geomagnetic longitude.  The
C                        acceptable range of values is (0 to 360 degrees).
C
C         HEIGHT       - REAL*4 - The height in kilometers for which the
C                        coordinate transformation will be accomplished.
C                        The acceptable range of values is (0 km to 
C                        2000 km)
C
C         IFLAG        - INTEGER - The flag that indicates which way
C                        the conversion will proceed.
C                     
C                        = 1  convert geographic to corrected
C                                 geomagnetic coordinates
C
C                        = 2  convert corrected geomagnetic
C                                 to geographic coordinates
C
C     Output Arguments:
C
C         LATOUT      - REAL*4 - The output latitude in degrees.
C                       This could be either the geographic latitude
C                       or corrected geomagnetic latitude.  This 
C                       value will be between -90 and +90 degrees. 
C
C         LONOUT      - REAL*4 - The output longitude in degrees.
C                       This could be either geographic longitude or
C                       corrected geomagnetic longitude.  This 
C                       value will be between 0 and 360 degrees.
C
C         IERROR      - INTEGER - The error flag 
C
C                       =  0  normal processing
C                       = -2  R_HEIGHT_IN is outside the allowable range
C                       = -4  I_FLAG value is invalid
C                       = -8  R_LAT_IN is outside the allowable range
C                       = -16 R_LON_IN is outside the allowable range
C                       = -32 Magnitude of the "unit vector" of the 
C                             target coordinate system deviates 
C                             significantly (+/- 10% or more) from 1. 
C                       = -64 For altitudes > 0, for the corrected 
C                             geomagnetic to geographic coordinate
C                             conversion there is a range of latitudes
C                             for which the transformation is invalid.
C                             This flag is set when the requested input
C                             cgm latitude falls within the invalid
C                             region.
C
C     Local Variables:
C
C         DCINT(,,)       - Double Precision Array (3-D) - 
C                            Contains the spherical harmonic coefficients
C                            interpolated to the input height, R_HEIGHT_IN.
C
C         DCOEF(,,,)      - Double Precision Array (3-D) -
C                            Contains the spherical harmonic coefficients
C                            used to compute the Cartesian coordinates of
C                            unit vector in the target coordinate system.
C
C                            First index: sp. harm. coeff. index
C                            Second       x, y, z components of unit vector
C                            Third        altitude indices 0, 300, 1200 km
C                            Fourth       direction of conversion index
C
C                            coefficients for a given altitude have the form
C                      
C                            a0 + h a1 + h * h a2 where h = alt/1000 [km] 
C
C         DCOLTEMP         - Double Precision - colatitude (radians) in the
C                            input coordinate system
C
C         DLONTEMP         - Double Precision - longitude (radians) in the
C                            input coordinatesystem
C
C         DX,DY,DZ         - Double Precision - the XYZ-components of the
C                            unit radius vector in target coordinate system
C
C         DR               - Double Prec unit radius in target coord system
C
C         DYLMVAL          - Double Precision - the array of spherical
C                            harmonic basis functions evaluated at
C                            a particular colatitude and longitude.      
C
C         IERR64           - Logical - error flag of CGM2ALT routine
C
C         K,L,M            - Integer  indices of spherical harmonic functions
C
C         HEIGHTOLD(2)     - Real*4 Array  - Variable containing
C                            previous height (km) used to determine whether
C                            the interpolation to compute DCINT() from
C                            DCOEF() needs to be done.
C           
C         LATADJ           - Real*4 - result of ALT2CGM or CGM2ALT conversion.
C
C         LATALT           - Real*4 - altitude at dipole latitude.
C
C         RR               - Real*4 - magnitude of unit vector (DR in float)
C
C     Constants:
C         NAXES       - Integer  - the number of axes in a Cartesian
C                            coordinate system (3)
C         NFLAG       - Integer  - the number of coordinate trans-
C                            formation flags available (2)
C         NLEVEL      - Integer  - the number of grid levels 
C                            available.  This is the number of
C                            distinct heights for which there are
C                            spherical harmonic coefficients available
C         IORDER          - Integer  - the order of the spherical harmonic
C                            expansion used.
C
C         DEGRAD           - Double Precision  - the multiplicative
C                            conversion factor for converting degrees
C                            to radians
C
C         PI               - Double Precision  - mathematical constant
C      
C     Subroutines Required:
C
C         RYLM              - This subroutine returns the spherical harmonic
C                            function value array for a given colatitude and 
C                            longitude.  The spherical harmonics are returned
C                            in the D_YLMVAL array. 
C
C         CG_ALT_DIP       - Given altitude (km), latitude, and a direction
C                            flag, returns altitude corrected latitude, and
C                            an error flag. See comments in subroutine for
C                            additional information.
C
C     Files Used at Compile Time: 
C     Files Used at Run Time:  None
C     Databases Accessed:  None
C
C     Warnings:  
C
C     1. For certain values of Corrected Geomagnetic Coordinates, the 
C        transformation to Geographic Coordinates (Geocentric) is
C        undefined. When this situation occurs, an error flag is set
C        IERROR = -64 and returned to the calling routine.
C
C     Revision History: 
C        Original Routine was converted to a standard library
C        subroutine for SFC$$ by MFS on  14 OCT 1992.
C
C     09/17/93 SPR1993-0203 (SWS) The return condition value needs to 
C             be initialized before coordinate conversion processing begins
C
C     08/18/93 SPR1993-0175 (SWS) Routine performed a comparison of
C             Z with a -1.D0. The variable Z should be D_Z.
C
C     The present routine represents the use of an improved algorithm for
C     the computation of corrected geomagnetic coordinates and (where
C     it exists) the inverse, together with the use of an updated
C     magnetic field model (IGRF 1995). 
C
C     The present routine incorporates the functionality of the previous
C     routine, and, uses the same subroutine calls. Relevent portions of
C     the old FORTRAN code have been retained where possible.
C
C     The new algorithm and the current code have been developed by 
C     Radex, Inc, 3 Preston Court, Bedford, MA 01730. A technical 
C     report describing the improved algorithm, and discussing its
C     limitations is in preparation, and will be published as a 
C     Philips Laboratory technical report entitled:.
C
C     An Improved Algorithm for the Computation of Corrected Geomagnetic
C     Coordinates, by K. Bhavnani and C. Hein.
C
C     The principal changes implementing the new algorithm and code are
C     as follows:
C
C        (1) The set of spherical harmonic function values are computed 
C            recursively using a single call to a new subroutine RYLM  
C            rather than using repetitive function call. 
C
C            Computationally this method is much faster than computing 
C            each of the spherical harmonics as they are needed.
C
C        (2) The new spherical harmonic coefficients are based upon 
C            the IGRF 95 magnetic field model. The order of the
C            spherical harmonic expansion used here is 10 (previously 
C            was 4).
C
C        (3) The coefficients represent a quadratic fit to the spherical
C            harmonic fits at 0, 300 and 1200 km altitude. The allowable  
C            range of altitudes is 0 - 2000 km. The previous version used
C            polynomial interpolations of the coefficients at 0, 150,
C            300 and 450 km. 
C  
C        (4) The spherical harmonic coefficients were computed in an
C            auxilliary coordinate system, which is aligned with the 
C            desired target coordinate system. This results in a
C            considerable simplification of the code, eliminating the 
C            need to perform multiple coordinate system rotations. 
C
C     17/07/95
C
C     MODIFIED FOR THE IGRF 95 MAGNETIC FIELD MODEL. 
C
C     CHANGES: BLOCKDATA SECTION HAS BEEN REPLACED WITH COEFFICIENTS
C              APPROPRIATE FOR THE IGRF 1995 MODEL
C
C              REFERENCES TO IGRF 90 IN THE COMMENT LINES HAVE BEEN
C              CHANGED TO IGRF 95.
C
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   RCS revision log for POSIX systems
c
c $Log: conv_geo_crd.f,v $
c Revision 1.1.1.1  2000/07/12 19:25:10  ecf-poa
c stpoa branch with a close version to the first release
c
c Revision 2.2  2000/07/11 12:26:29  aalexov
c Updated all the code to have a history (new history for poa
c code) and appended a history for calfos files which were
c updated by us.  Also deleted .f files from the reposutory
c which were no longer being used (cleaning up).
c
c Revision 2.1  2000/07/10 19:06:37  aalexov
c These are Michael's changes, for the release.  Comments were
c added for the headers of all poa new files.  Also, new files
c were created in order to make sure that all subroutines were
c in their own files.  mkpkg was updated to look "clean".
c
c Revision 2.0  2000/05/02 14:52:02  mrosa
c Re-delivery of POA_calfos from April 5, 2000
c
c Revision 1.1  1999/12/07 13:22:58  aalexov
c updates to calfos using Michael Rosa's flight software code and
c POA code;  data files are now preprocessed with additions of new
c group parameter keywords which are updated (per group) using these
c new code additions.  the new pieces of code calculate the
c hst positions, velocities, geomagnetic vectors and pixel offsets in
c x and y;  the code places these new values in the new header keywords.
c the results are also printed to the screen.  new "calibration" tables
c are used (norad and dgrf files).
c
c Revision 1.6  1996/03/20  22:53:03  baker
c Explicitly declared the sizes of the arguments to the
c subroutine.  This should help avoid compatibility
c problems when mixing C and Fortran with different
c machines and different compilers.
c
c Revision 1.5  1996/03/12  18:59:18  baker
c Added code to force a recomputation of the
c conversion coefficients at a given ghheight if the
c coordinates model has changed from the last call.
c
c Revision 1.4  1996/03/11  19:25:36  baker
c Modifications for the 1995 version of AACGM.
c
c Revision 1.3  94/10/17  12:35:32  12:35:32  baker (Kile Baker S1G)
c added error code -64 to indicate invalid magnetic coordinates specified
c as input.  This also requires a change in the call to cg_alt_dip
c 
c Revision 1.2  94/10/14  10:53:36  10:53:36  baker (Kile Baker S1G)
c Added the SAVE instruction to make variables static
c 
c Revision 1.1  94/10/12  15:28:38  15:28:38  baker (Kile Baker S1G)
c Initial revision
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE 
      SAVE    !make all variables static
C
      INTEGER IORDER,IERROR,IFLAG,NTERMS,NAXES,NLEVEL,NFLAG

      REAL*8 DCINT(121,3,2)
      REAL*8 DCOEF(121,3,3,2)
      REAL*8 DYLMVAL(121) 
c
      REAL*8 DCOLTEMP,DLONTEMP
      REAL*8 COLATI,COLATO,DLONI,DLONO
      REAL*8 DX,DY,DZ,DR  
      REAL*8 PI, DEGRAD
      REAL*8 FCOEFFOLD

      REAL*4 HEIGHT,LATIN,LONIN,LATOUT,LONOUT,LATADJ,LATALT
      REAL*4 HEIGHTOLD(2)
      REAL*4 altvar, altvarsq,rr 
      INTEGER i,j,k,l,m
      LOGICAL       IERR64
C                                                               
      COMMON /IGRCOEFF/ DCOEF

      DATA IORDER /10/      ! Set the order used in this algorithm to 10. 
C         Parameterize the maximum values of the indices of the
C         spherical harmonic coefficient array D_COEF( , , , ) and the
C         interpolated spherical harmonic coefficient array, D_CINT( , , ).
      DATA NTERMS,NAXES,NLEVEL,NFLAG /121,3,3,2/
c      REAL*8 DCINT(NTERMS,NAXES,NFLAG)
c      REAL*8 DCOEF(NTERMS,NAXES,NLEVEL,NFLAG)
c      REAL*8 DYLMVAL(NTERMS)       
      DATA PI /3.141592656589793D0/
      DATA DEGRAD /1.745329251994330D-2/
      DATA HEIGHTOLD,FCOEFFOLD/-1.,-1.,-1.D0/ !Initialize to impossible values.
C
C     The COMMON block / SPH_HARM_MODEL / array D_COEF contains
C     the spherical harmonic coefficients used to generate the 
C     Cartesian coordinates of the unit vector in the target coordinate
C     system.  The target coordinate system is the system to which
C     the algorithm is converting and is the coordinate system 
C     corresponding to the output variables, R_LAT_OUT and R_LON_OUT. 
C         
C     The coefficient set is stored in a 4-dimensional array with
C     indices defined as follows:
C
C         First Index  - Represents the number of terms used in the 
C                        spherical harmonic expansion. Equal to
C                        (I_ORDER + 1) * (I_ORDER + 1)
C
C         Second Index - Represents the Cartesian coordinate a particular 
C                        coefficient will be used to generate.  Indices are
C                        defined as follows:
C
C                        1  - X-coordinate (the X-axis points from the
C                             center of the earth to where the prime
C                             meridian crosses the equator.
C
C                        2  - Y-coordinate (the Y-axis points from the
C                             center of the earth to 90 degrees east
C                             of the X-axis)
C
C                        3  - Z-coordinate (the Z-axis points from the
C                             center of the earth to the north pole)  
C 
C         Third Index  - Represents the terms of a quadratic fit to
C                        altitude (independent variable h = 
C                        altitude [km]/1200) for a given spharical 
C                        harmonic coefficient. The indices are defined 
C                        as follows:
C 
C                        1  - Constant term: corresponds to the fit at
C                             0 km altitude
C                        2  - linear term
C                        3  - quadratic term
C
C         Fourth Index - Represents the direction of the coordinate 
C                        transformation.  Indices are defined as follows:
C
C                        1  - Conversion of geographic coordinates to 
C                             corrected geomagnetic coordinates.
C
C                        2  - Conversion of corrected geomagnetic coordinates
C                             to geographioc coordinates.  
C
C
C     The Data for D_COEF is provided in a BLOCK DATA file (see below)    
c
C     Initialize the error return condition indicator
      IERROR = 0
      PI = 4.0D0*DATAN(1.0D0)

C
C This IF statement checks to see if the magnetic
C     coordinates model has been changed.  If so, we
C     have to force the recalculation of the conversion
C     coefficients, by resetting the values of the old height
      IF ( FCOEFFOLD .NE. DCOEF(1,1,1,1)) THEN
         HEIGHTOLD(1) = -1
         HEIGHTOLD(2) = -1
      ENDIF
      FCOEFFOLD = DCOEF(1,1,1,1)
C
C Checkin input arguments to ensure they are within allowable ranges.
      IF ((HEIGHT .LT. 0.) .OR. (HEIGHT .GT. 2000.)) THEN
C     The height in kilometers is outside the allowable range.
        IERROR = -2  
        RETURN
      ELSEIF ((IFLAG .LT. 1) .OR. (IFLAG .GT. 2)) THEN  
C     The conversion flag is neither 1 nor 2. 
        IERROR = -4  
        RETURN
      ELSEIF (ABS(LATIN) .GT. 90.) THEN 
C     The latitude is outside the allowable range.
        IERROR = -8 
        RETURN
      ELSEIF ((LONIN .LT. 0.) .OR. (LONIN .GT. 360.)) THEN 
C     The longitude is outside the allowable range. 
        IERROR = -16
        RETURN
      ENDIF
C All input arguments are within allowable ranges.
C                                                                     
C Compute Spherical Harmonic Coefficients for current altitude if required
      IF (HEIGHT .NE. HEIGHTOLD(IFLAG)) THEN   
        ALTVAR    = HEIGHT/1200.0
        ALTVARSQ  = ALTVAR * ALTVAR
        DO 20 I = 1, 3
         DO 10 J = 1, 121
           DCINT(J,I,IFLAG) = DCOEF(J,I,1,IFLAG) + ALTVAR * 
     $     DCOEF(J,I,2,IFLAG) + ALTVARSQ *  DCOEF(J,I,3,IFLAG)
 10        CONTINUE
 20     CONTINUE
        HEIGHTOLD(IFLAG) = HEIGHT  
      ENDIF    
C
C Zero Sums for Spherical Harmonic Expansion Computation
      DX = 0.0D0                                                    
      DY = 0.0D0                                          
      DZ = 0.0D0                                                           
C
C Prepare for Spherical Harmonic Expansion Computation
      DLONI   = DBLE(LONIN) * DEGRAD
      IF (IFLAG .EQ. 1) THEN
C     Computing CGM from Geographic Coordinates. No altitude
C     correction required
        COLATI = (90.0D0 - DBLE(LATIN)) * DEGRAD            
      ELSE
C     Computing Geographic Coordinates from CGM Coordinates. 
C     Convert CGM Latitude to Dipole Latitude at Altitude HEIGHT
        CALL CGM2ALT(HEIGHT,LATIN,LATADJ,IERR64)
        IF (IERR64 .EQV. .TRUE.) THEN
            IERROR = -64  
            RETURN
        ENDIF
        COLATI = (90.0D0 - DBLE(LATADJ)) * DEGRAD            
      ENDIF         
C
C Generate the spherical harmonics at the coordinate point
      CALL RYLM(COLATI,DLONI,IORDER,DYLMVAL) 
C
C Calculate cartesian coordinates of unit vector in target coordinate system.
      DO 40 L= 0, IORDER                                        
        DO 30 M = -L, L     
          K = L * (L + 1) + M + 1 
C         Add the contribution of each spherical harmonic to the 
C         Cartesian components of the unit vector in the
C         appropriate coordinate system. 
          DX = DX + DCINT(K,1,IFLAG) * DYLMVAL(K)        
          DY = DY + DCINT(K,2,IFLAG) * DYLMVAL(K)                     
          DZ = DZ + DCINT(K,3,IFLAG) * DYLMVAL(K)  
 30     CONTINUE                                                            
 40   CONTINUE 
C             
C Compute magnitude of Cartesian unit vector of magnetic dipole coord system.
      DR = SQRT(DX * DX + DY * DY + DZ * DZ) 
C       If the magnitude of the unit vector differs significantly 
C       from 1, set the error flag and continue processing.
        IF ((DR .LT. 0.9D0) .OR. (DR .GT. 1.1D0)) THEN          
          RR = SNGL(DR)
          IERROR = -32
          RETURN                                   
        ENDIF 
C         
C Adjust the components so they do represent the components of
C       a unit vector.  If DR is equal to 1.0D0, this step will not 
C       change the values of D_X, D_Y, or D_Z. 
        DZ = DZ / DR                                                      
        DX = DX / DR                                               
        DY = DY / DR                                                   
C          
C Obtain output co_latitude and longitude from the unit              
C           vector using standard formulas
        IF (DZ .GT. 1.0D0) THEN                                     
          DCOLTEMP = 0.0D0                                   
        ELSE IF (DZ .LT.- 1.0D0) THEN                    
          DCOLTEMP = PI                                
        ELSE                                                   
          DCOLTEMP = DACOS(DZ)                             
        ENDIF 
c
        IF ((ABS(DX) .LT. 1.0D-8) .AND. (ABS(DY) .LT. 1.0D-8)) THEN        
          DLONTEMP = 0.0D0                                                
        ELSE                                                    
          DLONTEMP = DATAN2(DY,DX)                              
        ENDIF 
C
C Prepare Latitude Data for Output     
       DLONO   = DLONTEMP
       IF (IFLAG .EQ. 1) THEN  
         LATALT = 90.0 - DCOLTEMP/DEGRAD
         CALL ALT2CGM(HEIGHT,LATALT,LATADJ)
         COLATO = (90.0D0 - DBLE(LATADJ)) * DEGRAD            
       ELSE
         COLATO = DCOLTEMP
       ENDIF         
C         
C Convert colatitude into latitude and convert both coordinates
C     from REAL*8 radians to REAL degrees. 
      LATOUT = SNGL(90.0D0 - COLATO / DEGRAD)          
      LONOUT = SNGL(DLONO / DEGRAD)
      IF (LONOUT .LT. 0.0) LONOUT = LONOUT + 360.   
C                                                            
      RETURN
c90    FORMAT(' INVERSE UNDEFINED AT CGM LAT ',F6.2,' CGM LON ',F7.2,
c     $ ' ALTITUDE ',F8.2)
      END
