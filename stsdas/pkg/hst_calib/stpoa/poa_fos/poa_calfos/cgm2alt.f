      SUBROUTINE CGM2ALT (HEIGHT,LATIN,LATADJ,IERR64)

c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NEWBILCAL" program to produce IGRF geomagnetic model (GSFC)
c  ------------------------------------------------------------------------
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Original version writen by RADEX, INC. for use on VAX/VMS systems.
c   The 1990 version of sfc_convert_geo_coord used a subroutine
c   called 'cg_alt_dip'.  This subroutine has been replaced
c   by two subroutines, cgm_to_altitude, and altitude_to_cgm.
c
c   Initial version for POSIX compliant systems made by KBB
c   at the Johns Hopkins Univ. Applied Physics Laboratory.
c   These revisions have been managed using the Revision Control
c   System (RCS).  The log of revisions follows:
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c $Log: cgm2alt.f,v $
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
c Revision 1.1  1996/03/11  19:24:31  baker
c Initial revision
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Purpose:      
C
C     Computes Latitude Adjustment for R_HEIGHT_IN for use with the 
C     Spherical Harmonic expansion computation.  
C
C     Except for low R_HEIGHT_IN, the corrected geomagnetic latitude
C     is discontinuous in the vicinity of the magnetic equator. This
C     continuity would result in poor convergence of a spherical 
C     harmonic expansion. To avoid this problem, it was necessary to
C     perform the spherical harmonic fit and expansion with respect
C     to dipole coordinates at the table R_HEIGHT_INs, and to compute
C     an appropriate latitude adjustment. This routine computes the
C     required adjustment.
C
C     Input Arguments:
      REAL*4  HEIGHT       ! Input height in km above earth mean radius
      REAL*4  LATIN        ! Corrected Geomagnetic Latitude 
C         
C     Output Arguments:
      REAL*4  LATADJ         !Height Adjusted Corrected Dipole Coordinates
C                                corresponding to R_LAT_IN
      LOGICAL IERR64           !Logical - Error Flag
C                       = .false.  normal return
C                       = .true.   the input R_LAT value for
C                         the IFLAG = 2 option is invalid for the
C                         input altitude R_HEIGHT_IN.  
c
C     Local Variables:
      REAL*4   RA           !Single Precision - for intermediate results
C
C     Constants:

      REAL*8    DEGRAD      !Conversion Factor degrees to radians
      REAL*4  ERADIUS                  !Earth Radius in km
      REAL*4  UNIM        !limit parameter, used to
C                       avoid computational singularities
C     Revision History
C     Written by Radex, Inc., 3 Preston Court, Bedford, MA 01730 12/94
C
      SAVE
      DATA DEGRAD  / 1.745329251994330D-2/
      DATA ERADIUS / 6371.2 / 
      DATA UNIM    / 1.0 /
      IERR64 = .false.
C
C     Compute the corresponding altitude adjusted dipole latitude.
      RA = (1.0 + HEIGHT/ERADIUS) * (COS(DEGRAD * LATIN))**2
      IF (RA .GT. UNIM) THEN 
         RA = UNIM
         IERR64 = .TRUE.
      ENDIF
      LATADJ = SIGN(ACOS(SQRT(RA)), LATIN)/DEGRAD
C
      RETURN
      END
