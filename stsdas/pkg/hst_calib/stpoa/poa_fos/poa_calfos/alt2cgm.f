      SUBROUTINE ALT2CGM(HEIGHT,LATALT,LATADJ)

c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NEWBILCAL" program to produce IGRF geomagnetic model (GSFC)
c  ------------------------------------------------------------------------
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c *******************************************************************
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
c $Log: alt2cgm.f,v $
c Revision 1.1.1.1  2000/07/12 19:25:10  ecf-poa
c stpoa branch with a close version to the first release
c
c Revision 2.2  2000/07/11 12:26:27  aalexov
c Updated all the code to have a history (new history for poa
c code) and appended a history for calfos files which were
c updated by us.  Also deleted .f files from the reposutory
c which were no longer being used (cleaning up).
c
c Revision 2.1  2000/07/10 19:06:36  aalexov
c These are Michael's changes, for the release.  Comments were
c added for the headers of all poa new files.  Also, new files
c were created in order to make sure that all subroutines were
c in their own files.  mkpkg was updated to look "clean".
c
c Revision 2.0  2000/05/02 14:51:49  mrosa
c Re-delivery of POA_calfos from April 5, 2000
c
c Revision 1.1  1999/12/07 13:22:57  aalexov
c updates to calfos using Michael Rosa's flight software code and
c POA code;  data files are now preprocessed with additions of new
c group parameter keywords which are updated (per group) using these
c new code additions.  the new pieces of code calculate the
c hst positions, velocities, geomagnetic vectors and pixel offsets in
c x and y;  the code places these new values in the new header keywords.
c the results are also printed to the screen.  new "calibration" tables
c are used (norad and dgrf files).
c
c Revision 1.1  1996/03/11  19:23:22  baker
c Initial revision
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Purpose:      
C     Computes Latitude Adjustment for R_HEIGHT_IN for use with the 
C     Spherical Harmonic expansion computation.  
C
C     Input Arguments:
      REAL*4  HEIGHT  ! Height in km above earth mean radius  
      REAL*4  LATALT  ! RHEIGHT Corrected Dipole Latitude 
C         
C     Output Arguments:
      REAL*4  LATADJ    !Corrected Geomagnetic Latitude for IFLAG = 1
C                             Height Adjusted Corrected Dipole Coordinates
C                             for IFLAG = 2
C
C     Local Variables:
      REAL*4    RA, R0       ! Single Precision - for intermediate results
C  
C     Constants:
      REAL*8  DEGRAD    !Conversion Factor degrees to radians
      REAL*4 ERADIUS               !Earth Radius in km
      REAL*4 EPS,UNIM              !Limit parameter, used to 
C                                   avoid computational difficulties
C
C     Written by Radex, Inc., 3 Preston Court, Bedford, MA 01730 12/94
C
      SAVE
      DATA DEGRAD  / 1.745329251994330D-2/
      DATA ERADIUS / 6371.2 / 
      DATA EPS     / 1.0E-9 /
      DATA UNIM    / 0.9999999 /
C
C     input LATALT is an altitude adjusted dipole latitude. The 
C     following code computes the corresponding 0 km altitude dipole 
C     latitude (CGM latitude)
      RA = (COS(DEGRAD * LATALT))**2
      IF (RA .LT. EPS) RA = EPS
      R0 = (1.0 + HEIGHT/ERADIUS)/RA
      IF (R0 .LT. UNIM) R0 = UNIM
      LATADJ = SIGN(ACOS(SQRT(1.0/R0)), LATALT)/DEGRAD
C
      RETURN
      END

