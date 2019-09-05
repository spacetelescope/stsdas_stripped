      SUBROUTINE NORAD_TLES(TLEBUF,ISTAT)
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NORAD TLES" based ephemeris program (NASA)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c      1.2       Jun 01  A. Alexov       Added IDS's common block for file
c                                        header access
c  ------------------------------------------------------------------------
C     15 Jul 00                    reads from STSDAS table 
c
c     This routine initializes the tles
      INTEGER J
      INTEGER ISTAT
 
c      CHARACTER ABUF*80
      CHARACTER*80 TLEBUF(6830)
c      INTEGER*4 IEXP,IBEXP
c      REAL*8 EPOCH,XNDT2O,XNDD6O,BSTAR,XINCL,XNODEO,EO,OMEGAO,XMO,XNO 
      
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)

C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
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

C local variables
C
c      INTEGER IDIN,COLIDS(12),ROW,IDET,INTD,I,ISTATS(12),NROWS,II
      INTEGER IDIN,COLIDS(12),I,ISTATS(12),NROWS,II
      CHARACTER*80 CONTXT
      CHARACTER*17 COLNAM(12)
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
      LOGICAL NULL
      INTEGER*4 D2EXP, BSTAREX
      REAL*8 DATE, D1, D2, BSTARD
      REAL*8 INCL, RAAN, ECC, AOP, MA, RPD
      CHARACTER*64 NORFILE
      CHARACTER*8  HEADER_KEY_NOR
      DATA COLNAM/'DATE','TIME_DERIV1','TIME_DERIV2','TIME_DERIV2_EXPO',
     *            'BSTAR_DRAG','BSTAR_DRAG_EXPO','INCLINATION',
     *            'RA_ASEN_NODE','ECCENTRICITY', 'ARG_PERIGEE',
     *            'MEAN_ANOMALY', 'REV_PER_DAY'/
      DATA HEADER_KEY_NOR/'CYCCSFR'/



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc          National Aeronautics and Space Administration              Page: 1
cc                   Orbital Information Archive                    07/30/1998
cc                 Requested by: Dr Michael R Rosa                    10:42:03
cc                         Satellite: 20580                                   
c
c
c
cc       From: 01/01/1958 (58001) through: 07/29/1998 (98210)                 
cc1 20580U 90037B 90115.58382970 -.00023887 +00000-0 -28316-2 0 00018
c   Sat #  Launch Epochtime       Drag       Drag     Drag    Elemnt set #
cc2 20580 028.4677 224.1406 0005588 265.4255 094.5690 14.84116207000169
c         i        RASCN   e         argper M        Orb 
c         D        D       D         D      D        (rev/day)   rev#  chksum
cc..........
cc1 20580U 90037B   98202.08071191 +.00000651 +00000-0 +55052-4 0 01201
cc2 20580 028.4679 011.5693 0014298 018.6082 341.5022 14.86815717252472
cc     Catalog #: 20580    3415  two line element sets found!
cNew TLE format(standard)
c 
c23635B
c1 23635U 95040B   95215.97504380 -.00000165 +00000-0 +10000-3 0 00018
c2 23635 004.1663 111.0618 7252147 179.2403 004.8644 02.18377056000000
c 
cBreak-out of a Two Line Element
c 
c23635B  =  Name of this two line element set of three lines
c 
c!>Line Number
c!   !>Catalog Number
c!   !  !>Security Classification for this Element Set
c!   !  !    !>International Identification for this Object
c!   !  !    !     !>Two Digit Year
c!   !  !    !     !  !>Day of Year
c!   !  !    !     !  !     !>Fraction of 24 Hour Day
c!   !  !    !     !  !     !     !>Sign of 1st Time Derivative
c!   !  !    !     !  !     !     !    !>1st Time Derivative
c!   !  !    !     !  !     !     !    !     !>Sign of 2nd Time Derivative
c!   !  !    !     !  !     !     !    !     !  !>2nd Time Derivative
c!   !  !    !     !  !     !     !    !     !  !  !>Sign of exponent
c!   !  !    !     !  !     !     !    !     !  !   !>Exponent 2ndTimeDerivative
c!   !  !    !     !  !     !     !    !     !  !     !>Sign of BSTAR drag term
c!   !  !    !     !  !     !     !    !     !  !     !  !>BSTAR/Drag Term
c!   !  !    !     !  !     !     !    !     !  !     !  !  !>Eign of Exponent
c!   !  !    !     !  !     !     !    !     !  !     !  !  !!>Exponent
cBstarDrg
c!   !  !    !     !  !     !     !    !     !  !     !  !  !! !>Ephemeris Type
c!   !  !    !     ! ---    !     !    !     !  !     !  !  !! ! !>Element No.
c! -----! -------- --|||--------- !--------- !-----   !-----!! ! ----!>Checksum
c1 23635U 95040B   95215.97504380 -.00000165 +00000-0 +10000-3 0 00018 = Line 1
c 
c2 23635 004.1663 111.0618 7252147 179.2403 004.8644 02.18377056000000 = Line 2
c 
c123456789012345678901234567890123456789012345678901234567890123456789
c! ----- ---.---- ---.---- ------- ---.---- ---.---- --.--------|||||!>Checksum
c!   !   Incl     RAAN     Ecc     AoP      MA       RpD        -----
c!   !   !        !        !       !        !        !          !>Rev # @ Epoch
c!   !   !        !        !       !        !        !>Revolutions Per Day
c!   !   !        !        !       !        !>Mean Anomaly
c!   !   !        !        !       !>Argument of Perigee
c!   !   !        !        !>Eccentricity, with assumed decimal point leading
c!   !   !        !>Right Asencion of Ascending Node
c!   !   !>Inclination
c!   !>Catalog Number
c!>Line Number
c 
cLine 1
c 
c   Column Numbers     Number of
c   First     Last     Characters     Description
c     1        1           1          Line No. Identification
c     3        7           5          Catalog No.
c     8        8           1          Security Classification
c    10       17           8          International Identification
c    19       32          14          YRDOY.FODddddd
c    34       34           1          Sign of first time derivative
c    35       43           9          1st Time Derative
c    45       45           1          Sign of 2nd Time Derivative
c    46       50           5          2nd Time Derivative
c    51       51           1          Sign of 2nd Time Derivative Exponent
c    52       52           1          Exponent of 2nd Time Derivative
c    54       54           1          Sign of Bstar/Drag Term
c    55       59           5          Bstar/Drag Term
c    60       60           1          Sign of Exponent of Bstar/Drag Term
c    61       61           1          Exponent of Bstar/Drag Term
c    63       63           1          Ephemeris Type
c    65       68           4          Element Number
c    69       69           1          Check Sum, Modulo 10
c 
cLine 2
c 
c   Column Numbers     Number of
c   First     Last     Characters     Description
c     1        1           1          Line No. Identification
c     3        7           5          Catalog No.
c     9       16           8          Inclination
c    18       25           8          Right Asencion of Ascending Node
c    27       33           7          Eccentricity with assumed leading decimal
c    35       42           8          Argument of the Perigee
c    44       51           8          Mean Anomaly
c    53       63          11          Revolutions per Day
c    64       68           5          Revolution Number at Epoch
c    69       69           1          Check Sum Modulo 10
c 
cMore Definitive definituions;
c 
cSign digit where used; Only negative values are flagged with a "minus",
c     positive values have a "space".
c 
cFirst time  derivative of the mean motion or ballistic coefficient (depending
con Ephemeris Type).  Revolution per day-squared or meters-squared per
ckilogram.
c  ------------------------------------------------------------------------
cc                                 Last page                                  
cc                                 ---------                                  
cc          National Aeronautics and Space Administration              Page: 1
cc                   Orbital Information Archive                    07/30/1998
cc                 Requested by: Dr Michael R Rosa                    10:42:03
cc                          Query Summary                                     
cc
cc          CatNo  From Date    To Date    Total
cc          ----- ---------- ---------- --------
cc          20580 01/01/1958 07/29/1998     3415
cc                                      --------
cc                         Query total:     3415
cc                                 Last page                                  
cc                                 ---------                                  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

CCC AA - closing out this section since the table was changed
CCC to be the standard stsdas format

cc jump over the first 5 lines and print
c      OPEN(29,FILE='/home/ecf-poa/cal/noradtles.dat',err=999) 
c      DO J = 1,6  
c        READ(29,706) ABUF 
c        WRITE(6,706) abuf
c      ENDDO
C read the elements ascii records (2 per epoch)
c      J = 0
c 123  J = J+1 
c      IF (J.LT.6829) THEN
c        READ (29,706) TLEBUF(J) 
c        DECODE(61,761,TLEBUF(J)) EPOCH,XNDT2O,XNDD6O,IEXP,BSTAR,IBEXP
c        J = J+1 
c        READ (29,706) TLEBUF(J) 
c        DECODE(63,763,TLEBUF(J)) XINCL,XNODEO,EO,OMEGAO,XMO,XNO 
cc        type *,EPOCH,XNDT2O,XNDD6O,IEXP,BSTAR,IBEXP,XINCL,
cc     &                 XNODEO,EO,OMEGAO,XMO,XNO 
cc         write(33,733) epoch,XNDT2O,XNDD6O,IEXP,
cc     1     INT(BSTAR),IBEXP,XINCL,XNODEO,EO,OMEGAO,XMO,XNO
cc 733     FORMAT(1x,f14.8,f11.8,1x,i6,1x,i2,1x,
cc     1             i6,1x,i2,f8.4,f9.4,f9.7,2(f9.4),f12.8)
c        GOTO 123
c       ENDIF
c        
c      DO J = 1,15  
c        READ(29,706,END=888) ABUF 
cc        WRITE(6,706) abuf
c      ENDDO
c
c 888  close(29)
cc      return 
c
cc 999  stop
c706   FORMAT(A80)
c 761  FORMAT(18X,D14.8,1X,F10.8,2(1X,F6.5,I2))
c 763  FORMAT(7X,2(1X,F8.4),1X,F7.7,2(1X,F8.4),1X,F11.8)
cc      end


C
C Open the .d0h header, to get the name of the NORAD tables
       CALL UHDGST(IDS(1), HEADER_KEY_NOR, NORFILE, ISTAT)

C
C Check for errors encountered when reading keywords
C
       IF (ISTAT .NE. 0) THEN
           CONTXT='ERROR: reading .d0h keyword '//HEADER_KEY_NOR
           GO TO 999
       END IF

C Open table
C
        CALL UTTOPN(NORFILE,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening NORAD table '//NORFILE
                GO TO 998
        ENDIF
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading noradtles table '//NORFILE
                GO TO 999
        ENDIF

C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,12,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in norad table '//
     *                  NORFILE
                GO TO 999
        ENDIF

C
C read values
C
        J=1
        DO 50 II=1,NROWS
           CALL UTRGTD(IDIN,COLIDS(1),1,II,DATE,NULL,ISTATS(1))
           CALL UTRGTD(IDIN,COLIDS(2),1,II,D1,NULL,ISTATS(2))
           CALL UTRGTD(IDIN,COLIDS(3),1,II,D2,NULL,ISTATS(3))
           CALL UTRGTI(IDIN,COLIDS(4),1,II,D2EXP,NULL,ISTATS(4))
           CALL UTRGTD(IDIN,COLIDS(5),1,II,BSTARD,NULL,ISTATS(5))
           CALL UTRGTI(IDIN,COLIDS(6),1,II,BSTAREX,NULL,ISTATS(6))
           CALL UTRGTD(IDIN,COLIDS(7),1,II,INCL,NULL,ISTATS(7))
           CALL UTRGTD(IDIN,COLIDS(8),1,II,RAAN,NULL,ISTATS(8))
           CALL UTRGTD(IDIN,COLIDS(9),1,II,ECC,NULL,ISTATS(9))
           CALL UTRGTD(IDIN,COLIDS(10),1,II,AOP,NULL,ISTATS(10))
           CALL UTRGTD(IDIN,COLIDS(11),1,II,MA,NULL,ISTATS(11))
           CALL UTRGTD(IDIN,COLIDS(12),1,II,RPD,NULL,ISTATS(12))
           DO 30 I=1,12
               IF(ISTATS(I).NE.0)THEN
                   CONTXT='ERROR reading norad table '//NORFILE
                   GO TO 999
               ENDIF
30         CONTINUE
C          write the value to the buffer
           WRITE(TLEBUF(J),709)DATE,D1,D2,D2EXP,BSTARD,BSTAREX
           J=J+1
           WRITE(TLEBUF(J),708)INCL,RAAN,ECC,AOP,MA,RPD
           J=J+1
50      CONTINUE

C
        CALL UTTCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000

 708    FORMAT(7X,2(1X,F8.4),1X,F9.7,1X,F8.4,1X,F10.4,1X,F11.8)
 709    FORMAT(18X,F14.8,1X,F10.8,1X,F6.5,1X,I2,1X,F8.5,1X,I2)


999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
