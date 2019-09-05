# nc_clgen - Generate a resonable set of contours.
#
# Description
#   nc_clgen puts the values of the contour levels in cl.
#   variable names match those in conrec, with the following additions.
#         ncl     -number of contour levels put in cl.
#         icnst   -flag to tell conrec if a constant field was detected.
#                 .icnst=0 means non-constant field.
#                 .icnst non-zero means constant field.
#
#   to produce non-uniform contour level spacing, replace the code in this
#   subroutine with code to produce whatever spacing is desired.
#
#   NOTE: This code is taken from the NCAR routine CONREC.
#   There is also a bit of change of functionality.  In the original,
#   if the finc parameter was the number of contours, this routine did
#   not produce levels with that number of contours, but again rounded
#   the interval to some nice number.  This has been modified to take
#   the finc parameter more literally; it produces the asked-for number
#   of contours, regardless.
#
# History
#   23Jan91 - Taken from NCAR'S CONREC source and rewritten into SPP.
#             Jonathan D. Eisenhamer, STScI.
#    4Feb91 - Changed the functionality of the finc parameter. jde
#---------------------------------------------------------------------------

procedure nc_clgen (z,mx,nx,nny,cclo,chi,cinc,nla,nlm,cl,ncl,icnst)

real z[mx,nny]  # I:  The array to be contoured.
int  mx         # I:  The declared dimension of z in the x direction.
int  nx         # I:  The number of columns to be contour (in the x direction).
int  nny        # I:  The number of rows to be contour (in the y direction).
real cclo       # IO: The low value to contour.
real chi        # IO: The high value to contour.
real cinc       # IO: The encoded contour increment.  The possible values are:
                #      cinc < 0,  then -cinc is the number of contours to 
                #                 produce.
                #      cinc == 0, then figure number of contours/interval.
                #      cinc > 0,  then cinc is the interval between contours.
                #     What is returned is the final calculated interval.
int  nla        # I:  The approximate number of contours to go for if it needs
                #     to be calculated.
int  nlm        # I:  The maximum number of contours.
real cl[nlm]    # O:  The contour levels
int  ncl        # O:  The number of valid contour levels in cl.
int  icnst      # O:  Flag to indicate whether a constant field was found.
                #     Possible values:
                #       icnst == 0 then a non-constant field was found.
                #       icnst != 0 then a constant field was found.
 
# Declarations
real cc, clo, crat, fanc, glo, ha, p

int i, j, k, kk, ny

begin

  # Initialize.
  icnst = 0
  ny = nny
  clo = cclo
  glo = clo
  ha = chi
  fanc = cinc
  crat = nla

  # Make sure that the high/low values are defined.
  if( IS_INDEFR (glo) ) {
      glo = z(1,1)
      do  j=1,ny
        do i=1,nx {
          glo = amin1(z(i,j),glo)
        }
  }
  if( IS_INDEFR (ha) ) {
      ha = z(1,1)
      do  j=1,ny
        do i=1,nx {
          ha = amax1(z(i,j),ha)
        }
  }
  if ( ( ha-glo == 0. ) && ( glo != 0. ) ) {
    cl(1) = glo
    ncl = 1
  } else {
    if (ha-glo < 0. ) {
      glo = ha
      ha = clo
    } else if( ha-glo == 0. ) {
      glo = z(1,1)
      ha = z(1,1)
      do  j=1,ny
        do i=1,nx {
          glo = amin1(z(i,j),glo)
          ha = amax1(z(i,j),ha)
        }
    }

    if (glo >= ha) {
      icnst = 1
      ncl = 1
      cclo = glo
    } else {
      if (fanc < 0. ) {
        crat = amax1(1.,-fanc)
        fanc = (ha-glo)/crat
      } else if( fanc == 0. ) {
        fanc = (ha-glo)/crat
        p = 10.**(ifix(alog10(fanc)+5000.)-5000)
        fanc = aint(fanc/p)*p
      }
      if( chi-clo == 0. ) {
        glo = aint(glo/fanc)*fanc
        ha = aint(ha/fanc)*fanc*(1.+sign(1.e-6,ha))
      }
      do  k=1,nlm {
        cc = glo+float(k-1)*fanc
        if (cc > ha)
          break
        kk = k
        cl(k) = cc
      }
      ncl = kk
      cclo = cl(1)
      chi = cl(ncl)
      cinc = fanc
    }
  }

end
#---------------------------------------------------------------------------
# End of nc_clgen
#---------------------------------------------------------------------------
