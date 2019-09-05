include <iraf77.h>

# TOROOT -- Parse an image name and return the root and file extension

procedure toroot(fname, root, ext, istat)

%      character*(*) fname
%      character*(*) root
%      character*(*) ext
int    istat
#--

char   sppfnm[SZ_FNAME]
char   spprt[SZ_FNAME]
char   sppext[SZ_FNAME]

char   clust[SZ_FNAME], period
int    lastp
int    strldx()

begin

   # Convert the fortran string to an spp string
   call f77upk(fname, sppfnm, SZ_FNAME)

   # get the cluster name (root.ext)
   call imgcluster(sppfnm, clust, SZ_FNAME)

   # Find the last occurance of a period in the cluster name.  Everything
   # before the period is the root, after is the extension
   period = '.'
   lastp = strldx(period, clust)
   if (lastp > 0) {
      call strcpy(clust, spprt, lastp-1)
      call strcpy(clust[lastp+1], sppext, SZ_FNAME)

   } else {
      # If there is no extension specified in the file name then
      # assume it is a c1h (default)
      call strcpy(clust, spprt, SZ_FNAME)
      call strcpy("c1h", sppext, SZ_FNAME)
   }

   call strlwr(spprt)
   call strlwr(sppext)

   call f77pak(spprt, root, 64)
   call f77pak(sppext, ext, 64)

   istat = 0
end

