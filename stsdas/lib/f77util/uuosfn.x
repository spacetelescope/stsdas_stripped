include <fio.h>
include <iraf77.h>


# VFNAME -- Translate an iraf virtual file name into host system filename
#	  Code contribution of Bernie Simon. 8 Feb 1988
#	  Modified to complied with iraf77/vos standard. Nelson Zarate Apr 88.

procedure uuosfn (vfname, osfile, istat)

%	character*(*)	 vfname
%       character*(*)    osfile
int	istat

char    ch
int     ic, vstatus
pointer sp, virtual_name, host_name, vp

int     vfnmapu(), stridx()
pointer vfnopn()

begin
	istat = ER_OK
        # Allocate space for SPP file names

        call smark (sp)
        call salloc (virtual_name, SZ_LINE, TY_CHAR)
        call salloc (host_name, SZ_LINE, TY_CHAR)

        # Convert file name from Fortran 77 string to SPP string

        call f77upk (vfname, Memc[virtual_name], SZ_LINE)
        call strlwr (Memc[virtual_name])

        # Convert virtual file name to host system name. If an error occurs
        # just do a straight copy. Strip the node name from the host name
        # (delimeted by a '!')

        ic = 0
        ch = '!'
        vp = vfnopn (Memc[virtual_name], VFN_WRITE)

        if (vp == ERR) {
            call strcpy (Memc[virtual_name], Memc[host_name], SZ_LINE)
	    istat = ER_IMOPOLD
        } else {
            vstatus = vfnmapu (vp, Memc[host_name], SZ_LINE)
            if (vstatus == ERR) {
                call strcpy (Memc[virtual_name], Memc[host_name], SZ_LINE)
	        istat = ER_IMOPOLD
            } else {
                call vfnclose (vp, VFN_UPDATE)
                ic = stridx (ch, Memc[host_name])
            }
        }

        call f77pak (Memc[host_name+ic], osfile, SZ_LINE)
        call sfree (sp)
end
