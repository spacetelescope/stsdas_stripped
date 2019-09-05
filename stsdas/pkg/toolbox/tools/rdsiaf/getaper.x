#* HISTORY *
#* B.Simon	09-Mar-89	Original
#* B.Simon	19-Jun-92	Rewritten to produce an sdas table
#* B.Simon	12-Nov-97	modified to remove cdbs dependencies

# GETAPER -- Get the aperture parameters from the SIAF file

int procedure getaper (fd, tp, pdbdate)

int	fd		# i: The file descriptor of the SIAF file
pointer	tp		# i: The output table descriptor
char	pdbdate		# o: The date read from the SIAF file
#--
bool	sias_flag
int	status, npoly, idx, jdx, icol
pointer	sp, shape, colstr

string	aq1cols  "vrt1_x,vrt1_y,vrt2_x,vrt2_y"
string	aq2cols  "vrt3_x,vrt3_y,vrt4_x,vrt4_y"
string	apcols   "in_rot_ang,in_ang_ext,out_rot_ang,out_ang_ext"
string	akcols   "a%d%d,b%d%d,ac%d%d,bc%d%d"
string	amcols   "disp_v2_ref,disp_v3_ref,disp_scale_x,disp_scale_y"
string	ancols   "disp_beta_1,disp_beta_2,disp_x_ref,disp_y_ref"

string	fmt      "f15,f15,f15,f15"

bool	streq()
int	nextaper()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (shape, SZ_LINE, TY_CHAR)
	call salloc (colstr, SZ_LINE, TY_CHAR)

	# Find the next aperture in the SIAF file

	status = nextaper (fd, tp, pdbdate, Memc[shape], sias_flag, npoly)
	if (status == EOF)
	    return (status)

	# Read records dependent on aperture shape

	if (streq (Memc[shape], "QUAD")) {
	    call readaper (fd, tp, "AQ", aq1cols, fmt)
	    call readaper (fd, tp, "AQ", aq2cols, fmt)
	} else if (streq (Memc[shape], "PICK")) {
	    call readaper (fd, tp, "AP", apcols, fmt)
	}

	# Read SIAS coordinate transformation polynomial

	if (sias_flag) {
	    do idx = 0, npoly {
		do jdx = 0, idx {
		    call sprintf (Memc[colstr], SZ_LINE, akcols)
		    do icol = 1, 4 {
			call pargi (idx)
			call pargi (jdx)
		    }
		    call readaper (fd, tp, "AK", Memc[colstr], fmt)
		}
	    }
	}

	# Read remaining records

	call readaper (fd, tp, "AM", amcols, fmt)
	call readaper (fd, tp, "AN", ancols, fmt)

	call sfree (sp)
	return (status)
end
