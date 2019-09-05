include <tbset.h>

define	FMTSTR "pdbdate:c17,siap_id:c10,sics_v2:d,sics_v3:d,shape:c4,\
maj_axis:r,macro_flg:c1,boa_flg:c1,boa_thresh:r,macro_id:c4,min_axis:r,\
plate_scale:r,area:r,rot_angle:r,sias_flg:c1,parity:i,n_poly:i,sias_x:r,\
sias_y:r,sics_x:r,sics_y:r,vrt1_x:r,vrt1_y:r,vrt2_x:r,vrt2_y:r,vrt3_x:r,\
vrt3_y:r,vrt4_x:r,vrt4_y:r,in_rot_ang:r,in_ang_ext:r,out_rot_ang:r,\
out_ang_ext:r,disp_v2_ref:d,disp_v3_ref:d,disp_scale_x:r,disp_scale_y:r,\
disp_beta_1:r,disp_beta_2:r,disp_x_ref:r,disp_y_ref:r"

#* HISTORY *
#* B.Simon	19-Jun-92	Original

# SIAFTAB -- Create the columns in the siaf output table

procedure siaftab (tp)

pointer	tp		# i: table descriptor
#--
char	fmt[1], units[1]
int	ic, dtype, idx, jdx
pointer	sp, fp, cp, col, temp

string	fmtlist  FMTSTR
string	matlist  "a%d%d,b%d%d,ac%d%d,bc%d%d"

int	rd_fmtlist(), word_fetch()
pointer	opn_fmtlist()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (col, SZ_COLNAME, TY_CHAR)
	call salloc (temp, SZ_COLNAME, TY_CHAR)

	# Read the format list, extract each name and format
	# and create a table column with that name and format

	fmt[1] = EOS
	units[1] = EOS

	fp = opn_fmtlist (fmtlist)

	while (rd_fmtlist (fp, dtype, Memc[col], SZ_COLNAME) > 0) {
	    call strupr (Memc[col])
	    if (dtype == TY_BOOL)
		dtype = -1

	    call tbcdef (tp, cp, Memc[col], units, fmt, dtype, 1, 1)
	}

	call cls_fmtlist (fp)

	# Create SIAS/SICS tranformation coefficient table columns

	ic = 1
	dtype = TY_REAL
	while (word_fetch (matlist, ic, Memc[temp], SZ_COLNAME) > 0) {
	    do idx = 0, 5 {
		do jdx = 0, idx {
		    call sprintf (Memc[col], SZ_COLNAME, Memc[temp])
		    call pargi (idx)
		    call pargi (jdx)

		    call strupr (Memc[col])
		    call tbcdef (tp, cp, Memc[col], units, fmt, dtype, 1, 1)
		}
	    }
	}

	# Create the table after all columns have been defined

	call tbtcre (tp)
	call sfree (sp)
end
