# AVSHIFT   -  Process a table of shifts measured in the 4 WFPC2 chips.
#              Tranform them to the WF4 coordinate system, average and
#              tranform back. Input table is output from shiftfind task.
#
#  The reference chip is WF4
#
#  This version includes a rotation.
#
#  Based on a FORTRAN program by Richard Hook, ST-ECF,  Jan/Feb 1996
#
# 
#     Mar-96:  Command line parameters in FORTRAN code    (Andy Fruchter)
#  15-Apr-96:  Translated to CL script                    (I. Busko)
#  29-Jul-96:  Weighted rms                               (IB)
#  14-Aug-96:  Multiple image input                       (IB)
#  07-Nov-96:  Sort of input table                        (IB)
#  03-Jul-97:  Correction of weighted sum                 (Andy Fruchter)
#  07-Jul-97:  Revise calls that rely on the 'imtype' setting (IB)
#  06-Aug-98:  Modified to handle large rotations         (Richard Hook)


procedure avshift (table)

char	table    = ""         {prompt="Input table"}
real	angle    = INDEF         {prompt="Rotation angle"}
char	weight   = "0. 1. 1. 1."  {prompt="Weights for each chip"}
char    version  = "06Aug98"  {prompt="Date of installation"}
struct	*list              {prompt="Internal parameter, do not use."}

begin
	# Note that there are some rather arbitrary values here.
	char    name[4], filename, msg
	file	table1, table2
	struct  line
	int     i, j, jj, k, ift, ig, irot
	int     nr, nrwf, done[4]
	real    dx[4], dy[4], avx, avy, sw, r[4]
	real    rot, xd[4], yd[4], rad[4]
	real    t1, t2, t3, rpc, v, a1, a2, a3, a4, a5, a6, a7
	real    rms, rmswf, wrms, wrmswf, xx, yy
	real	wt1, wt2, wt3, wt4, wei[4], sumw, sumwwf

	# Check for the presence of pre-requisite tasks/packages.
	msg = ""
	if (!deftask("tsort")) msg = msg // " ttools"
	if (strlen(msg) > 0) {
	    printf ("Please, load packages: %s\n", msg)
	    bye
	}

	# Read file name and rotation.
	table1 = table
	rot    = angle
	print ("Assumed angle of rotation = ", rot, " degrees")
	rot = - rot * 3.14159 / 180.0 

	# Sort input table, just in case.
	table2 = mktemp ("tmp$avsh")
	copy (table1, table2, verbose-)
	tsort (table2, "c1", ascend+, casesens+)

	# Read weights string and parse.
	i = fscan (weight, wt1, wt2, wt3, wt4)
	wei[1] = wt1
	wei[2] = wt2
	wei[3] = wt3
	wei[4] = wt4

	printf ("Weights: %g %g %g %g\n", wei[1], wei[2], wei[3], wei[4]);

	# The reference is chip 4
	xd[4] = 0.0
	yd[4] = 0.0
        rad[4] = 0.0

	# These are offsets and angles for the other chips.
	t1 = atan2 (195.83,509.32)
	t2 = atan2 (730.79,698.21)
	t3 = atan2 (721.79,-5.28) 

        # Calculate the radial distances to the other chip centres
        rad[1] = 545.67
        rad[2] = 1010.72
        rad[3] = 721.81
         
	# Convert the rotation to vectors at each chip.
        # This no longer relies on a small angle approximation
	xd[1] = rad[1] * (cos(t1)-cos(t1)*cos(rot)-sin(t1)*sin(rot))
	yd[1] = rad[1] * (sin(t1)-sin(t1)*cos(rot)+cos(t1)*sin(rot))
	xd[2] = rad[2] * (cos(t2)-cos(t2)*cos(rot)-sin(t2)*sin(rot))
	yd[2] = rad[2] * (sin(t2)-sin(t2)*cos(rot)+cos(t2)*sin(rot))
	xd[3] = rad[3] * (cos(t3)-cos(t3)*cos(rot)-sin(t3)*sin(rot))
	yd[3] = rad[3] * (sin(t3)-sin(t3)*cos(rot)+cos(t3)*sin(rot))


	list = table2
	while (fscan (list, line) != EOF) {

	    # Clear group flags.
	    for (jj=1; jj<=4; jj+=1)
	        done[jj] = 0

	    for (jj=1; jj<=4; jj+=1) {

	        # Read columns from current line.
	        i = fscan (line, filename, ig, a1, a2, a3, a4)
	        if (nscan() != 6)            
	            error (0, "Failed to read table data.")

	        # Read next line.
	        if (jj < 4)
	            i = fscan (list, line)


	        # Read data from file columns. Clean filename extension.
	        for (i=strlen(filename); i > 0; i-=1) {
	            if (substr (filename, i, i) == ".")
	                break
	        }
	        name[ig] = substr (filename, 1, i-1)
	        dx[ig]   = a1
	        dy[ig]   = a3
	        done[ig] = 1
	    }

	    # Test for missing group.
	    for (jj=1; jj<=4; jj+=1) {
	        if (done[jj] != 1)
	            error (0, "Missing or duplicate group.")
	    }

	    # Convert the PC scale.
	    rpc = 0.09961 / 0.04557
	    dx[1] = dx[1] / rpc
	    dy[1] = dy[1] / rpc

	    # Here we define the rotation angles (taken from Trauger).
	    r[2] = -180.86 / (180.0 / 3.14159)
	    r[3] =  -90.55 / (180.0 / 3.14159)
	    r[4] = 0.0
	    r[1] = -270.34 / (180.0 / 3.14159)

	    # Calculate weighted averages.
	    avx = 0.0
	    avy = 0.0
	    sw  = 0.0
	    for (i=1; i<=4; i+=1) {
	        avx = avx - (xd[i] - dx[i] * cos(r[i]) + dy[i] * sin(r[i])) * wei[i]
	        avy = avy - (yd[i] - dx[i] * sin(r[i]) - dy[i] * cos(r[i])) * wei[i]
	        sw = sw + wei[i]
	    }
	    avx = avx / sw
	    avy = avy / sw

	    rms    = 0.0
	    rmswf  = 0.0
	    wrms   = 0.0
	    wrmswf = 0.0
	    nr     = 0
	    nrwf   = 0
	    sumw   = 0.0
	    sumwwf = 0.0

	    # Tranform back and write out the results.
	    print ("#")
	    print ("# Image   Group  xsh_in   ysh_in best_xsh best_ysh tot_sh_in delta_xsh delta_ysh")
	    for (i=1; i<=4; i+=1) {
	        if(i == 1)
	            v = rpc
	        else
	            v = 1.0

	        # Reverse the angles.
	        r[i] = -r[i]

	        if(strlen(name[i]) > 0) {
	            xx = v * (avx + xd[i])
	            yy = v * (avy + yd[i])
	            a1 = v * dx[i]
	            a2 = v * dy[i]
	            a3 = xx * cos (r[i]) - yy * sin(r[i])
	            a4 = xx * sin (r[i]) + yy * cos(r[i])
	            a5 = sqrt ((xx * cos (r[i]) - yy * sin (r[i]))**2 +
	                       (xx * sin (r[i]) + yy * cos (r[i]))**2)
	            a6 = v * dx[i] - xx * cos (r[i]) + yy * sin (r[i])
	            a7 = v * dy[i] - xx * sin (r[i]) - yy * cos (r[i])
	            if (strlen(name[i]) < 11)
	                printf("%-11s %1d  %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %9.3f\n", 
                                 name[i],i,a1,a2,a3,a4,a5,a6,a7)
	            else
	                printf("%11s %1d  %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %9.3f\n", 
                                 name[i],i,a1,a2,a3,a4,a5,a6,a7)

	            rms = rms + (a6/v)**2 + (a7/v)**2
	            wrms = wrms + ((a6/v)**2 + (a7/v)**2)*wei[i]


	            nr = nr + 1
	            sumw = sumw + wei[i]
	            if (i != 1) {
	                rmswf = rmswf + a6**2 + a7**2
	                wrmswf = wrmswf + (a6**2 + a7**2)*wei[i]
	                nrwf   = nrwf + 1
	                sumwwf = sumwwf + wei[i]
	            }
	        }
	    }

	    rms   = sqrt (rms   / real(nr))
	    rmswf = sqrt (rmswf / real(nrwf))
	    printf ("# RMS error (all chips on WF scale)): %8.4f\n", rms)
	    printf ("# RMS error (WF only):   %8.4f\n", rmswf)
	    if ((wei[2] != 0.0) || (wei[3] != 0.0) || (wei[4] != 0.0)) {
	        wrms   = sqrt (wrms   / sumw)
	        wrmswf = sqrt (wrmswf / sumwwf)
	        printf ("# weighted RMS error (all chips on WF scale)): %8.4f\n", wrms)
	        printf ("# weighted RMS error (WF only):   %8.4f\n", wrmswf)
	    }
	}

	delete (table2, go_ahead+, verify-, allversions+, subfiles+)
	list = ""
end




