# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
define	NTEMPLIN	16

#  templin -- Calculate the temperature by piece-wise linear interpolation
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  05-Jul-1991  J.-C. Hsu		coding
#------------------------------------------------------------------------------
procedure templin (indata, code, temperature)

						## input
int	indata
int	code
						## output
real	temperature
						## local
int	dn[NTEMPLIN,5]
real	temp[NTEMPLIN,5]
int	i

# temperature coefficients of WFPC and WFPC2 camera heads elec., pick off 
# mirror, hot junctions, and optical benches.
data 	(dn[i, 1], i = 1, NTEMPLIN) / 32, 64, 128, 224, 384, 640, 992, 1408, 
	2000, 2434, 2800, 3072, 3168, 3472, 3680, 3760/
data 	(temp[i,1], i = 1, NTEMPLIN) / -7.41500e+01, -6.56600e+01, 
	-5.65100e+01, -4.85000e+01, -4.00000e+01, -3.13200e+01, 
	-2.22000e+01, -1.36500e+01, -2.57000e+00, +5.85000e+00, 
	+1.39900e+01, +2.11600e+01, +2.38500e+01, +3.52400e+01, 
	+4.68700e+01, +5.36000e+01/

# Cold junctions (1-4) temperature coefficients of WFPC2
data 	(dn[i, 2], i = 1, NTEMPLIN) / 252, 669, 878, 1087, 1298, 1509, 1721,
	1933, 2147, 2361, 2576, 2792, 3008, 3226, 3445, 3665/
data 	(dn[i, 3], i = 1, NTEMPLIN) / 236, 653, 863, 1073, 1284, 1496, 1708,
	1921, 2135, 2350, 2565, 2781, 2999, 3217, 3436, 3657/
data 	(dn[i, 4], i = 1, NTEMPLIN) / 252, 669, 878, 1088, 1298, 1509, 1721,
	1934, 2147, 2361, 2576, 2792, 3008, 3226, 3445, 3665/
data 	(dn[i, 5], i = 1, NTEMPLIN) / 231, 648, 858, 1069, 1280, 1492, 1704,
	1918, 2132, 2346, 2562, 2779, 2996, 3215, 3434, 3655/

data 	(temp[i,2], i = 1, NTEMPLIN) / +5.00000e+01, +3.00000e+01, 
	+2.00000e+01, +1.00000e+01, +0.00000e+00, -1.00000e+01,
        -2.00000e+01, -3.00000e+01, -4.00000e+01, -5.00000e+01,
	-6.00000e+01, -7.00000e+01, -8.00000e+01, -9.00000e+01, 
	-1.00000e+02, -1.10000e+02/
data 	(temp[i,3], i = 1, NTEMPLIN) / +5.00000e+01, +3.00000e+01, 
	+2.00000e+01, +1.00000e+01, +0.00000e+00, -1.00000e+01,
        -2.00000e+01, -3.00000e+01, -4.00000e+01, -5.00000e+01,
	-6.00000e+01, -7.00000e+01, -8.00000e+01, -9.00000e+01, 
	-1.00000e+02, -1.10000e+02/
data 	(temp[i,4], i = 1, NTEMPLIN) / +5.00000e+01, +3.00000e+01, 
	+2.00000e+01, +1.00000e+01, +0.00000e+00, -1.00000e+01,
        -2.00000e+01, -3.00000e+01, -4.00000e+01, -5.00000e+01,
	-6.00000e+01, -7.00000e+01, -8.00000e+01, -9.00000e+01, 
	-1.00000e+02, -1.10000e+02/
data 	(temp[i,5], i = 1, NTEMPLIN) / +5.00000e+01, +3.00000e+01, 
	+2.00000e+01, +1.00000e+01, +0.00000e+00, -1.00000e+01,
        -2.00000e+01, -3.00000e+01, -4.00000e+01, -5.00000e+01,
	-6.00000e+01, -7.00000e+01, -8.00000e+01, -9.00000e+01, 
	-1.00000e+02, -1.10000e+02/
#==============================================================================
begin
	temperature = -300.

	do i = 1, NTEMPLIN-1 {
	    if (indata >= dn[i,code] && indata < dn[i+1,code]) {
		temperature = temp[i,code] + (temp[i+1,code]-temp[i,code]) * 
			real(indata-dn[i,code]) / real(dn[i+1,code]-dn[i,code])
		return
	    }
	}

	if (indata == dn[NTEMPLIN,code])
	    temperature = temp[NTEMPLIN,code]
end
