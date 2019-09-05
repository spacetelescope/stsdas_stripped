procedure yv2v3_calculate (dx, dy, detector)

real	dx	{prompt="Delta X"}
real	dy	{prompt="Delta Y"}
char	detector {min="|amber|blue|AMBER|BLUE",prompt="Detector"}
bool	costar	{yes,prompt="Costar orientation?"}
real	v2	{prompt="Output: offset in V2 (arcsec)"}
real	v3	{prompt="Output: offset in V3 (arcsec)"}

begin
	# Declarations.
	real	c, s
	char	pdet
	real	pdx, pdy
	
	# Get interactive input.
	pdx = dx
	pdy = dy
	pdet = detector
	
	# Calculate transformation constants.
	if (costar) {
	    c = -0.989802
	    s = -0.142453
	} else {
	    c = 0.989802
	    s = 0.142453
	}

	# Calculate transformation.
	if (pdet == "amber" || pdet == "AMBER") {
	    v2 = s*pdx - c*pdy
	    v3 = c*pdx + s*pdy
	} else {
	    v2 = c*pdx - s*pdy
	    v3 = s*pdx + c* pdy
	}
	if (abs (v2) < 1e-7)
	    v2 = 0.
	if (abs (v3) < 1e-7)
	    v3 = 0.
end
