procedure foc()
string	mode="al"

begin
	set focprism	= "foc$focprism/"

	package foc

	task	focprism.pkg	= focprism$focprism.cl
	task	newgeom = "foc$x_newgeom.e"
	task	calfoc = "foc$x_calfoc.e"

	cl()
end

