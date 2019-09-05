# STS_HELP -- Issue a help line

procedure plhelp ()

int	linenr, maxline

data	linenr /1/
data	maxline/5/

begin
	switch (linenr) {
	case 1:
	    call printf (
		"h=plot(Ref_x,xi_res)  i=plot(Ref_x,eta_res)")
	    call printf ("  j=plot(Ref_y,xi_res")
	case 2:
	    call printf ("k=plot(Ref_y,eta_res)  l=plot(xi_res,eta_res)")
	    call printf ("  f=fit model")
	case 3:
	    call printf ("d=delete points   u=undelete points")
	    call printf ("  w=write coeff to image header")
	case 4:
	    call printf ("r=replot   v= view(h,i,j,k) together")

	case 5:
	    call printf (":= colon command ")
	    call printf ("  q=quit  ?=help  /=linehelp")

	}
	call flush (STDOUT)

	linenr = linenr + 1
	if (linenr > maxline)
	    linenr = 1
end
