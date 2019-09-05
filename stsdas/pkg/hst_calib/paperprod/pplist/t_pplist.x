# Create the paper product target list and observation list.

procedure t_pplist()

pointer	tproot
pointer	tpimtype
char	instr[SZ_LINE]
char	output[SZ_FNAME]
char	selector[SZ_LINE]
char	timetag[SZ_LINE]
int	page

pointer	imtopenp()
bool	streq()
int	clgeti()

begin
	# read parameters
	tproot = imtopenp ("rootlist")
	tpimtype = imtopenp ("imtypelist")
	call clgstr ("output", output, SZ_LINE)
	call clgstr ("instr", instr, SZ_LINE)
	call clgstr ("selector", selector, SZ_LINE)

	call strlwr (instr)
	if (streq(instr, "fos")) {
	    call ypplist (tproot, tpimtype, output)
	} else if (streq(instr, "ghrs") || streq(instr, "hrs")) {
	    call zpplist (tproot, tpimtype, output)
	} else if (streq(instr, "wfpc2") || streq(instr, "wfpcii")) {
	    call clgstr ("timetag", timetag, SZ_LINE)
	    page = clgeti ("page")
	    call upplist (tproot, tpimtype, output, selector, timetag, page)
        call clputi("page", page)
	} else if (streq(instr, "foc")) {
	    call clgstr ("timetag", timetag, SZ_LINE)
	    page = clgeti ("page")
	    call xpplist (tproot, tpimtype, output, selector, timetag, page)
        call clputi("page", page)
	} else if (streq(instr, "stis")) {
	    call clgstr ("timetag", timetag, SZ_LINE)
	    page = clgeti ("page")
	    call opplist (tproot, output, selector, timetag, page)
	    call clputi ("page", page)
	} else if (streq(instr, "acs")) {
	    call clgstr ("timetag", timetag, SZ_LINE)
	    page = clgeti ("page")
	    call jpplist (tproot, output, selector, timetag, page)
	    call clputi ("page", page)
	} else if (streq(instr, "nicmos")) {
	    call clgstr ("timetag", timetag, SZ_LINE)
	    page = clgeti ("page")
	    call npplist (tproot, output, selector, timetag, page)
	    call clputi ("page", page)
	}
end
