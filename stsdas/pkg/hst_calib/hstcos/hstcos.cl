procedure hstcos()
string	mode="al"

begin

    package hstcos

    pyexecute ("hstcos$calcos_iraf.py", tasknames="calcos")
    pyexecute ("hstcos$x1d_iraf.py", tasknames="x1dcorr")
    pyexecute ("hstcos$splittag_iraf.py", tasknames="splittag")

    cl()
end
