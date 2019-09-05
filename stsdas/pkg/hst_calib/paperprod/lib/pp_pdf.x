# print out a the igi command to pass along a PDFMARK command 
# through to the PostScript to create a PDF bookmark using PDFMARK.
 
procedure pp_pdfbook (fd, page, title)
 
int     fd
char    title[ARB]
char    str[SZ_LINE]
int     page

begin
    
    # Create the PDFMARK command string for the bookmark
    call sprintf(str,SZ_LINE, "[/Page %d /View [/XYZ null null null] /Title (%s) /OUT pdfmark")
        call pargi(page)
        call pargstr(title)
    
    # Now, pass that string along to IGI for insertion into the PostScript
    call fprintf (fd, "pscmd %s\n")
        call pargstr (str)

end

procedure pp_pdfsection (fd, page, count, id)
 
int     fd
char    str[SZ_LINE]    
int     page            # page number this section title refers to
char    id[ARB]         # PropID or ObsID to be used as header title
int     count           # Number of pages associated with this header

begin
    

    # Create the PDFMARK command string for the bookmark
    call sprintf(str,SZ_LINE, "[/Count %d /Page %d /View [/XYZ null null null] /Title (%s) /OUT pdfmark")
        call pargi(count)
        call pargi(page)
        call pargstr(id)
    
    # Now, pass that string along to IGI for insertion into the PostScript
    call fprintf (fd, "pscmd %s\n")
        call pargstr (str)

end

# This procedure will create a PDFMARK command to cause the
#   viewer to automatically show all bookmarks when this document
#   is viewed.
# This should be called prior to any other PDFMARK commands.
procedure pp_pdfsetup (fd)
 
int     fd

begin

    # Create the PDFMARK command string for the bookmark
    # and pass that string along to IGI for insertion into the PostScript
    call fprintf (fd, "pscmd [/PageMode /UseOutlines /DOCVIEW pdfmark \n")

end
