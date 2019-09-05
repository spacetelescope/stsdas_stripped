# TABACCESS -- Test to see if an table is accessible with the given access
# mode. Return the result of the test as YES or NO.
#
# B. Simon	12-Aug-87	First Code

int procedure tabaccess (tablename, acmode)

char	tablename[ARB]	# i: table file name
int	acmode		# i: access mode
#--
int	status
pointer	sp, newname

int	access()

begin
	if (acmode == NEW_FILE || acmode == NEW_COPY)
	    return (YES)

	call smark (sp)
	call salloc (newname, SZ_FNAME, TY_CHAR)

	call tbtext (tablename, Memc[newname], SZ_FNAME)
	status = access (Memc[newname], acmode, BINARY_FILE)

	call sfree (sp)
	return (status)
end
