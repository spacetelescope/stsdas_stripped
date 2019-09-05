include <imhdr.h>

# Routines that get the values of the keywords

long procedure imgctime (imptr)
pointer imptr
begin
	return IM_CTIME(imptr)
end

procedure imghistory (imptr, str, maxch)
pointer imptr
char 	str[ARB]
int	maxch
begin
	call strcpy(IM_HISTORY(imptr),str,maxch)
end

long procedure imglimtime (imptr)
pointer imptr
begin
	return IM_LIMTIME(imptr)
end

real procedure imgmax (imptr)
pointer imptr
begin
	return IM_MAX(imptr)
end

real procedure imgmin (imptr)
pointer imptr
begin
	return IM_MIN(imptr)
end

long procedure imgmtime (imptr)
pointer imptr
begin
	return IM_MTIME(imptr)
end

int procedure imgndim (imptr)
pointer imptr
begin
	return IM_NDIM(imptr)
end

long procedure imglen (imptr, axis)
pointer imptr
int axis
begin
	return IM_LEN(imptr,axis)
end

procedure imgpixfile (imptr, str, maxch)
pointer imptr
char 	str[ARB]
int	maxch
begin
	call strcpy(IM_PIXFILE(imptr),str,maxch)
end

int procedure imgtypepix (imptr)
pointer imptr
begin
	return IM_PIXTYPE(imptr)
end

procedure imgtitle (imptr, str, maxch)
pointer imptr
char 	str[ARB]
int	maxch
begin
	call strcpy(IM_TITLE(imptr),str,maxch)
end

# Routines that set the values of the keywords

procedure impctime (imptr, val)
pointer imptr
long 	val
begin
	IM_CTIME(imptr) = val
end

procedure imphistory (imptr, str)
pointer imptr
char 	str[ARB]
int	n
int	strlen()
begin
	n = strlen(str)
	if (n > SZ_IMHIST)
		n = SZ_IMHIST
	call strcpy(str,IM_HISTORY(imptr),n)
end

procedure implimtime (imptr, val)
pointer imptr
long 	val
begin
	IM_LIMTIME(imptr) = val
end

procedure impmax (imptr, val)
pointer imptr
real 	val
begin
	IM_MAX(imptr) = val
end

procedure impmin (imptr, val)
pointer imptr
real 	val
begin
	IM_MIN(imptr) = val
end

procedure impmtime (imptr, val)
pointer imptr
long 	val
begin
	IM_MTIME(imptr) = val
end

procedure impndim (imptr, val)
pointer imptr
int 	val
begin
	IM_NDIM(imptr) = val
end

procedure implen (imptr, axis, val)
pointer imptr
int axis
long 	val
begin
	IM_LEN(imptr,axis) = val
end

procedure imppixfile (imptr, str)
pointer imptr
char 	str[ARB]
int	n
int	strlen()
begin
	n = strlen(str)
	if (n > SZ_IMPIXFILE)
		n = SZ_IMPIXFILE
	call strcpy(str,IM_PIXFILE(imptr),n)
end

procedure imptypepix (imptr, val)
pointer imptr
int 	val
begin
	IM_PIXTYPE(imptr) = val
end

procedure imptitle (imptr, str)
pointer imptr
char 	str[ARB]
int	n
int	strlen()
begin
	n = strlen(str)
	if (n > SZ_IMTITLE)
		n = SZ_IMTITLE
	call strcpy(str,IM_TITLE(imptr),n)
end

# get the address of the user area
pointer procedure imguserarea (imptr)
pointer imptr
begin
	return IM_USERAREA(imptr)
end


