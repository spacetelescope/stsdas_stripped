include <imhdr.h>
include <iraf77.h>

# UIGL3? -- get a data line from an apparently 3-d file

procedure uigl3r (im, linenum, bandnum, buffer, istat)

pointer im           # pointer to the image header file
int linenum          # line number to get from image
int bandnum          # band number to get from image
real buffer[ARB]    # user's buffer to be filled by this routine
int istat            # return status code

pointer ip, imgl3r()

begin
	istat = ER_OK
	# call imio to read the data into an imio buffer area
	iferr (ip = imgl3r(im, linenum, bandnum)) {
		istat = ER_IMREAD
		return
		}
	# load the user's array from imio buffer
	call amovr (Memr[ip], buffer, IM_LEN(im,1))

end
