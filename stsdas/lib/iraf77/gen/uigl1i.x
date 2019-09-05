include <imhdr.h>
include <iraf77.h>

# UIGL1? -- get a data line from an apparently 1-d file

procedure uigl1i (im, buffer, istat)

pointer im           # pointer to the image header file
int buffer[ARB]    # user's buffer to be filled by this routine
int istat            # return status code

pointer ip, imgl1i()

begin
	istat = ER_OK

	# call imio to read the data into an imio buffer area
	iferr (ip = imgl1i(im))
		{istat = ER_IMREAD
		return
		}

	# load the user's array from imio buffer
	call amovi (Memi[ip], buffer, IM_LEN(im,1))

end
