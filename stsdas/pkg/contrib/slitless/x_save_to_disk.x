procedure save_to_disk(filename,title,spectrum, sky, width, npix, outfits)
# Output spectra information to a text file
# [pixel] [wabelength] [raw spectra] [sky] [sky substractedspectra]
char filename[SZ_LINE],title[SZ_LINE]
int width
real spectrum[npix,2]
real sky[npix,2]
int x, npix
bool outfits
pointer file, open()
char command[SZ_LINE]

begin
	call printf("Saving data to disk.\n")
	file = open(filename, NEW_FILE, TEXT_FILE)
	call fprintf(file,"#%s\n")
		call pargstr(title)
	do x=1, npix {
		call fprintf(file,"%f %f %d %f %f\n")
			call pargr(spectrum[x,2])
			call pargr(spectrum[x,1])
			call pargi(x)
			call pargr(sky[x,1])
			call pargr(spectrum[x,1]+sky[x,1])

	}
	call flush(file)
	call close(file)
	
	if (outfits) {
		call printf("Attempting to save data in fits format.\n")
		call printf("(This will only work if the noao onedspec package is loaded.)\n")
		call sprintf(command,SZ_LINE,"rspectext %s %s.fits dtype=nonlinear \n")
			call pargstr(filename)
			call pargstr(filename)

		call clcmdw(command)
	}
end
