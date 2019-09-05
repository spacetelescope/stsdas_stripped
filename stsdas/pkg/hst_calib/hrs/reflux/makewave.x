# MAKE_WAVE -- Construct a HRS/FOS wavelength image name from an   
# arbitrary image name
#
# Create an image with a "c0h" extension, given an some image name, typically 
# ending in "c1h" but stripping off the group and image section information.
#
# S. Hulbert	Jul91	Original

procedure make_wave(flux_file, wave_file, maxchar)

char    flux_file[ARB]		#I: input flux image name
char    wave_file[ARB]		#O: output wavelength image name
int	maxchar			#I: max characters in image name

char    root[SZ_FNAME], section[SZ_FNAME]
char    ldir[SZ_FNAME], root_only[SZ_FNAME], EXTN[SZ_FNAME]
int     def_gcount, gindex, gcount

begin

        # begin parsing: get rootname, section, group specification
        call tp_parse (flux_file, def_gcount, root, section, gindex, gcount)

        # continue parsing rootname to directory path, root, and extension
        call fnldir (root, ldir, SZ_FNAME)
        call fnroot (root, root_only, SZ_FNAME)
        call fnextn (root, extn, SZ_FNAME)

        # now rebuild replacing with a value extension set to 'c0h'
        call strcpy (ldir, wave_file, maxchar)
        call strcat (root_only, wave_file, maxchar)
        call strcat (".c0h", wave_file, maxchar)

end


