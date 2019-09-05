include <imhdr.h>
include <mach.h>

# Define number of "special" diodes there are in the X0H.
define  N_SPECIAL       12

# Define locations in the data of where information is stored.
define  CARPOS          19
define  COADD           22
define  DMP_IND         899
define  FINCODE         74
define  INTE            20

#---------------------------------------------------------------------------
.help obshrs Jun92 source
.ih
NAME
obs_zshp -- Print out information based on the GHRS SHP file.
obs_zudl -- Print out information based on the GHRS UDL file.
obs_zsci -- Print out information based on the GHRS SCI files.
.endhelp
#---------------------------------------------------------------------------
procedure obs_zshp (im, date_obs, time_obs, primary_group, total, o)

pointer im                      # I:  The image descriptor.
char    date_obs[ARB]           # I:  The date of observation.
char    time_obs[ARB]           # I:  The time of observation.
int     primary_group           # I:  Primary group.
int     total                   # I:  Total number of groups written.
pointer o                       # I:  The output pointer.

# Declarations.
int     dmp_ind                 # Begin/end indicator.
int     w1114                   # Word 11/14.

pointer buffer                  # Data buffer.
pointer filetype                # File type.
pointer observtn                # Observation number.
pointer obset_id                # Obs. id.
pointer progrmid                # Program id.
pointer sp                      # Stack pointer.
pointer xstr                    # Generic.

string  out     "%5d %11s %11s %3d %6d   %3s   0  PINFO= %3s %2s %2s DMP-IND=%1d\n"

# Function prototypes.
int     imgeti()
pointer imgl1s()

begin
        call smark(sp)
        call salloc (filetype, SZ_LINE, TY_CHAR)
        call salloc (observtn, SZ_LINE, TY_CHAR)
        call salloc (obset_id, SZ_LINE, TY_CHAR)
        call salloc (progrmid, SZ_LINE, TY_CHAR)
        call salloc (xstr, SZ_LINE, TY_CHAR)

        # Get header parameters.
        w1114 = imgeti (im, "WRD11_14")
        call imgstr (im, "FILETYPE", Memc[filetype], SZ_LINE)
        call imgstr (im, "PROGRMID", Memc[progrmid], SZ_LINE)
        call imgstr (im, "OBSET_ID", Memc[obset_id], SZ_LINE)
        call imgstr (im, "OBSERVTN", Memc[observtn], SZ_LINE)

        # Retrieve data from the image array.
        buffer = imgl1s (im)
        dmp_ind = Mems[buffer+DMP_IND]
        
        call sprintf (Memc[xstr], SZ_LINE, out)
        call pargi (total)
        call pargstr (date_obs)
        call pargstr (time_obs)
        call pargi (w1114)
        call pargi (primary_group - 1)
        call pargstr (Memc[filetype])
        call pargstr (Memc[progrmid])
        call pargstr (Memc[obset_id])
        call pargstr (Memc[observtn])
        call pargi (dmp_ind)
        call obs_print_out (o, Memc[xstr])

        # That's all folks.
        call sfree(sp)
end
#---------------------------------------------------------------------------
# End of obs_zshp
#---------------------------------------------------------------------------
procedure obs_zudl (im, date_obs, time_obs, primary_group, total, o)

pointer im                      # I:  The image descriptor.
char    date_obs[ARB]           # I:  The date of observation.
char    time_obs[ARB]           # I:  The time of observation.
int     primary_group           # I:  Primary group.
int     total                   # I:  Total number of groups written.
pointer o                       # I:  The output descriptor.

# Declarations.
int     fincode                 # Finish code.

pointer buffer                  # Data buffer.
pointer filetype                # File type.
pointer sp                      # Stack pointer.
pointer xstr                    # Generic.

string  out     "%5d %11s %11s     %6d   %3s   0     FINCODE=%4d\n"

# Function prototypes.
pointer imgl1s()

begin
        call smark(sp)
        call salloc (filetype, SZ_LINE, TY_CHAR)
        call salloc (xstr, SZ_LINE, TY_CHAR)

        # Retrieve header parameters.
        call imgstr (im, "FILETYPE", Memc[filetype], SZ_LINE)

        # Retrieve data parameters.
        buffer = imgl1s (im)
        fincode = Mems[buffer + FINCODE]

        call sprintf (Memc[xstr], SZ_LINE, out)
        call pargi (total)
        call pargstr (date_obs)
        call pargstr (time_obs)
        call pargi (primary_group - 1)
        call pargstr (Memc[filetype])
        call pargi (fincode)
        call obs_print_out (o, Memc[xstr])

        # That's all folks.
        call sfree(sp)
end
#---------------------------------------------------------------------------
# End of obs_zudl
#---------------------------------------------------------------------------
procedure obs_zsci (sci_im, x0h_im, date_obs, time_obs, primary_group, total, o)

pointer sci_im                  # I:  The image descriptor for the science.
pointer x0h_im                  # I:  Image descriptor for the extracted data.
char    date_obs[ARB]           # I:  The date of observation.
char    time_obs[ARB]           # I:  The time of observation.
int     primary_group           # I:  Primary group.
int     total                   # I:  Total number of groups written.
pointer o                       # I:  The output descriptor.

# Declarations.
real    sum                     # Sum of the data.

int     carpos                  # Carosel position.
int     coadd                   # Number of coadds.
int     detector                # Detector.
int     i                       # Generic.
int     inte                    # Integration count.
int     pfc                     # Packet format code.

pointer buffer                  # Data buffer.
pointer filetype                # File type.
pointer sp                      # Stack pointer.
pointer xstr                    # Generic.

string  out     "%5d %11s %11s     %6d   %3s %2d %2d %6d %4d %4d %8.0f\n"

# Function prototypes.
real    asumr(), asums()
int     imgeti()
pointer imgl1r(), imgl1s()

begin
        call smark(sp)
        call salloc (filetype, SZ_LINE, TY_CHAR)
        call salloc (xstr, SZ_LINE, TY_CHAR)

        # Retrieve header parameters.
        call imgstr (sci_im, "FILETYPE", Memc[filetype], SZ_LINE)
        pfc = imgeti (sci_im, "PKTFMT")
        detector = imgeti (sci_im, "DETECTOR")

        # Retrieve data parameters from the shp.
        buffer = imgl1s (x0h_im)
        carpos = Mems[buffer+CARPOS]
        if (carpos < 0)
            carpos = carpos + 2**NBITS_SHORT
        coadd = Mems[buffer+COADD]
        if (coadd < 0)
            coadd = coadd + 2**NBITS_SHORT
        inte = 0
        i = Mems[buffer+INTE]
        call bitmov (i, 1, inte, 1, 8)
        sum = asums (Mems[buffer], N_SPECIAL)

        # Retrieve the science data and sum it.
        buffer = imgl1r (sci_im)
        sum = sum + asumr (Meml[buffer], IM_LEN(sci_im,1))
        
        # Print it out.
        call sprintf (Memc[xstr], SZ_LINE, out)
        call pargi (total)
        call pargstr (date_obs)
        call pargstr (time_obs)
        call pargi (primary_group - 1)
        call pargstr (Memc[filetype])
        call pargi (pfc)
        call pargi (detector)
        call pargi (carpos)
        call pargi (inte)
        call pargi (coadd)
        call pargr (sum)
        call obs_print_out (o, Memc[xstr])

        # That's all folks.
        call sfree(sp)
end
#---------------------------------------------------------------------------
# End of obs_zudl
#---------------------------------------------------------------------------
