include "mkms.h"

#---------------------------------------------------------------------------
.help lists Mar92 source
.ih
NAME
.nf
mk_alloc_lists  -- Allocate list descriptor.
mk_open_lists   -- Populate the list descriptor.
mk_next_lists   -- Retrieve the filenames for the next list.
mk_close_lists  -- Close and free list descriptor.
.fi
.ih
USAGE
.nf
lists = mk_alloc_lists (n_lists)
call mk_open_lists (lists)
done =  mk_next_lists (lists)
call mk_close_lists (lists)
.fi
.ih
ARGUMENTS
.ls n_lists (integer :input)
The number of lists that will be managed.
.le
.ls lists (pointer :input)
The LIST structure pointer.
.le
.ih
RETURNS
.ls lists (pointer)
The LIST structure pointer.
.le
.ls done (boolean :output)
TRUE when there is another input file to process.
.le
.ih
DESCRIPTION
These are the utility routines to handle the LISTS structure defined
in mkms.h.  These routines handle the coordination of retrieving file
names from four seperate lists: input spectra, wavelength tables,
weight tables, and output spectra.  The input spectra list is the
driving list; when there are no more input files, we are done.  There
must be at least one wavelength table, and there may be zero or more
weight and output files specified.
.endhelp
#---------------------------------------------------------------------------
pointer procedure mk_alloc_lists (n_lists)

int     n_lists                 # I:  The number of lists to support.

pointer lists                   # The lists structure descriptor.

errchk  malloc

begin
        call malloc (lists, MK_SZ_LISTS, TY_STRUCT)
        call malloc (MK_TMPLT_PTR(lists), n_lists * TMPLT_SIZE, TY_CHAR)
        call malloc (MK_LD_PTR(lists), n_lists, TY_POINTER)
        call malloc (MK_FILE_PTR(lists), n_lists * FILE_SIZE, TY_CHAR)
        call malloc (MK_LEN_PTR(lists), n_lists, TY_INT)
        call malloc (MK_NEW_PTR(lists), n_lists, TY_INT)
        
        # That's all folks.
        return (lists)
end
#---------------------------------------------------------------------------
# End of mk_alloc_lists
#---------------------------------------------------------------------------
procedure mk_open_lists (lists)

pointer lists                   # I:  The lists descriptor.

int     imtlen()                # Get number of files in list.
pointer imtopen()               # Open the file list.

errchk  imtlen, imtopen

begin
        # The input file lists has no real restrictions on it
        # accept that there must be at least one file specified.
        MK_LD(lists,INPUT) = imtopen (MK_TEMPLATE(lists,INPUT))
        MK_LEN(lists,INPUT) = imtlen (MK_LD(lists,INPUT))
        if (MK_LEN(lists,INPUT) == 0)
            call error (1, "No input images specified")
        MK_FILE(lists,INPUT) = EOS

        # For the wavelengths, there must be at least one specified or
        # as many wavelengths as inputs.
        MK_LD(lists,WAVE) = imtopen (MK_TEMPLATE(lists,WAVE))
        MK_LEN(lists,WAVE) = imtlen (MK_LD(lists,WAVE))
        if (MK_LEN(lists,WAVE) == 0)
            call error (1, "No wavelength tables specified")
        if (MK_LEN(lists,WAVE) != MK_LEN(lists,INPUT) && MK_LEN(lists,WAVE) > 1)
                call error (1, "Need only one or same number of wavelength images as input images")
        MK_FILE(lists,WAVE) = EOS

        # For the output and weights, there can be none, one, or as
        # many as the input files.
        MK_LD(lists,OUTPUT) = imtopen (MK_TEMPLATE(lists,OUTPUT))
        MK_LEN(lists,OUTPUT) = imtlen (MK_LD(lists,OUTPUT))
        if (MK_LEN(lists,OUTPUT) != MK_LEN(lists,INPUT) &&
            MK_LEN(lists,OUTPUT) > 1)
                call error (1, "Need only one or same number of output images as input images")
        MK_FILE(lists,OUTPUT) = EOS

        MK_LD(lists,WEIGHT) = imtopen (MK_TEMPLATE(lists,WEIGHT))
        MK_LEN(lists,WEIGHT) = imtlen (MK_LD(lists,WEIGHT))
        if (MK_LEN(lists,WEIGHT) != MK_LEN(lists,INPUT) &&
            MK_LEN(lists,WEIGHT) > 1)
                call error (1, "Need none, one, or same number of weight images as input images")
        MK_FILE(lists,WEIGHT) = EOS

end
#---------------------------------------------------------------------------
# End of mk_open_lists
#---------------------------------------------------------------------------
bool procedure mk_next_lists (lists)

pointer lists                   # I:  The lists descriptor.

int     i                       # Generic.
int     imtgetim()              # Get next image from list.
bool    not_done                # TRUE if there are more files.
pointer sp                      # Stack pointer.
pointer xstr                    # Generic.

errchk  imtgetim

begin
        # Memory.
        call smark (sp)
        call salloc (xstr, FILE_SIZE, TY_CHAR)
        
        # The input is the driving force here.  If there are no
        # more input files, then return done.
        not_done = (imtgetim (MK_LD(lists,INPUT), MK_FILE(lists,INPUT),
                              FILE_SIZE) != EOF)

        # For the rest of the files, retrieve the next file or preserver
        # the original name.
        if (not_done)
            do i = 1, N_LISTS {
                if (i == INPUT)
                    next

                if (imtgetim (MK_LD(lists,i), Memc[xstr], FILE_SIZE) != EOF) {
                        call strcpy (Memc[xstr], MK_FILE(lists,i),
                                     FILE_SIZE)
                    MK_NEW(lists,i) = YES
                } else
                    MK_NEW(lists,i) = NO
            }   
        
        # That's all folks.
        call sfree (sp)
        return (not_done)
end
#---------------------------------------------------------------------------
# End of mk_next_lists
#---------------------------------------------------------------------------
procedure mk_close_lists (lists)

pointer lists                   # IO: List descriptor.  Null on return.

errchk  mfree

begin
        call mfree (MK_NEW_PTR(lists), TY_INT)
        call mfree (MK_LEN_PTR(lists), TY_INT)
        call mfree (MK_FILE_PTR(lists), TY_CHAR)
        call mfree (MK_LD_PTR(lists), TY_POINTER)
        call mfree (MK_TMPLT_PTR(lists), TY_CHAR)
        call mfree (lists, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of mk_close_lists
#---------------------------------------------------------------------------
