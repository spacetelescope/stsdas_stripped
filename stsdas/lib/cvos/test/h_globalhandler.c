/* C standard header files */
# include <stdio.h>
# include <string.h>

/* CVOS header files */
# include <c_iraf.h>
# include <ximio.h>

# define SZ_FNAME 255

/* 
** Define a global error handler for this routine.
*/
static void detect_iraferr () {
    fprintf (stderr, "\nIRAF error %d: %s\n", c_iraferr(), c_iraferrmsg());
    fflush (stderr);
    exit (1);
}
/*
** Simple C program written which illustrates the
** necessary components for a host-level task.
*/
int main(int argc, char **argv) {
    IRAFPointer in;
    IRAFType pixtype;
    int dim1, dim2;
    char input[SZ_FNAME];

    /* Need to initialize the IRAF libraries *
     * for host-level tasks                  */
    c_irafinit (argc, argv);

    /* Push a function onto the error handler stack. */
    c_pusherr (detect_iraferr);

    if (argc < 2) {
        printf ("syntax:  h_globalhandler input\n");
        exit (1);
    }
    strcpy (input, argv[1]);

    /* Open the input image */
    in = c_immap (input, IRAF_READ_ONLY, 0);

    /* Get the size of each dimension */
    dim1 = c_imglen (in, 1);
    dim2 = c_imglen (in, 2);
    /* ...and the pixel type */
    pixtype = (IRAFType) c_imgtypepix (in);
    printf ("Input dimensions -- dim1 = %d  dim2 = %d of type %d\n",
            dim1, dim2, pixtype);

    /* Close the image and remove the function from the error handler stack */
    c_imunmap (in);
    c_poperr ();

    return (0);
}
