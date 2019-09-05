/* C standard header files */
# include <stdio.h>
# include <string.h>

/* CVOS header files */
# include <c_iraf.h>
# include <ximio.h>

# define SZ_FNAME 255

/*
** Simple C program written which illustrates the necessary components
** for a host-level task.
*/

int main(int argc, char **argv) {
    IRAFPointer in;
    IRAFType pixtype;
    int ndim, dim1, dim2;
    char input[SZ_FNAME];

    /* Need to initialize the IRAF libraries *
     * for host-level tasks                  */
    c_irafinit (argc, argv);

    if (argc < 2) {
        printf ("syntax:  h_openimage input\n");
        exit (1);
    }
    strcpy (input, argv[1]);

    /* Open the input image */
    in = c_immap (input, IRAF_READ_ONLY, 0);

    /* Check the input image was opened without error */
    if (c_iraferr()) {
        printf ("IRAF error code: %d\nIRAF message: %s\n",
                c_iraferr(), c_iraferrmsg());
        exit (1);
    }

    /* Get the image dimensions */ 
    ndim = c_imgndim (in);
    if (ndim != 2) {
        printf ("Sorry! This example only works for two dimensions.\n");
        exit (1);
    }

    /* Get the size of each dimension */
    dim1 = c_imglen (in, 1);
    dim2 = c_imglen (in, 2);
    /* ...and the pixel type */
    pixtype = (IRAFType) c_imgtypepix (in);
    printf ("Input:  %d dimensions -- dim1 = %d  dim2 = %d of type %d\n",
            ndim, dim1, dim2, pixtype);

    /* Close the image and end */
    c_imunmap (in);
    return (0);
}
