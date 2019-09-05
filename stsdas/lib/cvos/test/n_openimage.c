/* C standard header files */
# include <stdio.h>

/* CVOS header files */
# include <c_iraf.h>
# include <xclio.h>
# include <ximio.h>

# define SZ_FNAME 255

/*
** Simple C program written which illustrates the necessary components 
** for a native IRAF task.
*/

/* 
** Wrapper which serves as the main entry/exit routine.  This wrapper is
** needed to handle error exits which might hang the IRAF CL.
*/
IRAFTASK (n_openimage) {

    /* Declare local variables */
    int i;

    /* Declare the function prototype */
    int openIt (void);

    i = openIt ();
}

/*
** Real work routine.
*/
int openIt (void) {
    
    IRAFPointer in;
    IRAFType pixtype;
    int ndim, dim1, dim2;
    char input[SZ_FNAME];

    c_clgstr ("input", input, SZ_FNAME);

    /* Open the input image */
    in = c_immap (input, IRAF_READ_ONLY, 0);

    /* Check the input image was opened without error */
    if (c_iraferr()) {
        printf ("IRAF error code: %d\nIRAF message: %s\n",
                c_iraferr(), c_iraferrmsg());
        return (1);
    }

    /* Get the image dimensions */ 
    ndim = c_imgndim (in);
    if (ndim != 2) {
        printf ("Sorry! This example only works for two dimensions.\n");
        return (1);
    }

    /* Get the size of each dimension */
    dim1 = c_imglen (in, 1);
    dim2 = c_imglen (in, 2);

    /* Get the pixel type */
    pixtype = (IRAFType) c_imgtypepix (in);
    printf ("Input:  %d dimensions -- dim1 = %d  dim2 = %d of type %d\n",
            ndim, dim1, dim2, pixtype);

    /* Close the image */
    c_imunmap (in);

    return (0);
}
