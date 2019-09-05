/* C standard header files */
# include <stdlib.h>
# include <stdio.h>

/* CVOS header files */
# include <c_iraf.h>
# include <xtables.h>

/* Define the number of columns */
# define NCOLS 7

/*
 * Define the global error handler for this routine
 */
void iraferr () {
    printf ("IRAF error code %d\nIRAF message: %s\n",
            c_iraferr (), c_iraferrmsg ());
    exit (1);
}

/*
** Simple C program which uses tools from the tables package
** in a host-level task.
*/

int main (int argc, char **argv) {

    const int REGION = 0;      /* region plate is located in */
    const int PLATE  = 1;      /* plate id                   */
    const int RA     = 2;      /* right ascension            */
    const int DEC    = 3;      /* declination                */
    const int EPOCH  = 4;      /* epoch of observation       */
    const int SURVEY = 5;      /* type of survey             */
    const int DISK   = 6;      /* CD-ROM disk plate is on    */
    int	   row       = 0;
    int    disk;
    char   *region;
    char   *plate;
    double ra;
    double dec;
    double epoch;
    char   *survey;
    IRAFPointer tab;           /* pointer to table struct    */
    IRAFPointer col[NCOLS];    /* pointers to column info    */

    /* Need to initialize the IRAF libraries *
     * for host-level tasks                  */
    c_irafinit (argc, argv);

    /* Install a global error handler */
    c_pusherr (iraferr);

    /* Open the output table */
    tab = c_tbtopn ("wrtableTest.tab", IRAF_NEW_FILE, 0);

    /* Define columns.  The "Name" column is a string up to 20 char long. */
    c_tbcdef1 (tab, &col[REGION], "REGION", "", "", -6, 1);
    c_tbcdef1 (tab, &col[PLATE], "PLATE", "", "", -4, 1);
    c_tbcdef1 (tab, &col[RA], "RA", "hours", "%14.3h", IRAF_DOUBLE, 1);
    c_tbcdef1 (tab, &col[DEC], "DEC", "degrees", "%14.3h", IRAF_DOUBLE, 1);
    c_tbcdef1 (tab, &col[EPOCH], "EPOCH", "years", "%10.3f", IRAF_DOUBLE, 1);
    c_tbcdef1 (tab, &col[SURVEY], "SURVEY", "", "", -3, 1);
    c_tbcdef1 (tab, &col[DISK], "DISK", "", "%3d", IRAF_INT, 1);

    /* Create the output table file */
    c_tbtcre (tab);

    /* Add a history record */
    c_tbhadt (tab, "history", "Simple test program to create a table.");

    /* Add a few rows in a simple manner */
    row++;
    region = "S256";
    plate  = "000Y";
    ra     = 105.37155;
    dec    = -45.07291;
    epoch  = 1980.122;
    survey = "UK";
    disk   = 18;

    c_tbeptt (tab, col[REGION], row, region);
    c_tbeptt (tab, col[PLATE], row, plate);
    c_tbeptd (tab, col[RA], row, ra);
    c_tbeptd (tab, col[DEC], row, dec);
    c_tbeptd (tab, col[EPOCH], row, epoch);
    c_tbeptt (tab, col[SURVEY], row, survey);
    c_tbepti (tab, col[DISK], row, disk);

    row++;
    region = "S734";
    plate  = "006Q";
    ra     = 275.68950;
    dec    = -9.97406;
    epoch  = 1978.647;
    survey = "UK";
    disk   = 50;

    c_tbeptt (tab, col[REGION], row, region);
    c_tbeptt (tab, col[PLATE], row, plate);
    c_tbeptd (tab, col[RA], row, ra);
    c_tbeptd (tab, col[DEC], row, dec);
    c_tbeptd (tab, col[EPOCH], row, epoch);
    c_tbeptt (tab, col[SURVEY], row, survey);
    c_tbepti (tab, col[DISK], row, disk);

    /* Close the table and clean up */	
    c_tbtclo (tab);

    printf ("Created and closed the tables\n");

    /* Remove the function from the error handler stack */
    c_poperr ();

    return (0);
}
