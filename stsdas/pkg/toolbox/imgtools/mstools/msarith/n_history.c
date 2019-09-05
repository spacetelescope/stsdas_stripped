#include <stdlib.h>	/* malloc */
# include <hstio.h>
# include <stdlib.h>
# include <string.h>
# include <time.h>
# include <stdio.h>
# include <math.h>
# include "msarith.h"


/* N_HISTORY: Appends HISTORY record to image header. The output file
   inherits the header (except FILENAME) from the 1st input file.


   Revision history:
   ---------------
   01 Mar 96  -  Implementation (IB)
   12 Nov 96  -  STIS support (IB)
   21 Apr 97  -  Report group lists (IB)
   25 Mar 04  -  Add ACS support (IB)

*/

int n_history (arithControl *ac, GenericGroup *in1, GenericGroup *in2,
               int ngroups) {

	/* Local variables */
	char               str[10], line[80];
	char               HistText[240];  /* HISTORY record  */
	char               timeStamp[61];  /* time stamp      */
	Bool               cr;             /* mode flag       */
	time_t             now;
	int                *list1 = NULL;  /* lists of groups */
	int                *list2 = NULL;
	int                angroup, group;

	/* Function declarations */
	int putKeyS (Hdr *, char *, char *, char *);
	int n_addHistory (GenericGroup *, char *);

	/* File name is output image's name */
	switch (in1->instrument) {
	case WFC3IR:
	case NICMOS: 
	    if (putKeyS (in1->sng->globalhdr, "FILENAME", ac->outfile, "."))
	        return (1); 
	    break;
	case STIS:
    case ACS:
    case COS:
	case WFC3UV:
	    if (putKeyS (in1->sg->globalhdr, "FILENAME", ac->outfile, "."))
	        return (1); 
	    break;
	}

	/* Assemble time stamp string. */
	now = time (NULL);
	strftime (timeStamp, 60, "%a %H:%M:%S %Z %d-%b-%Y", localtime(&now));

	/* Set count rate flag. */
	cr = False;
	if (in1->instrument == NICMOS || in1->instrument == WFC3IR) {
	    if (ac->isFile) {
	        if (*(ac->oper) == '+')
	            cr = True;
	    } else {
	        if ((*(ac->oper) == '*')  || 
                    (*(ac->oper) == '/'))
	            cr = True;
	    }
	}

	/* Add HISTORY records. */
	sprintf (HistText, "MSARITH v%s at %s", MSARITH_VERSION, timeStamp);
	if (n_addHistory (in1, HistText)) return (1);
	if (cr) {
	    if (ac->isCountRate) {
	        if (n_addHistory (in1, "MSARITH in count rate mode."))
	            return (1);
	    } else {
	        if (n_addHistory (in1, "MSARITH in raw counts mode."))
	            return (1);
	    }
	}

	sprintf (HistText, "MSARITH operand1: %s", ac->infile1);
	if (n_addHistory (in1, HistText)) return (1);

	str[0] = *(ac->oper);
	str[1] = '\0';
	sprintf (HistText, "MSARITH operator: %s", str);
	if (n_addHistory (in1, HistText)) return (1);

	if (ac->isFile)
	    sprintf (HistText, "MSARITH operand2: %s", ac->infile2);
	else
	    sprintf (HistText, "MSARITH operand2: %g(%g)",
	             ac->num_operand.value, ac->num_operand.error);
	if (n_addHistory (in1, HistText)) return (1);

	sprintf (HistText, "MSARITH result:   %s", ac->outfile);
	if (n_addHistory (in1, HistText)) return (1);

	/* Build local lists of groups. These may have repetitive entries. */
	angroup = (ac->ngroup1 > 0) ? ac->ngroup1 : ngroups;
	list1 = (int *) malloc (angroup * sizeof (int));
	if (ac->isFile)
	    list2 = (int *) malloc (angroup * sizeof (int));
	for (group = 0; group < angroup; group++) {
	    list1[group] = (ac->ngroup1 > 0) ? ac->list1[group] : group+1;
	    if (ac->isFile) {
	        if (ac->ngroup2 > 1)
	            list2[group] = ac->list2[group];
	        else if (ac->ngroup2 == 1)
	            list2[group] = ac->list2[0];
	        else
	            list2[group] = group+1;
	    }
	}

	/* Write HISTORY with group lists. */
	line[0] = '\0';
	for (group = 0; group < angroup; group++) {
	    sprintf (str, " %2d", list1[group]);
	    strcat (line, str);
	}
	sprintf (HistText, "MSARITH IMSET list1:%s", line);
	if (n_addHistory (in1, HistText)) return (1);
	if (ac->isFile) {
	    line[0] = '\0';
	    for (group = 0; group < angroup; group++) {
	        sprintf (str, " %2d", list2[group]);
	        strcat (line, str);
	    }
	    sprintf (HistText, "MSARITH IMSET list2:%s", line);
	    if (n_addHistory (in1, HistText)) return (1);
	}

	/* Free list memory. */
	free (list1);
	if (ac->isFile)
	    free (list2);

	return (0);
}



/* Add one HISTORY record. */

int n_addHistory (GenericGroup *g, char* text) {
 
	switch (g->instrument) {
	case WFC3IR:
	case NICMOS: 
	    addHistoryKw (g->sng->globalhdr, text);
	    if (hstio_err())
	        return (1);
	    break;
	case STIS:
    case ACS:
    case COS:
	case WFC3UV:
	    addHistoryKw (g->sg->globalhdr, text);
	    if (hstio_err())
	        return (1);
	    break;
	}
	return (0);
}
