# include <string.h>
# include <stdlib.h>	/* atoi, malloc */
# include <stdio.h>
# include <math.h>
# include "msarith.h"


/*   Get a group list specification in string format and create the
     appropriate int array with group ids.



      Revision history:
      ---------------
      21 Apr 97  -  Implementation (IB)

*/

int n_getGroupList (arithControl *ac, char *list, int ord) {

	char	token[80];
	int	commas, i, j, k, *ilist;

	/* Count how many possible tokens. */
	commas = 0;
	for (i = 0; i < strlen (list); i++) {
	    if (list[i] == ',')
	        commas++;
	}

	/* Alloc space for group list. */
	if (ord == 1) {
	    ac->list1 = (int *) malloc ((commas+1) * sizeof (int));
	    ilist = ac->list1;
	} else {
	    ac->list2 = (int *) malloc ((commas+1) * sizeof (int));
	    ilist = ac->list2;
	}

	/* Parse. */
	j = 0;
	i = 0;
	while (i < strlen (list)) {
	    k = 0;
	    while (list[i] != ',' && list[i] != '\0') 
	        token[k++] = list[i++];
	    token[k] = '\0';
	    i++;
	    if ((ilist[j] = atoi (token)) != 0)
	        j++;
	}

	if (ord == 1)
	    ac->ngroup1 = j;
	else
	    ac->ngroup2 = j;

	return (j);
}
