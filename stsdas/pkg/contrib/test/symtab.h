/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/*
**    SYMTAB.H    - Structures and declarations for things pertaining to the
**    symbol tables and token lists.
*/


struct nlist {
	char *sname;		/* symbol name */
int tokval;		/* token value */
int par_type;		/* type if variable */
char *def;		/* defined value */
int blevel;		/* bracket level (scope;  0=> global ) */
struct nlist *next;	/* next pointer in list */
};

struct nlist *insert(), *entry_alloc(), *lookup();


/*****  reserved keywords *****/
struct sysw {
char *word;
int value;
};


/*   TLIST is the structure containing information  on the series of tokens
**   seen.  This is later used to try to figure out what error occured.
**   TLIST is a circular, doubly-linked list of structures.
*/
struct tlist {
	struct tlist *back;	/* back pointer */
char *symbol;		/* symbol name  */
int token;		/* token value  */
struct tlist *next;	/* front pointer*/
};
struct tlist *tlist_alloc();

#define TLIST_SIZE	25
