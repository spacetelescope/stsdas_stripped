/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_ctype
#include <iraf.h>

#include "def.h"
#include "symtab.h"


# define IF 278
# define ELSE 279
# define ELSEIF 280
# define BREAK 281
# define CONTINUE 282
# define FOR 283
# define DO 284
# define WHILE 285
# define UNTIL 286
# define RETURN 287
# define INDEX 300
# define PARAMETER 288
# define STARPARAM 289
# define ITEMPARAM 290
# define PLATEPARAM 291
# define SETPARAM 292
# define OBSERVATION 293
# define DATUM 294
# define VARIABL 295
# define VECTOR 296
# define INCLUDE 297
# define LOOP 298
# define CONSTANT 299
# define VARIABLE 264



struct nlist *symtab ;          /* pointer to beginning of */
/* symbol table            */
struct nlist *symtab_last ;     /* pointer to last entry of*/
/* symbol table            */

/*****  reserved keywords *****/
struct sysw  syswords[] = {
	/*{"main\0",      MAIN } ,*/      { 
		"if\0",	        IF 	} 
	,
	{ 
		"else\0",	      ELSE 	} 
	,       { 
		"elseif\0",      ELSEIF 	} 
	,
	{ 
		"break\0",	      BREAK 	} 
	,      { 
		"continue\0",    CONTINUE 	} 
	,
	{ 
		"return\0",      RETURN 	} 
	,     { 
		"include\0",     INCLUDE 	} 
	,
	{ 
		"while\0",	      WHILE 	} 
	,      { 
		"for\0",	        FOR 	} 
	,  
	{ 
		"until\0",	      UNTIL 	} 
	,      { 
		"do\0",	        DO 	} 
	,
	{ 
		"parameter\0",   PARAMETER 	} 
	,  { 
		"starparam\0",   STARPARAM 	} 
	,
	{ 
		"itemparam\0",   ITEMPARAM 	} 
	,  { 
		"plateparam\0",  PLATEPARAM 	} 
	,
	{ 
		"setparam\0",    SETPARAM 	} 
	,   { 
		"observation\0", OBSERVATION 	} 
	,
	{ 
		"data\0",        DATUM 	} 
	,      { 
		"variable\0",    VARIABL  	} 
	,
	{ 
		"loop\0",        LOOP 	} 
	,	     { 
		"constant\0",	CONSTANT 	}
};
#define NUM_SYSWORDS   sizeof(syswords)/sizeof(struct sysw)

int var_decls[] = { 
	PARAMETER, OBSERVATION, DATUM, VARIABL, CONSTANT };
#define NUM_DECL_TYPES sizeof(var_decls)/sizeof(int)

extern int bracket_level;
extern int saw_param;
extern int saw_eql;
extern int par_type;


struct tlist *tlist, *start;





/*
** INSERT - put a new name into the symbol table.
**    input   -   symbol name and token value
**    output  -   nothing.  Blindly inserts anything
*/

struct nlist *insert(s, val)
char *s;		/* symbol name */
int val;		/* token value */
{
	unsigned size;
	struct nlist *np, *lookup(), *entry_alloc();
	char *malloc();

#ifdef MLDEBUG
	fprintf(stderr,"insert():  s = :%s:\n",s);
#endif

	if ((np=lookup(s)) == NULL) {      /* insert in symtab */
insert_:	
		np = entry_alloc();
		if (np == NULL) {
			fprintf(stderr,
			"Memory allocation fault installing %s in symtab\n",s);
			return(NULL);
		}
		np->next = symtab;
		size = strlen(s) + 1;
		np->sname = malloc(size); 
		strcpy(np->sname,s);
		if (val == NM_PARM) {
			np->tokval = VARIABLE;
			np->par_type = VARIABL;
			np->blevel = 1;
		}
		else if (val == NM_FUNC) {
			np->tokval = val;
			np->par_type = par_type;
			np->blevel = 0;
		}
		else if (val == INDEX) {
			np->tokval = VARIABLE;
			np->par_type = INDEX;
			np->blevel = 0;
		}
		else {
			np->tokval = val;
			np->par_type = par_type;
			np->blevel = bracket_level;
		}

		/*printf("insert %8s %4d : %4d %4d %4d (%4d)\n", s, val,
			np->tokval, np->par_type, np->blevel, bracket_level);*/
		symtab = np;
		return(np);
	} 
	else {
		/* already in symbol table */
		if(np->blevel != bracket_level)
			goto insert_;
		else
			return(np);
	}

}





/*  
**  LOOKUP - check the symbol table for a given entry 's'
**     input    - symbol name in 's'
**     output   - token value
*/

struct nlist *lookup(s)
char *s;		/* sumbol name */
{
	struct nlist *np;

	for(np=symtab; np != NULL; np = np->next)

	{ 
		if(strcmp(s,np->sname) == 0)
			return(np);
			}
	return(NULL);
}






/* 
**   IS_RESERVED - check sysword[] to see if input is a reserved word.
**	returns value field of struct.   -1 is returned if not a sysword.
*/

is_reserved(s)
char *s;
{
	register int i;

	for(i=0;i<NUM_SYSWORDS;i++) {
		if(strcmp(syswords[i].word,s) == 0)
			return (syswords[i].value) ;
	}
	return( -1 );
}




/*
** ENTRY_ALLOC - Allocate space for the structure for a new entry in the
** symbol table
*/

struct nlist *entry_alloc()
{
	char *malloc();
	return( (struct nlist *) malloc(sizeof(struct nlist)));
}





/*
**  TLIST_ALLOC - allocate a structure for the token list.
*/

struct tlist *tlist_alloc()
{
	char *malloc();

	return((struct tlist *)malloc(sizeof(struct tlist)));
}






/*
**  INIT_TABLE - Initialize the symbol table by placing all reserved words
**  in it and setting pointers to 1st and last
*/

init_table()
{
	register int i;
	char *malloc();
	struct nlist *np, *entry_alloc(), *lookup(), *insert();


	if((np=entry_alloc()) == NULL) {
		fprintf(stderr,"Memory fault initializing symbol table\n");
		exit(-1);
	}
	symtab = symtab_last = np;		/* insert IF    */
np->sname = malloc(strlen("if")+1);
strcpy(np->sname,"if");
np->tokval = IF;
np->blevel = 0;
np->next = NULL;

for(i=1;i<NUM_SYSWORDS;i++)		/* do the rest of em */
	insert(syswords[i].word,syswords[i].value);


}





/*
**    DELTAB - delete entries from the symbol table based on the current
**    bracket_level.  Called after each '}' encountered to remove local
**    variables from the list.
*/

deltab(blevel)
int blevel;
{
	struct nlist *np;

#ifdef SYMTAB_DEBUG
	dump_symtab(blevel);		/* debug print of symtab contents */
#endif

if(symtab->blevel < blevel) return;
for(np=symtab; np->blevel >= blevel ; np = symtab) {
symtab = np->next;
free((char *)np);
}
}



/*  INIT_TOKEN_LIST  -  For error recovery purposes I want to keep a list
**  of tokens seen so that I can hopefully figure out what happened.  TLIST
**  is a doubly linked circular list of tokens.  I made it circular so I
**  can recycle the tokens.
*/

struct tlist *init_token_list()
{
	register int i;
	struct tlist *tlist_alloc(), *tp;

	start = tlist_alloc();		/* get first struct              */
start->next = tlist_alloc();	/* first link	                 */
start->next->back = start;	/* doubly link first two structs */

tp = start->next;		/* move to next struct		 */

for(i=0; i<TLIST_SIZE; i++) {
tp->next = tlist_alloc();	/* make another one	 */
tp->next->back = tp;		/* doubly link it	 */
tp = tp->next;			/* move on to next one   */
}

tp->next = start;		/* link back to start 		 */
start->back = tp;		/* now circular			 */

return((struct tlist *)start);	/* send it back			 */
}



/*
**   DUMP_SYMTAB  -  Print out symtab contents for debugging.
*/

dump_symtab(BLEV)
int BLEV;
{
	struct nlist *np;

	fprintf(stderr,"\tSYMBOL TABLE BLEV = %d\n",BLEV);
	for(np=symtab;np!=NULL;np=np->next) {
	    if(is_reserved(np->sname)== -1) {
		fprintf(stderr,"blevel=%d\tpar_type=%d\ttoken=%d\tname=:%s:\n",
			np->blevel,np->par_type,np->tokval,np->sname);
	     }
	}
}




/* 
**  IS_DECLARATION_SYSWORD -  Check to see if token value is a type of 
**  declaration.
*/

is_declaration_sysword(val)
int val;
{
	register int i;

	for(i=0; i<NUM_DECL_TYPES; i++) {
		if(val == var_decls[i]) return(1);
	}
	return(0);
}



/*
**  IS_VALID_LHS - Check to see if data type is legal for LHS of equation.
*/

is_valid_lhs(val) int val; { return( (val == VARIABL? 1 : 0) ); }
