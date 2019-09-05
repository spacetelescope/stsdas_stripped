
/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/*
**	COMPILE  - routine for ML.  At first writing only basic routines to
**	     satisfy YACC but easily modified to do more.
**
**
**	Programming begun 09/11/1986  by Mike Fitzpatrick
**
**	MODIFICATION HISTORY:
**
*/

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_ctype
#include <iraf.h>

#include "symtab.h"
#include "compile.h"
#include "def.h"


#define VARIABLE      264		/* TOKEN values we need */
#define FUNC_NAME     259
#define OP	      512
#define Y_LP          267
#define Y_RP          268
#define Y_LB          269
#define Y_RB          270
#define Y_LC          271
#define Y_RC          272
#define Y_COM	      273
#define Y_SMC         274
#define Y_CLN         275

extern int cur_fd,		/* current file descriptor 	*/
line_number,		/* current line number		*/
label_stack[], 	/* stack of labels 		*/
lstack_index, 	/* index to label stack		*/
yytstk_index,	/* index of yytext stack        */
label_counter;	/* counter to insure unique labs*/
extern FILE *fd_stack[];	/* fd stack for include files   */
extern struct tlist *tlist;	/* token list for error recovery*/
FILE *fd_out;			/* output filke descriptor 	*/
int NLOFILE;			/* NULL output file		*/

char *itos();			/* convert an integer to string	*/
char *tlist_symbol();		/* return token from list	*/
struct ifile fl_list[10];	/* error recovery list		*/



/*
**  COMPILE() - This is the entry point for the compiler.  Only 
**  input requireed is an input and output file name.  All other
** routines are called from here.
**
**  RETURNS: A struct containg the error message and line number
** 	     if an error was found, otherwise NULL
*/

struct ifile *compile(ifname,ofname)
char *ifname, *ofname;
{
	char arg[30];
		    
	if(!init(ifname)) 		/* Initialize   */
return( (struct ifile *)NULL);

if (ofname == NULL) {		/* NULL output  */
	fprintf(stderr,"compile: ofname is a NULL pointer\n");
	fflush(stderr);
	return( (struct ifile *)NULL);
}

if (ofname != NULL && strcmp(ofname,"")!=0) {
	if((fd_out=fopen(ofname,"w+")) == NULL) {
		fprintf(stderr,"Unable to open output file  %s\n",ofname);
		fflush(stderr);
		return( (struct ifile *)NULL);
	}
	NLOFILE = 0;	/* OK for output */
} 
else 
NLOFILE = 1;	/* not OK for output */


/* Let's get started on output */
print_instr("DEFUN","_GLOBS");
sprintf(arg,"%s:%d",fl_list[cur_fd].fname,1);
print_instr("LINE",arg);

if(yyparse()) {		/* call parser and begin compilation */
	print_instr("ENDGLOB","");
	if (ofname != NULL && strcmp(ofname,"")!=0) fclose(fd_out);
	/*cleanup();*/
	return(&fl_list[cur_fd]);
} 
else {
	print_instr("ENDGLOB","");
	if (ofname != NULL && strcmp(ofname,"")!=0) fclose(fd_out);
	/*cleanup();*/
	return((struct ifile *)NULL);
}

}



/*
**  INIT() - Initialize all file names and variables for compilation
**
**  RETURNS: 0		if unable to initlialize
**	     1		initialization went OK
*/

init(ifname) 
char *ifname;
{
	struct tlist *init_token_list();
	char *malloc();

			/* Initialize File pointer to input */
cur_fd = 0;
if((fd_stack[cur_fd]=fopen(ifname,"r")) == NULL) {
fprintf(stderr,"Unable to open input file  %s\n",ifname);
return(0);
}
fl_list[cur_fd].fname = malloc(strlen(ifname)+1);
strcpy(fl_list[cur_fd].fname,ifname);
fl_list[cur_fd].line_number = 1;
if(strcmp(ifname,"debug") == 0 ) verbose = 1;
print_verbose(1);

init_table(); 			/* Initialize symbol_table */
tlist = init_token_list(); 	/* Initialize token list   */

return(1);
}


yyerror(s)		/* Standard YACC error print */
char *s;
{
fprintf(stderr,"%s\n",s);
}


char *itos(n)		/* Convert integer to character pointer */
int n;
{
char *s, *malloc();
extern double log10();

s = malloc(2+(int)log10((double)n));
sprintf(s,"%d",n);
return((char *)s);
}


/* 
** RECOVER  -  Perhaps the most intelligent routine yet 8-) Here we look at the
** token list and attempt to figure out what happened.  Recovery involves only
** detecting the type of error and printing a message.
*/

recover()
{
	extern struct tlist *tlist;
	struct tlist *tp;
	extern int paren_level;
	extern char errmess[];
	char  *malloc();
	int i;


	/*  Print out filename and line number where error encountered */
fprintf(stderr,"\"%s\", line %d: ",
fl_list[cur_fd].fname,fl_list[cur_fd].line_number);


tp = tlist;		/* set token pointer */

/* Try to figure out what type of error it was */
if ((i=ftn_err(tp))) {
sprintf(errmess, "Syntax error: '%s' is Fortran syntax",
tlist_symbol(tlist,i));
} 
else if( paren_level < 0) {
sprintf(errmess,"Missing left parenthesis");
} 
else if(paren_level >0) {
sprintf(errmess,"Missing right parenthesis");
} 
else if(tlist_token(tp,2)==FUNC_NAME && 
(tlist_token(tp,1)==Y_RP||tlist_token(tp,1)==Y_COM)){
sprintf(errmess,
"Illegal use of function or undeclared variable: '%s'",
tlist_symbol(tlist,2));
} 
else if(tlist_token(tp,1)==Y_RB && tlist_token(tp,2)==Y_LB) {
sprintf(errmess,
"Missing subscript in vector: '%s'",
tlist_symbol(tlist,3));
} 
else if(tlist_token(tp,2)==FUNC_NAME && tlist_token(tp,1)!=Y_LP){
sprintf(errmess,"Undeclared variable '%s'",
tlist_symbol(tlist,2));
} 
else if(tlist_token(tp,1)==Y_LC && tlist_token(tp,4)==FUNC_NAME) {
sprintf(errmess,"Syntax error - missing bracket?");
} 
else if(tlist_token(tp,1) != OP) {
sprintf(errmess,
"Missing operand or semicolon near symbol '%s'",
tlist_symbol(tlist,1));
} 
else {
sprintf(errmess, "Syntax error at or near symbol '%s'",
tlist_symbol(tp,2));
}

yyerror(errmess);	/* output the message to the screen */

/* update fl_list		    */
fl_list[cur_fd].errmess = malloc(strlen(errmess)+1);
strcpy(fl_list[cur_fd].errmess,errmess);

#ifdef MLDEBUG
{ 
int i;
for(i=0;i<16;i+=2) 
fprintf(stderr,"TOK %d=%d  SYM=:%s:\t\t\tTOK %d=%d  SYM=:%s:\n",
i,tlist_token(tlist,i),tlist_symbol(tlist,i),
i+1,tlist_token(tlist,i+1),tlist_symbol(tlist,i+1));
}
#endif


fflush(stderr);
}



/*
**  TLIST_TOKEN - Return the token of tlist-nback tokens in the
**  token list.
*/

tlist_token(tlist,nback)     	      /* Return token value of (tlist-nback) */
struct tlist *tlist;		      /* pointer to current structure        */
int nback;			      /* number back from current tlist      */
{
struct tlist *tp;
register int i;

tp = tlist;
for(i=0;i<nback;i++) 
tp = tp->back;
return(tp->token);
}


/*
**  TLIST_SYMBOL - Return the symbol name of tlist-nback tokens in the
**  token list.
*/

char *tlist_symbol(tlist,nback)       /* Return symbol name of (tlist-nback) */
struct tlist *tlist;		      /* pointer to current structure        */
int nback;			      /* number back from current tlist      */
{
struct tlist *tp;
register int i;

tp = tlist;
for(i=0;i<nback;i++) 
tp = tp->back;
return(tp->symbol);
}



/*
**  FTN_ERR - Check a dictionary to see if the symbol that caused the error
**  is perhaps a fortran statement.  Loop back through 4 tokens and check the
**  symbols.  
**
**  Returns:  The number back in the token list.
**
*/

ftn_err(tp)
struct tlist *tp;
{
	extern struct tlist *tlist;
	register int i,j;
	char buf[16], *tlist_symbol();
	static char *ftn_dict[] = { 
			"assign", 	"call", 	"common",
			"continue",	"dimension", 	"do",
			"entry", 	"equivalence", 	"external",
			"function", 	"goto", 	"implicit",
			"open", 	"pause", 	"print",
			"program", 	"read", 	"rewind",
			"save", 	"stop", 	"subroutine",
			"write"
			};
	static int num_dict = 22;

	for(i=1;i<=4;i++) {	              /* Loop through the tokens */
strcpy(buf, tlist_symbol(tp,i));	       /* Get the symbol */
j = 0;
while(buf[j] != '\0') {		/* convert to lower case */
if(isupper(buf[j])) buf[j] = tolower(buf[j]);
j++;
}
for(j=0; j<num_dict; j++) {		 /* Check the dictionary */
if(strcmp(buf, ftn_dict[j]) == 0)
return (i);	      	        /* found one!!!! */
}
}

return(0);				        /* nothing found */
}




/* 
**  RECERROR() - For a specific error that was trapped print the message
**  in the same format as RECOVER().
*/

recerror(s,a1,a2)
char *s, *a1, *a2;
{
	extern char errmess[];
	char *malloc();

	/*  Print out filename and line number where error encountered */
fprintf(stderr,"\"%s\", line %d: ",
fl_list[cur_fd].fname,fl_list[cur_fd].line_number);

sprintf(errmess,s,a1,a2);
yyerror(errmess);
fl_list[cur_fd].errmess = malloc(strlen(errmess)+1);
strcpy(fl_list[cur_fd].errmess,errmess);
}


/*  
**  LABGEN  -  Generate a label for the stack.  The top value is changed in the
**  grammar but label values are always increasing and so always unique
*/
labgen(top,number)
int *top, number;
{
	register int i;
	for(i=0;i<number;i++) {
		label_stack[*top] = label_counter;     
                label_counter++;     
		(*top)++;
	}
}




/*  
**   PRINT_INSTR  -  Print the instruction and argument to the output file
*/
     

print_instr(s,t)
	char *s, *t;
{
	char arg[30];

#ifdef MLDEBUG
        if( s == NULL || t == NULL) {
          if (s==NULL) fprintf(stderr,"NULL POINTER s IN PRINT_INSTR  t:%s:  %d\n",
				t,yytstk_index);
          else if (t==NULL) 
              fprintf(stderr,"NULL POINTER t in PRINT_INSTR  s:%s: %d\n",s,yytstk_index);
        } else fprintf(stderr,"PRINT_INSTR:  s=:%s:  t=:%s:\n",s,t);
        fflush(stderr);
	if (t != NULL && strcmp(t,"") != 0) fprintf(stderr,"\t&%s(%s)\n",s,t);
        else fprintf(stderr,"\t&%s\n",s);
	fflush(stderr);
#endif
	instruction(findsymbol(s),findsymbol(t));  /* Pass instruction to 
							interpreter */

	if (!NLOFILE) {			/* only if output file != NULL */
		if (strcmp(t,"") != 0) fprintf(fd_out,"\t&%s(%s)\n",s,t);
		else fprintf(fd_out,"\t&%s\n",s);
		fflush(fd_out);

		if (strcmp(s,"LABEL")==0 || strcmp(s,"LABEL1")==0 ||
			strcmp(s,"LABEL2")==0) {
			sprintf(arg,"%s:%d", 
				fl_list[cur_fd].fname,fl_list[cur_fd].line_number);
			print_instr("LINE",arg);
		}
	}
}



/*  
**  PROCESS_TOKEN  -  Add the token to TLIST and return it to the parser
*/

process_token(symbol,TOKEN)
char *symbol;
int TOKEN;
{
	char *malloc();

	tlist->token = TOKEN;
	tlist->symbol = malloc(strlen(symbol)+1);
	strcpy(tlist->symbol,symbol);	/* include symbol name    */

#ifdef MLDEBUG
fprintf(stderr,"TOKEN=%d tlist->token=%d symbol=:%s:\n",
TOKEN,tlist->token,tlist->symbol);
fflush(stderr);
#endif
tlist = tlist->next;		/*increment tlist pointer */

return(TOKEN);
}





/*
**   CLEANUP - Free all of the pointers allocated thus far.  And reset whatever
**   needs it for the next time.
**
**   Currently freed pointers include:
**		symtab		tlist
**		yytext_stack	func_stack
*/

cleanup()
{
	register int i;
	extern struct nlist *symtab, *symtab_last;
	extern struct tlist *tlist;
	struct nlist *np;
	struct tlist *tp;

	while(np != symtab_last) {	/* Now kill of the symtab  */
np = symtab;		
free((char *)np->sname);
symtab = np->next;
free((char *)np->next);
}
free(np);
free(symtab_last);

tp = tlist;			/* First delete token list */
tp->back->next = NULL;		/* sever circular link     */
free((char *)tp->back);	
while(tp!=NULL) {		/* do the rest of the list */
tp = tp->next;
if(tp!=NULL) {
free((char *)tp->back->next);
free((char *)tp->back);	
}
}
free((char *)tp);
free((char *)tlist);
}



/*
**  ECHO() - DEBUG PRINT
*/

echo(yyt)
char *yyt;
{
#ifdef MLDEBUG
	fprintf(stderr,"yytext = :%s:\n",yyt);
	fflush(stderr);
#endif
}
