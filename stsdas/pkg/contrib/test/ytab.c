/*
**	GAUSS - A System for Least Squares and Robust Estimation
**
**	Source Code Copyright (C) 1987 by William H. Jefferys,
**	Michael J. Fitzpatrick and Barbara E. McArthur
**	All Rights Reserved.
*/

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_ctype
#include <iraf.h>

# line 35 "ml.y"
#include "strings.h"
#include "symtab.h"
#include "def.h"
#include "compile.h"
#include "defines.h"
#include "machine.h"

#define  Echo    echo(yytext)
#define  OP	 512

FILE *fd_stack[10];			/* Max no. of include files - fd    */
					/* kept in a stack for convenience  */
extern FILE *fd_out;			/* Output file descriptor           */
extern struct ifile fl_list[];		/* struct containing error recovery */
extern SYMBOL *symtable;	/* symbol table */
int cur_fd = 0;				/* current working file descriptor  */
int colnum = 1;				/* column no. in input - not used   */
int verbose = 0;			/* verbose output flag              */
int nindices = 0;			/* number of array indices			*/
struct nlist *np;			/* Symbol table entry pointer       */
char errmess[120];			/* Error message buffer             */
char *malloc();				/* Space allocator                  */
char *Index();				/* Same as UNIX index() function    */
char *strbuf;				/* Generic string buffer	    */


	/*
	**  NOTES ON THE SYMBOL TABLE:
	**	The symbol table for the compiler is implemented as a linked
	**  list of structures containing the name and value of the token as
	**  well as pointers to maintain the list.  Initially all reserved
	**  words are inserted into the symtab with the appropriate token.
	**  New declared variables or functions are inserted and an entry is
	**  made as to their bracket level (with global vars and functions
	**  having a bracket level of zero).  After exit from each  bracket
	**  level names in the symtab with a level higher than the one just 
	**  exited are deletd from the symbol table.  This assures that locally
	**  declared variabled get used correctly.
	**
	**	The pointer 'symtab' contains a pointer to the first entry in 
	**  the table.  Likewise 'symtab_last' is a pointer to the last entry.
	**  Both of these are updated as needed.
	*/

extern struct nlist *symtab, 		/* Start pointer to symbol table    */
                    *symtab_last;	/* Tail pointer to symbol table     */
extern struct nlist *lookup(), 		/* Lookup a symbol from symtab      */
                    *insert(), 		/* Insert a new token into symtab   */
                    *entry_alloc();	/* Allocate space for a symtab entry*/
extern struct sysw syswords[];		/* Array containing reserved word   */
					/* structures.			    */
extern char *itos();			/* Converts integer to string       */
extern struct tlist *tlist;		/* Current pointer in token list    */

struct fstack{				/* Characteristics of each function */
	char *funcnm;			/* are kept to assure that arg count*/
	int argcnt;			/* will be correct                  */
};
struct fstack func_stack[MAX_STACK_SZ];	/* Stack of function names          */
  				        /*   encountered in each function   */
struct fstack *fsp, *get_func();	/* Get the no. of args to this func */
char *funcname;				/* Generic buffer for function names*/

char *yytext_stack[MAX_STACK_SZ]; 	/* Keep track of text found in      */
				    	/* analyzer so we can output it.    */
int label_stack[MAX_STACK_SZ];	  	/* Label stack - not yet implemented*/
int fstack_index = -1;		  	/* func_stack index 		    */
int yytstk_index=0;		  	/* yytext_stack index 		    */
int lstack_index=0;		  	/* label_stack index 		    */
int label_counter = 10001;	  	/* make sure we make unique labels  */
int line_number = 1;		  	/* current line number on input     */
int func_cnt = 0;	/* Count no. of functions seen so far               */
int npar[10];		/* # of parameters we've seen so far 		    */
int inpar = 0;		/* array counter for npar 			    */
int paren_level = 0;	/* # of parenthese encountered: +1 for (, etc       */
int bracket_level = 0;	/* # of brackets encountered: +1 for {, etc .       */
		        /* 'infunc' true if bracket_level = 1;  	    */ 
int infunc = 0;		/* flag if we're in a function. All other func-     */
 		        /* tion names are FCALL statements if true 	    */
int inargs = 0;		/* in the arglist ? */
int saw_nl = 0;		/* seen a newline yet?? 			    */
int saw_param = 0;	/* seen PARAMETER keyword yet? 			    */
int saw_eql = 0;	/* saw '=' ? Used to find LHS and RHS 		    */
int saw_lb = 0;		/* seen an '[' yet?				    */
int end_globals = 0;	/* end of global declarations 			    */
int in_vector = 0;	/* are we in an array ??  			    */
int if_seen = 0;	/* seen an 'if' yet? - for if-else conflicts        */
int left_vec = 0;	/* vector on LHS of eqn? 			    */
int par_type = 0;	/* type of parameter declaration 		    */
int parm_type = 0;	/* type of parameter declaration 		    */
int TWO_D_ARRAYS = 0;   /* Implement 2-D arrays? 0=>no  1=>yes		    */

extern int is_declaration_sysword(), is_valid_lhs();

/*  ERROR MESSGES - (not complete, see 'recover' in compile.c) */
char *already_decl_err =
	"Variable '%s' previously declared at this level.";
char *arg_count_error = 
	"Function '%s' previously called with %s arguments";
char *arg_type_error = 
	"Variable '%s' is of wrong type to be initialized.";
char *bad_lhs_error =
	"Invalid data type variable '%s' on LHS of equation.";
char *bad_vec_error =
	"DATA or OBSERVATION variable types may not be indexed/dimensioned";
char *decl_level_error =
	"Variable declarations are not allowed in statement blocks. ";
char *indx_type_error =
	"Only PARAMETER or CONSTANT variable types may be indexed. ";
char *mult_index_error =
	"Indented vectors are not legal on LHS of statement ";
char *o_indx_type_error =
	"Variable '%s' is of wrong type to be subscripted with '%s'";
char *obs_datatyp_error =
	"The data type '%s' is obsolete in this compiler release. ";
char *twod_nyi_error =
	"Two-dimensional arrays are not yet implemented. ";
char *varbl_type_error =
	"Only type VARIABLE is allowed to be explicitly dimensioned. ";
char *wrong_num_indices_error =
	"Reference of '%s' uses different number of indices than declared.";


# define DIGIT 257
# define LETTER 258
# define FUNC_NAME 259
# define PARAMS 260
# define SYS_WORDS 261
# define LOG_OP 262
# define CONST 263
# define VARIABLE 264
# define Y_NEWLINE 265
# define Y_STRING 266
# define Y_LP 267
# define Y_RP 268
# define Y_LB 269
# define Y_RB 270
# define Y_LC 271
# define Y_RC 272
# define Y_COM 273
# define Y_SMC 274
# define Y_CLN 275
# define EOFILE 276
# define EQUALS 277
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
# define INDEX 300
# define YOP_OR 306
# define YOP_AND 307
# define YOP_EQ 308
# define YOP_NE 309
# define YOP_LT 310
# define YOP_GT 311
# define YOP_LE 312
# define YOP_GE 313
# define YOP_PLUS 314
# define YOP_MINUS 315
# define YOP_MUL 316
# define YOP_DIV 317
# define YOP_EXP 318
# define YOP_NOT 319
# define UPLUS 320
# define UMINUS 321
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 957 "ml.y"



	/*  
	**  Here we parse the program and return tokens.  Token definitions
	**  are given above.  We first search for a character string and see
	**  if it's either a function name or a system_word.  
	**
	*/

#include "lexyy.h"


/*
**  YYWRAP - Override YACC function.  When and EOF is encountered handle
**  file stack or return code to end input processing.
*/

yywrap()
{
	extern struct ifile fl_list[];

	free(fl_list[cur_fd].fname);		/* Shift file pointers   */
	fclose(fd_stack[cur_fd]);		/* for include files     */
	cur_fd--;

	return( (cur_fd>=0 ? 0 : 1) );
}


/* 
**  GET_FUNC() - Search the function stack for the given name and return the 
**  struct.  Otherwise return a NULL.
*/
struct fstack *get_func(name)
char *name;
{
	register int i;

	for(i=0; i<=fstack_index; i++) {
#ifdef MLDEBUG
	fprintf(stderr,"GET_FUNC  %s <=> %s   %d  %d\n",
	name,func_stack[i].funcnm,bracket_level,yytstk_index);
#endif
		if( strcmp(name,func_stack[i].funcnm) == 0 )
			return(&(func_stack[i]));
	}
	return((struct fstack *)NULL);
}


/*
**  FIX_STACK() - Fix the func_stack so that the correct arg count is
**  inserted.
*/

fix_fstack(fsp)
struct fstack *fsp;
{
	register int i;

	for(i=0; i<=fstack_index; i++) {
		if( strcmp(fsp->funcnm,func_stack[i].funcnm) == 0 )
			func_stack[i].argcnt = fsp->argcnt;
	}
}


/*
**  PARM_DECL() - For a given delaration type print the proper instruction
**  and argument.
*/

parm_decl(type)
short type;
{
	extern char *itos();
	int symInx;

	yytstk_index--;
	switch(type) {
	    case PARAMETER:
		    print_instr("DEFPAR",yytext_stack[yytstk_index]); 
		    break;
	    case STARPARAM:
	    case ITEMPARAM:
		    print_instr("DEFITM",yytext_stack[yytstk_index]); 
		    break;
	    case SETPARAM:
	    case PLATEPARAM:
		    print_instr("DEFSET",yytext_stack[yytstk_index]); 
		    break;
	    case OBSERVATION:
		    print_instr("DEFOBS",yytext_stack[yytstk_index]); 
		    break;
	    case DATUM:
		    print_instr("DEFDAT",yytext_stack[yytstk_index]); 
		    break;
	    case VARIABL:
		    print_instr("DEFVAR",yytext_stack[yytstk_index]); 
	    	    break;
	    case CONSTANT:
		    print_instr("DEFCON",yytext_stack[yytstk_index]); 
	    	    break;
	    default:
		    break;
	}
	symInx = findsymbol(yytext_stack[yytstk_index]);
	symtable[symInx].x.c.type = par_type;
}



/*
**  PRINT_VERBOSE() - DEBUG print for functions
*/

print_verbose(flag)
int flag;		/* flag = 0 for functions, =1 for filenames  */
{
	register int i;

	if(verbose) {
		for(i=0;i<(flag?cur_fd:cur_fd+1);i++) printf("\t");
		if(flag) printf("%s:\n",fl_list[cur_fd].fname);
		else printf("%s\n", yytext_stack[yytstk_index]);
		fflush(stdout);
	}
}


short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 2,
	259, 8,
	-2, 3,
-1, 133,
	270, 84,
	273, 84,
	-2, 89,
-1, 134,
	270, 85,
	273, 85,
	-2, 88,
-1, 135,
	270, 86,
	273, 86,
	-2, 90,
	};
# define YYNPROD 110
# define YYLAST 466
short yyact[]={

 178, 105, 106, 107, 108, 109, 109, 174, 117, 110,
 111, 113, 112, 115, 114, 105, 106, 107, 108, 109,
 173, 110, 111, 113, 112, 115, 114, 105, 106, 107,
 108, 109, 116, 117, 110, 111, 113, 112, 115, 114,
 105, 106, 107, 108, 109, 116, 117, 110, 111, 113,
 112, 115, 114, 105, 106, 107, 108, 109, 116, 117,
 110, 111, 113, 112, 115, 114, 105, 106, 107, 108,
 109, 151, 116, 117, 110, 111, 113, 112, 115, 114,
 105, 106, 107, 108, 109, 107, 108, 109, 176, 161,
 130, 113, 112, 115, 114, 105, 106, 107, 108, 109,
  22, 122,  99, 124, 135,  49, 180,  60,  83, 116,
 117, 110, 111, 113, 112, 115, 114, 105, 106, 107,
 108, 109, 116, 117, 110, 111, 113, 112, 115, 114,
 105, 106, 107, 108, 109, 116, 117, 110, 111, 113,
 112, 115, 114, 105, 106, 107, 108, 109, 104,  22,
  97,  98, 179,  82,  49,  85,  84,  83,  61,  62,
  86,   9,  10,  11,  12,  13,  14,  15,  16,  59,
  32,  66,  17, 155,  74,  28,  72,  30,  33, 159,
 183, 116, 117, 110, 111, 113, 112, 115, 114, 105,
 106, 107, 108, 109, 175,  22, 170, 169, 160, 153,
  49, 152,  91,  90,  85,  84,  58,  28,  31,  86,
  48,  63,   3,  89,  51,  88,  75,  47, 186,  41,
  52,  40, 163,  46,   9,  10,  11,  12,  13,  14,
  15,  16,  19,   3,  53,  17, 103, 129, 128,  68,
  67,  29,  49,  26,   9,  10,  11,  12,  13,  14,
  15,  16, 177, 162, 123,  17,  93,  92,  22,  80,
  81, 132,  77, 126,  36,   9,  10,  11,  12,  13,
  14,  15,  16, 136,  65,  37,  17,  87,  24,  79,
  38,   7,  20,   7, 131, 101,   1,  23,  39,  18,
  50,  76,  73,  44,  43,  39,  27, 158, 157,  42,
  64, 187,  39,  39,  39, 182,  45,  69,  70,  71,
  55, 164,  96,  45, 154,  95,  35,  34,  54,  21,
  45,  45,  45,  57,  56,  25,   8,   6,  78,   5,
   4,   2,   0,   0,   0,   0,   0,   0,   0,  94,
   0,   0,   0,   0,   0,   0,   0, 100,   0,   0,
 102,   0,   0,   0,   0,   0, 127, 118, 119, 120,
 121, 134, 133,   0,   0,   0,   0, 137,   0, 125,
   0,   0,   0,   0,   0,   0,   0,   0,  78, 138,
 139, 140, 141, 142, 143, 144, 145, 146, 147, 148,
 149, 150,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0, 156,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
 134, 133, 168,  39,   0,   0,   0,   0, 171,   0,
 165, 166, 167,   0,   0, 134, 133, 181, 172,   0,
   0,  45, 127, 134, 133, 185, 184,  39,   0,   0,
   0,   0, 188,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,  45 };
short yypact[]={

 -23,-1000, -44,-1000,  -1,-1000,-127,-1000, -21,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
 -96, -26,-1000,-1000, -97, -65, -99,-1000, -64,-1000,
-1000, -21, -94,-156, -61, -64,-1000,-1000,-1000,-106,
 -27, -28, -64, -64, -64, -98,-1000,-100,-1000, -53,
-1000,-1000,-1000,-1000,-110,-1000, -55, -57, -70, -71,
-1000,  -6,  -7,-1000,-1000,-110,-1000,-1000,-1000,-135,
-177,-1000,-1000,-110,-1000,-1000,-110, -32,-125,-1000,
-1000,-1000,-1000,-110,-110,-110,-110,-1000,-1000,-1000,
-199,  -9,-1000,-1000,-171,-110, -22, -29, -30,-1000,
-184,-159,-234,-1000,-110,-110,-110,-110,-110,-110,
-110,-110,-110,-110,-110,-110,-110,-110,-197,-1000,
-1000,-1000, -72, -74,-1000,-234,-101,-106,-1000,-1000,
-1000, -91, -75,-1000,-1000,-1000,-234,-1000,-231,-231,
-312,-312,-312,-219,-219,-313,-313,-313,-313,-299,
-287,-1000,-211, -10, -46,-1000,-110,-110,-110,-1000,
-159, -76, -77, -64,-110,-234,-248,-261, -79,-212,
 -11,-1000,-274,-122,-168,-159,-1000,-1000,-1000,-1000,
-1000, -93, -22,-159, -50,-1000,-1000, -64,-1000 };
short yypgo[]={

   0, 286, 331, 330, 277, 275, 329, 327, 280, 326,
 278, 325, 324, 323, 319, 318, 262, 273, 317, 316,
 264, 259, 274, 315, 314, 312, 263, 311, 305, 301,
 299, 298, 297, 294, 293, 292, 291, 290, 285, 284,
 261, 260, 279 };
short yyr1[]={

   0,   1,   1,   1,   1,   2,   3,   6,   6,   7,
   7,   8,   9,   9,   9,   9,   9,   9,   9,   9,
   9,  10,  10,  11,  11,  11,  11,  11,  11,  12,
  12,  12,  12,  13,  13,  13,  13,  15,   4,  14,
  16,  16,  16,   5,  18,  18,  19,  19,  20,  20,
  20,  23,  24,  20,  25,  27,  28,  29,  20,  31,
  20,  32,  20,  20,  20,  20,  35,  20,  20,  20,
  26,  36,  33,  34,  30,  37,  37,  21,  38,  21,
  39,  39,  39,  39,  40,  40,  40,  17,  17,  42,
  42,  41,  41,  41,  41,  41,  41,  41,  41,  41,
  41,  41,  41,  41,  41,  41,  41,  41,  41,  22 };
short yyr2[]={

   0,   2,   2,   1,   1,   3,   1,   1,   0,   1,
   2,   3,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   3,   1,   4,   4,   3,   4,   4,   1,
   3,   5,   7,   1,   3,   5,   7,   0,   5,   1,
   0,   1,   3,   3,   1,   0,   1,   2,   1,   1,
   4,   0,   0,   7,   0,   0,   0,   0,  13,   0,
   8,   0,   8,   2,   2,   2,   0,   4,   2,   1,
   3,   0,   3,   3,   1,   1,   1,   1,   0,   5,
   1,   3,   5,   7,   1,   1,   1,   1,   1,   1,
   1,   3,   3,   3,   3,   3,   3,   2,   2,   3,
   3,   3,   3,   3,   3,   3,   3,   2,   1,   1 };
short yychk[]={

-1000,  -1,  -2, 256,  -3,  -6,  -7,  -8,  -9, 288,
 289, 290, 291, 292, 293, 294, 295, 299,  -1, 276,
  -4, -14, 259,  -8, -10, -11, 264,  -5, 271, 267,
 274, 273, 269, 277, -18, -19, -20,  -5,  -8, -21,
 285, 283, -30, -33, -34,  -4, 287, 281, 274, 264,
 -37, 278, 284, 298, -15, -10, -12, -13, 300, 263,
 263, 314, 315, 272, -20, -22, 277, 267, 267, -20,
 -20, -20, 274, -35, 274, 269, -36, -16, -17, -42,
 -21, -41, 263, 267, 315, 314, 319,  -4, 270, 270,
 273, 273, 263, 263, -17, -23, -25, 285, 286, 279,
 -17, -38, -17, 268, 273, 314, 315, 316, 317, 318,
 308, 309, 311, 310, 313, 312, 306, 307, -17, -17,
 -17, -17, 300, 263, 274, -17, -26, -21, 267, 267,
 274, -39, -40, -41, -21, 263, -17, -16, -17, -17,
 -17, -17, -17, -17, -17, -17, -17, -17, -17, -17,
 -17, 268, 273, 273, -24, 274, -22, -31, -32, 270,
 273, 300, 263, 268, -27, -17, -17, -17, -40, 273,
 273, -20, -17, 268, 268, 273, 300, 263, 274, 274,
 274, -40, -28, 273, -26, -40, 268, -29, -20 };
short yydef[]={

   8,  -2,  -2,   4,   0,   6,   7,   9,   0,  12,
  13,  14,  15,  16,  17,  18,  19,  20,   1,   2,
   0,   0,  39,  10,   0,  21,  23,   5,  45,  37,
  11,   0,   0,   0,   0,  44,  46,  48,  49,   0,
   0,   0,   0,   0,   0,   0,  66,   0,  69,  77,
  74,  71,  75,  76,  40,  22,   0,   0,  29,  33,
  26,   0,   0,  43,  47,   0, 109,  51,  54,   0,
  63,  64,  65,   0,  68,  78,   0,   0,  41,  87,
  88,  89,  90,   0,   0,   0,   0, 108,  24,  25,
   0,   0,  27,  28,   0,   0,   0,   0,   0,  73,
   0,   0,  72,  38,  40,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,  97,
  98, 107,  30,  34,  50,  52,   0,   0,  59,  61,
  67,   0,  80,  -2,  -2,  -2,   0,  42,  92,  93,
  94,  95,  96,  99, 100, 101, 102, 103, 104, 105,
 106,  91,   0,   0,   0,  55,   0,   0,   0,  79,
   0,  31,  35,   0,   0,  70,   0,   0,  81,   0,
   0,  53,   0,   0,   0,   0,  32,  36,  56,  60,
  62,  82,   0,   0,   0,  83,  57,   0,  58 };

#
# define YYFLAG -1000
# define YYERROR goto yyerrlab
# define YYACCEPT return(0)
# define YYABORT return(1)

/*	parser for yacc output	*/

#ifdef YYDEBUG
int yydebug = 0; /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar = -1; /* current input token number */
int yynerrs = 0;  /* number of errors */
short yyerrflag = 0;  /* error recovery flag */

yyparse() {

	short yys[YYMAXDEPTH];
	short yyj, yym;
	register YYSTYPE *yypvt;
	register short yystate, *yyps, yyn;
	register YYSTYPE *yypv;
	register short *yyxi;

	yystate = 0;
	yychar = -1;
	yynerrs = 0;
	yyerrflag = 0;
	yyps= &yys[-1];
	yypv= &yyv[-1];

 yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
	if( yydebug  ) printf( "state %d, char 0%o\n", yystate, yychar );
#endif
		if( ++yyps> &yys[YYMAXDEPTH] ) { yyerror( "yacc stack overflow" ); return(1); }
		*yyps = yystate;
		++yypv;
		*yypv = yyval;

 yynewstate:

	yyn = yypact[yystate];

	if( yyn<= YYFLAG ) goto yydefault; /* simple state */

	if( yychar<0 ) if( (yychar=yylex())<0 ) yychar=0;
	if( (yyn += yychar)<0 || yyn >= YYLAST ) goto yydefault;

	if( yychk[ yyn=yyact[ yyn ] ] == yychar ){ /* valid shift */
		yychar = -1;
		yyval = yylval;
		yystate = yyn;
		if( yyerrflag > 0 ) --yyerrflag;
		goto yystack;
		}

 yydefault:
	/* default state action */

	if( (yyn=yydef[yystate]) == -2 ) {
		if( yychar<0 ) if( (yychar=yylex())<0 ) yychar = 0;
		/* look through exception table */

		for( yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2 ) ; /* VOID */

		while( *(yyxi+=2) >= 0 ){
			if( *yyxi == yychar ) break;
			}
		if( (yyn = yyxi[1]) < 0 ) return(0);   /* accept */
		}

	if( yyn == 0 ){ /* error */
		/* error ... attempt to resume parsing */

		switch( yyerrflag ){

		case 0:   /* brand new error */

			recover();return(1);
		yyerrlab:
			++yynerrs;

		case 1:
		case 2: /* incompletely recovered error ... try again */

			yyerrflag = 3;

			/* find a state where "error" is a legal shift action */

			while ( yyps >= yys ) {
			   yyn = yypact[*yyps] + YYERRCODE;
			   if( yyn>= 0 && yyn < YYLAST && yychk[yyact[yyn]] == YYERRCODE ){
			      yystate = yyact[yyn];  /* simulate a shift of "error" */
			      goto yystack;
			      }
			   yyn = yypact[*yyps];

			   /* the current yyps has no shift onn "error", pop stack */

#ifdef YYDEBUG
			   if( yydebug ) printf( "error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1] );
#endif
			   --yyps;
			   --yypv;
			   }

			/* there is no state on the stack with an error shift ... abort */

	yyabort:
			return(1);


		case 3:  /* no shift yet; clobber input char */

#ifdef YYDEBUG
			if( yydebug ) printf( "error recovery discards char %d\n", yychar );
#endif

			if( yychar == 0 ) goto yyabort; /* don't discard EOF, quit */
			yychar = -1;
			goto yynewstate;   /* try again in the same state */

			}

		}

	/* reduction by production yyn */

#ifdef YYDEBUG
		if( yydebug ) printf("reduce %d\n",yyn);
#endif
		yyps -= yyr2[yyn];
		yypvt = yypv;
		yypv -= yyr2[yyn];
		yyval = yypv[1];
		yym=yyn;
			/* consult goto table to find next state */
		yyn = yyr1[yyn];
		yyj = yypgo[yyn] + *yyps + 1;
		if( yyj>=YYLAST || yychk[ yystate = yyact[yyj] ] != -yyn ) yystate = yyact[yypgo[yyn]];
		switch(yym){
			
case 4:
# line 208 "ml.y"
{ yyerrok;	} break;
case 11:
# line 226 "ml.y"
{ saw_param = 0;	} break;
case 12:
# line 230 "ml.y"
{  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = PARAMETER;  
			     } break;
case 13:
# line 237 "ml.y"
{  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				recerror(obs_datatyp_error, "starparam","");
				YYABORT;
			     } break;
case 14:
# line 244 "ml.y"
{  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				recerror(obs_datatyp_error, "itemparam","");
				YYABORT;
			     } break;
case 15:
# line 251 "ml.y"
{ if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				recerror(obs_datatyp_error, "plateparam","");
				YYABORT;
			     } break;
case 16:
# line 258 "ml.y"
{  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				recerror(obs_datatyp_error, "setparam","");
				YYABORT;
			     } break;
case 17:
# line 265 "ml.y"
{  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = OBSERVATION;
			     } break;
case 18:
# line 272 "ml.y"
{ if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = DATUM;      
			     } break;
case 19:
# line 279 "ml.y"
{  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = VARIABL;    
			     } break;
case 20:
# line 286 "ml.y"
{  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = CONSTANT;   
			     } break;
case 23:
# line 300 "ml.y"
{    /* RULE:  parg:  VARIABLE      		   */

		     /* Declare the variable with appropriate type */
		     parm_decl(par_type);
		} break;
case 24:
# line 306 "ml.y"
{   /* RULE:  parg  |  VARIABLE Y_LB INDEX Y_RB   */
			int symInx;

		    /*  Handle an indexed variable		   */
					/* Only allow certain types*/
			if(par_type != PARAMETER && par_type != CONSTANT) {
				if(par_type == DATUM || par_type == OBSERVATION)
					recerror(bad_vec_error,"","");
		     	else
					recerror(indx_type_error,"","");
				--yytstk_index;
				YYABORT;   /* print error and die		   */
			}
			yytstk_index--;
			if(par_type == PARAMETER) {       /* PARAMETER index*/
				print_instr("DEFXPAR",yytext_stack[yytstk_index]);
			} else if (par_type == CONSTANT) {/* CONSTANT index */
				print_instr("DEFXCON",yytext_stack[yytstk_index]);
			}
			symInx = findsymbol(yytext_stack[yytstk_index]);
			symtable[symInx].x.c.xvars4 = nindices;
			symtable[symInx].x.c.type = par_type;
			nindices = 0;
		} break;
case 25:
# line 331 "ml.y"
{   /* RULE:   parg  |  VARIABLE Y_LB CONST Y_RB*/
			int symInx;

			if(par_type != VARIABL) {
				if(par_type == DATUM || par_type == OBSERVATION)
					recerror(bad_vec_error,"","");
		     	else
		       		recerror(varbl_type_error,"","");
				--yytstk_index;
		   		YYABORT;
			}
		    /* Define a vector variable			*/
		    print_instr("DEFVEC",yytext_stack[--yytstk_index]);
			symInx = findsymbol(yytext_stack[yytstk_index]);
			symtable[symInx].x.c.xvars4 = nindices;
			symtable[symInx].x.c.type = par_type;
			nindices = 0;
		} break;
case 26:
# line 350 "ml.y"
{  /* RULE:   parg  |  VARIABLE EQUALS CONST     */
			int symInx;

		   /* Allow type VARIABLE to be initialized      */
		 if(par_type != VARIABL) {  /* Wrong type for this */
		     recerror(arg_type_error,yytext_stack[yytstk_index-2],"");
		     yytstk_index -= 2;
		     YYABORT;    /* print message and die   */
	         }
		 --yytstk_index;

		print_instr("DEFVAR",yytext_stack[yytstk_index-1]);
		symInx = findsymbol(yytext_stack[yytstk_index-1]);
		symtable[symInx].x.c.type = par_type;
		print_instr("PUSHCON",yytext_stack[yytstk_index]);
		print_instr("STO",yytext_stack[--yytstk_index]);
		} break;
case 27:
# line 368 "ml.y"
{  /* RULE:   parg  |  VARIABLE EQUALS YOP_PLUS CONST     */
			int symInx;

		   /* Allow type VARIABLE to be initialized      */
		 if(par_type != VARIABL) {  /* Wrong type for this */
		     recerror(arg_type_error,yytext_stack[yytstk_index-2],"");
		     yytstk_index -= 2;
		     YYABORT;    /* print message and die   */
	         }
		 --yytstk_index;
		 print_instr("DEFVAR",yytext_stack[yytstk_index-1]);
		symInx = findsymbol(yytext_stack[yytstk_index-1]);
		symtable[symInx].x.c.type = par_type;
		 print_instr("PUSHCON",yytext_stack[yytstk_index]);
		 print_instr("STO",yytext_stack[--yytstk_index]);
		} break;
case 28:
# line 385 "ml.y"
{  /* RULE:   parg  |  VARIABLE EQUALS YOP_MINUS CONST     */
			int symInx;

		   /* Allow type VARIABLE to be initialized      */
		 if(par_type != VARIABL) {  /* Wrong type for this */
		     recerror(arg_type_error,yytext_stack[yytstk_index-2],"");
		     yytstk_index -= 2;
		     YYABORT;    /* print message and die   */
	         }
		 --yytstk_index;
		 print_instr("DEFVAR",yytext_stack[yytstk_index-1]);
		symInx = findsymbol(yytext_stack[yytstk_index-1]);
		symtable[symInx].x.c.type = par_type;
		 strbuf = malloc(128);
		 sprintf(strbuf,"-%s\0", yytext_stack[yytstk_index]);
		 print_instr("PUSHCON",strbuf);
		 print_instr("STO",yytext_stack[--yytstk_index]);
		 free(strbuf);
		} break;
case 29:
# line 408 "ml.y"
{
		   print_instr("DEFINDEX",yytext_stack[--yytstk_index]);
		   nindices = 1;
		} break;
case 30:
# line 413 "ml.y"
{
		   print_instr("DEFINDEX",yytext_stack[--yytstk_index]);
		   print_instr("DEFINDEX",yytext_stack[--yytstk_index]);
		   nindices = 2;
		} break;
case 31:
# line 419 "ml.y"
{
		   print_instr("DEFINDEX",yytext_stack[--yytstk_index]);
		   print_instr("DEFINDEX",yytext_stack[--yytstk_index]);
		   print_instr("DEFINDEX",yytext_stack[--yytstk_index]);
		   nindices = 3;
		} break;
case 32:
# line 426 "ml.y"
{
		   print_instr("DEFINDEX",yytext_stack[--yytstk_index]);
		   print_instr("DEFINDEX",yytext_stack[--yytstk_index]);
		   print_instr("DEFINDEX",yytext_stack[--yytstk_index]);
		   print_instr("DEFINDEX",yytext_stack[--yytstk_index]);
		   nindices = 4;
		} break;
case 33:
# line 438 "ml.y"
{
		   print_instr("PUSHDIM",yytext_stack[--yytstk_index]);
		   nindices = 1;
		} break;
case 34:
# line 443 "ml.y"
{
		   print_instr("PUSHDIM",yytext_stack[--yytstk_index]);
		   print_instr("PUSHDIM",yytext_stack[--yytstk_index]);
		   nindices = 2;
		} break;
case 35:
# line 449 "ml.y"
{
		   print_instr("PUSHDIM",yytext_stack[--yytstk_index]);
		   print_instr("PUSHDIM",yytext_stack[--yytstk_index]);
		   print_instr("PUSHDIM",yytext_stack[--yytstk_index]);
		   nindices = 3;
		} break;
case 36:
# line 456 "ml.y"
{
		   print_instr("PUSHDIM",yytext_stack[--yytstk_index]);
		   print_instr("PUSHDIM",yytext_stack[--yytstk_index]);
		   print_instr("PUSHDIM",yytext_stack[--yytstk_index]);
		   print_instr("PUSHDIM",yytext_stack[--yytstk_index]);
		   nindices = 4;
		} break;
case 37:
# line 474 "ml.y"
{ inargs=1; } break;
case 38:
# line 475 "ml.y"
{   /*  RULE:  func_del :  func_name Y_LP func_arg Y_RP */
			inargs = 0;
		    func_cnt++;			/* increment func count */
		    if(bracket_level != 0) {    /* calling this function*/
		        print_instr("FCALL",yytext_stack[--yytstk_index]);
		    }
		    infunc--;
		    if(bracket_level != 0)   /* Get correct arg count    */
		         fsp = get_func(yytext_stack[yytstk_index]);
		    else
		         fsp = get_func(funcname);
		    if(fsp != NULL) {  /* saw this function somewhere */
#ifdef MLDEBUG
	fprintf(stderr,"FSP:  name=:%s: argcnt=%d npar=%d   %d inpar = %d\n",
	fsp->funcnm,fsp->argcnt,npar[inpar],fstack_index,inpar);
#endif
					/* error in number of args    */
		       if (fsp->argcnt != npar[inpar] && fsp->argcnt != -1) {
			 recerror(arg_count_error,
			 (bracket_level!=0?yytext_stack[yytstk_index]:funcname),
			 (fsp->argcnt<npar[inpar]?"fewer":"more"));
			 YYABORT;
		       } else if(fsp->argcnt == -1) {   /*  OK  */
				fsp->argcnt = npar[inpar];
				fix_fstack(fsp);
		       }
		    } else {    /* new function - set up args counted  */
			func_stack[fstack_index].argcnt = npar[inpar];
		   }
		   inpar--;
		} break;
case 39:
# line 509 "ml.y"
{   /* RULE:  func_name:  FUNC_NAME                     */


		    /* Here we declare a new function, otherwise it gets
		    ** handle up above
		    */
		    infunc++; inpar++; npar[inpar] = 0; func_cnt++;
		    if(bracket_level == 0) {		/* new function */
		       /*if(func_cnt<=1) print_instr("ENDGLOB","");*/
		       labgen(&lstack_index, 1);
		       print_instr("GOTO",itos(label_stack[lstack_index-1]));
		       print_instr("DEFUN",yytext_stack[--yytstk_index]);
		       print_verbose(0);
		       funcname = malloc(strlen(yytext_stack[yytstk_index])+1);
		       strcpy(funcname,yytext_stack[yytstk_index]);
		    }
		} break;
case 41:
# line 533 "ml.y"
{   /* RULE:  func_arg :  expr 				   */

		    /*  Handle local arg names				   */
		    if(bracket_level <= 0) {
				int symInx;
		        print_instr("DEFARG",yytext_stack[--yytstk_index]); 
				symInx = findsymbol(yytext_stack[yytstk_index]);
				symtable[symInx].x.c.xvars4 = -1;
		    }
		    npar[inpar]++;	/* count number of args           */
		} break;
case 42:
# line 545 "ml.y"
{   /* RULE:  func_arg :  expr Y_COM func_arg   	   */

		    /*  Handle local arg names				   */
		    if(bracket_level <= 0) {
				int symInx;
		        print_instr("DEFARG",yytext_stack[--yytstk_index]);
				symInx = findsymbol(yytext_stack[yytstk_index]);
				symtable[symInx].x.c.xvars4 = -1;
		    }
		    npar[inpar]++;	/* count number of args           */
		} break;
case 43:
# line 565 "ml.y"
{   /* RULE:  compound_stmt:  Y_LC optional_stmts Y_RC */
		    --bracket_level;
		    if(bracket_level == 0) {   /* End of compound_stmt */
			print_instr("END",""); 
			print_instr("LABEL",itos(label_stack[lstack_index-1]));
			lstack_index--;
			in_vector = 0; 
			infunc=0;
			saw_eql = 0; 
			if_seen = 0;
			left_vec = 0;
		     }
		} break;
case 50:
# line 591 "ml.y"
{  /* RULE:  stmt  | ref equals expr Y_SMC               */
			/* check for legal LHS */
		   np=lookup(yytext_stack[yytstk_index-1]); 
		   if(np->tokval == VARIABLE) {
		     if(!is_valid_lhs(np->par_type) ){
			fprintf(stderr, "name=:%s:  par_type=%d  tokval=%d\n",
				np->sname,np->par_type,
				np->tokval); fflush(stderr);
			recerror(bad_lhs_error,yytext_stack[yytstk_index-1],"");
			YYABORT;
		     }
		   }
			yytstk_index--;  
	  	   if(in_vector && left_vec)  {
			print_instr("STOVEC",yytext_stack[yytstk_index]);
		   } else {		  
		    	print_instr("STO",yytext_stack[yytstk_index]);
		   } 

		   saw_eql=0; in_vector=0; infunc=0; left_vec = 0;
		} break;
case 51:
# line 613 "ml.y"
{      /* RULE:  stmt  | WHILE Y_LP           		*/
			labgen(&lstack_index,3); /* Initialize labels   */
			print_instr("LABEL0",itos(label_stack[lstack_index-3]));
			print_instr("LABEL1",itos(label_stack[lstack_index-2]));
			infunc=1;
	        } break;
case 52:
# line 620 "ml.y"
{      /* RULE:  WHILE.....  expr			*/ 
			print_instr("IFNOT","");  /* evaluate expression*/
			print_instr("GOTO",itos(label_stack[lstack_index-1]));
			infunc=0;
		} break;
case 53:
# line 626 "ml.y"
{      /* RULE:  WHILE.....  Y_RP stmt			*/
			
			print_instr("GOTO",itos(label_stack[lstack_index-2]));
			print_instr("LABEL2",itos(label_stack[lstack_index-1]));

			lstack_index -= 3;
		} break;
case 54:
# line 634 "ml.y"
{	/* RULE:  stmt |  FOR  Y_LP			*/	
			labgen(&lstack_index,5);  /* Initialize labels	*/
			print_instr("LABEL0",itos(label_stack[lstack_index-5]));
		} break;
case 55:
# line 640 "ml.y"
{	/* RULE: FOR... stmt1 Y_SMC....			*/	
			print_instr("LABEL1",itos(label_stack[lstack_index-4]));
			infunc=1;
		} break;
case 56:
# line 645 "ml.y"
{	/* RULE: FOR... expr Y_SMC....			*/	
			print_instr("IFNOT",""); /* evaluate expression */
			print_instr("GOTO",itos(label_stack[lstack_index-1]));
			print_instr("GOTO",itos(label_stack[lstack_index-3]));
			print_instr("LABEL",itos(label_stack[lstack_index-2]));
			infunc=0;
		} break;
case 57:
# line 653 "ml.y"
{ 	/* RULE:  FOR....  stmt1 Y_RP...		*/	
			print_instr("GOTO",itos(label_stack[lstack_index-4]));
			print_instr("LABEL",itos(label_stack[lstack_index-3]));
		} break;
case 58:
# line 658 "ml.y"
{	/* RULE:  FOR....  stmt...			*/	
			print_instr("GOTO",itos(label_stack[lstack_index-2]));
			print_instr("LABEL2",itos(label_stack[lstack_index-1]));
			
			lstack_index -= 5;
		} break;
case 59:
# line 664 "ml.y"
{infunc=1;} break;
case 60:
# line 665 "ml.y"
{	/* RULE: doprefix stmt WHILE Y_LP expr Y_RP Y_SMC */	

			print_instr("IFNOT","");  /* evaluate expression  */
			print_instr("GOTO",itos(label_stack[lstack_index-1]));
			print_instr("GOTO",itos(label_stack[lstack_index-2]));
			print_instr("LABEL2",itos(label_stack[lstack_index-1]));
			infunc=0;

			lstack_index -= 3;
		} break;
case 61:
# line 675 "ml.y"
{infunc=1;} break;
case 62:
# line 676 "ml.y"
{	/* RULE: doprefix stmt UNTIL Y_LP expr Y_RP Y_SMC */	

		 	print_instr("IF","");  /* evaluate expression  */
			print_instr("GOTO",itos(label_stack[lstack_index-1]));
			print_instr("GOTO",itos(label_stack[lstack_index-2]));
			print_instr("LABEL2",itos(label_stack[lstack_index-1]));
			infunc=0;

			lstack_index -= 3;
		} break;
case 63:
# line 687 "ml.y"
{	/* RULE: stmt | ifprefix stmt			*/	
			print_instr("LABEL",itos(label_stack[lstack_index-2]));
			lstack_index -= 2;
		} break;
case 64:
# line 692 "ml.y"
{	/* RULE: stmt | ifelprefix stmt			*/	
			print_instr("LABEL",itos(label_stack[lstack_index-1]));
			lstack_index -= 2;
		} break;
case 65:
# line 697 "ml.y"
{   	/* RULE: func_del Y_SMC				*/

			/* Straight function call - drop result */
			print_instr("DROP","");
		    	infunc=0; saw_eql = 0; in_vector = 0;
		} break;
case 66:
# line 703 "ml.y"
{ saw_eql = 1; inargs = 1; } break;
case 67:
# line 704 "ml.y"
{    	/* RULE:  stmt | RETURN expr Y_SMC		*/
			inargs = 0;
			print_instr("RETURN","");
			saw_eql = 0;
		} break;
case 68:
# line 710 "ml.y"
{    	/* RULE:  stmt | BREAK Y_SMC			*/
			print_instr("GOTO",itos(label_stack[lstack_index-1]));
		} break;
case 70:
# line 718 "ml.y"
{  /* RULE:  stmt1  | ref equals expr */
			/* check for legal LHS */
			np=lookup(yytext_stack[yytstk_index-1]); 
			if(np->tokval == VARIABLE) {
				if(!is_valid_lhs(np->par_type) ){
					recerror(bad_lhs_error,yytext_stack[yytstk_index-1],"");
					YYABORT;
				}
			}
			yytstk_index--;  
			if(in_vector && left_vec)  {
				print_instr("STOVEC",yytext_stack[yytstk_index]);
			} else {		  
				print_instr("STO",yytext_stack[yytstk_index]);
			} 
			saw_eql=0; in_vector=0; infunc=0; left_vec = 0;
		} break;
case 71:
# line 738 "ml.y"
{ infunc=1; if_seen++; } break;
case 72:
# line 739 "ml.y"
{ /* RULE: ifprefix: IF expr				*/
		  
		  /* Begginning to IF-THEN expression  */
		  infunc=0; 
		  labgen(&lstack_index,2);
	  	  print_instr("IFNOT","");
		  print_instr("GOTO",itos(label_stack[lstack_index-2]));
		} break;
case 73:
# line 753 "ml.y"
{ /* RULE: ifelprefix: ifprefix stmt ELSE		*/
		  
		  /* Begginning to IF-THEN-ELSE  expression  */
		  /*lstack_index += 2;   /* restore label_stack */
			print_instr("GOTO",itos(label_stack[lstack_index-1]));
		 	print_instr("LABEL",itos(label_stack[lstack_index-2]));
		} break;
case 74:
# line 767 "ml.y"
{	/* RULE:  doprefix : do_or_loop			*/
	
			/* Initialize labels for DO loop		*/
			labgen(&lstack_index,3);
			print_instr("LABEL0",itos(label_stack[lstack_index-3]));
			print_instr("LABEL1",itos(label_stack[lstack_index-2]));
		} break;
case 77:
# line 790 "ml.y"
{   /* RULE: ref :  VARIABLE			*/

		    if((infunc||saw_eql||in_vector) && (bracket_level > 0) ) {
				if (!inargs) {
					int symInx;
					symInx = findsymbol(yytext_stack[yytstk_index-1]);
					if (symtable[symInx].x.c.xvars4 != -1 
						&& nindices != symtable[symInx].x.c.xvars4
						&& symtable[symInx].x.c.type == VARIABL) {
						recerror(wrong_num_indices_error,
							yytext_stack[yytstk_index-1],"");
						YYABORT;
					}
				}
				print_instr("PUSH",yytext_stack[--yytstk_index]); 
		    }
		} break;
case 78:
# line 807 "ml.y"
{in_vector++; } break;
case 79:
# line 808 "ml.y"
{   /* RULE: ref :  VARIABLE Y_LB index_list Y_RB */
			int symInx;

		    /*in_vector++;*/
		    if(!saw_eql) left_vec = 1;
			symInx = findsymbol(yytext_stack[yytstk_index-1]);
			if (symtable[symInx].x.c.xvars4 != -1 
				&& nindices != symtable[symInx].x.c.xvars4) {
				recerror(wrong_num_indices_error,
					yytext_stack[yytstk_index-1],"");
				YYABORT;
			}
			nindices = 0;
		    if(in_vector > 1 && left_vec && !saw_eql && !infunc) {
				recerror(mult_index_error,"","");
				YYABORT;
		    }
			if(((infunc||saw_eql) && (bracket_level>0)) ) {
				print_instr("PUSHVEC",yytext_stack[--yytstk_index]);
			}
		} break;
case 80:
# line 834 "ml.y"
{ nindices = 1; } break;
case 81:
# line 836 "ml.y"
{ nindices = 2; } break;
case 82:
# line 838 "ml.y"
{ nindices = 3; } break;
case 83:
# line 840 "ml.y"
{ nindices = 4; } break;
case 86:
# line 845 "ml.y"
{ 
			if(infunc||saw_eql||in_vector) {
				print_instr("PUSHCON", yytext_stack[--yytstk_index]);	
			}
		} break;
case 90:
# line 861 "ml.y"
{ 
			if(infunc||saw_eql||in_vector) {
				print_instr("PUSHCON", yytext_stack[--yytstk_index]);	
			}
		} break;
case 91:
# line 871 "ml.y"
{    yyval = yypvt[-1];	} break;
case 92:
# line 873 "ml.y"
{   print_instr("ADD","");
		} break;
case 93:
# line 876 "ml.y"
{   print_instr("SUB","");
		} break;
case 94:
# line 879 "ml.y"
{   print_instr("MPY","");
		} break;
case 95:
# line 882 "ml.y"
{   print_instr("DIV","");
		} break;
case 96:
# line 885 "ml.y"
{   print_instr("PWR","");
		} break;
case 97:
# line 888 "ml.y"
{   yyval =   - yypvt[-0];  print_instr("CHS","");
		} break;
case 98:
# line 891 "ml.y"
{   yyval =   yypvt[-0];  
		} break;
case 99:
# line 894 "ml.y"
{   print_instr("EQUALP","");
		} break;
case 100:
# line 897 "ml.y"
{   print_instr("DIFFERP","");
		} break;
case 101:
# line 900 "ml.y"
{   print_instr("GREATERP","");
		} break;
case 102:
# line 903 "ml.y"
{   print_instr("LESSP","");
		} break;
case 103:
# line 906 "ml.y"
{   print_instr("GEQP","");
		} break;
case 104:
# line 909 "ml.y"
{   print_instr("LEQP","");
		} break;
case 105:
# line 912 "ml.y"
{   print_instr("OR","");
		} break;
case 106:
# line 915 "ml.y"
{   print_instr("ANDLP","");
		} break;
case 107:
# line 918 "ml.y"
{   print_instr("NOT","");
		} break;
case 109:
# line 953 "ml.y"
{  saw_eql = 1; } break;
		}
		goto yystack;  /* stack new state and value */

	}
