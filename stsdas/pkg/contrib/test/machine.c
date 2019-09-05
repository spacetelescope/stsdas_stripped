/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

#define  import_spp
#define  import_libc
#define  import_stdio
#include <iraf.h>

#include "defines.h"
#include "machine.h"
#include "files.h"
#include "compile.h"
#include "strings.h"

SYMBOL *symtable = NULL;        /* symbol table */
static int tabmax=0;;		      /* length of allocated symtable */
static char **namelist;			 /* list of names (character strings ) */
static int nameptr;		      /* pointer into name list */
static int tableptr=0;            /* pointer into symbol table */

static int *core;			         /* core memory of abstract machine */
static int pcmax=0;               /*  last core address containing program */
static int coremax=0;             /*  size of currently allocated core space */

FILE *ftnout;

Ptr getnam(i)   /* get pointer to name(i) */
int i;
{
	return symtable[i].name;    /* look it up in the table */
}

putvalueptr(i,j)   /* put value into the table */
int i,j;
{
	symtable[i].valueptr = j;   /* put it in */
}

getvalueptr(i)  /* get value from table */
int i;
{
	return symtable[i].valueptr; /* get it */
}

int getcore(i) /* get instruction at location i of core */
int i;
{
	return *(core+i);
}

static entersymbol(token)    /* enter token into symbol table */
char *token;
{
	char *wordfill();
	if (tableptr >= tabmax)
		symtabspace();
	/* enter pntr to first character of name */
	symtable[tableptr].name = wordfill(token); 
	symtable[tableptr].label    = -1;	/* label is -1 */
	symtable[tableptr].valueptr = -1;	/* value is -1 */
	symtable[tableptr].findex   = -1;	/* not fastindexed */
	symtable[tableptr].filenum  = -1;	/* file unknown */
	symtable[tableptr].colnum   = -1;	/* column unknown */ 	
	symtable[tableptr].x.xvars[0] = -1;	/* index variables unknown */
	symtable[tableptr].x.xvars[1] = -1;	/* index variables unknown */
	symtable[tableptr].x.xvars[2] = -1;	/* index variables unknown */
	symtable[tableptr].x.xvars[3] = -1;	/* index variables unknown */
	symtable[tableptr].x.xvars[4] =  0;	/* zero dimensions */
	return tableptr++;
}

findsymbol(token)  /* find a token in the symbol table */
char *token;
{
	int i;

	for(i=0; i<tableptr; i++) { /* look through the table */
		/* if token is found return its symbol table position */
		if(strcmp(symtable[i].name,token)==0) return i;
	}
	return entersymbol(token);/* otherwise addd the new symbol into the table */
}

putinstruction(i) /* put new instruction into core */
int i;
{
	if(pcmax >= coremax) /* is there room in core */
		corespace();
	*(core+pcmax)=i; /* yes, install instruction and increment pcmax */
	pcmax++;
}

instruction(i,j)  /* put instruction into core */
int i,j;
{	
	switch(i) { /* different actions depending on opcode */
		case LINE: /* no op, not entered */
			return 0;
			break;                          
		case AMPERSAND:  /* no op, not entered */
		case LPAR:
		case RPAR:
			break;
		case DEFUN:  /* two byte isntructions with labels */
		case LABEL:
		case LABEL0:
		case LABEL1:
		case LABEL2:
			symtable[j].label = pcmax; /* enter label (address of instruction) into symbol table */
		case DEFVAR: /* two byte isntructions */
		case DEFVEC:
		case DEFPAR:
		case DEFOBS:
		case DEFDAT:
		case DEFARG:
		case DEFCON:
		case DEFINDEX:
		case DEFXCON:
		case DEFXPAR:
		case FCALL:
		case GOTO:
		case PUSH:
		case PUSHVEC:
		case STO:
		case STOVEC:
			putinstruction(i); /* put first byte */
			putinstruction(j); /* put second byte */
			break;
		case PUSHCON:
		case PUSHDIM:
			putinstruction(i); /* put first byte */
			putinstruction(j); /* put second byte */
			/* use index space for constant value */
			sscanf(symtable[j].name, "%lf", &symtable[j].x.c.cnst);
			break;

	default:     /* one byte instructions */
		putinstruction(i); 
		break;
	} 
	fprintf(ftnout,"      &%s(%s)\n",getnam(i),getnam(j));/* print instruciton in listing */
}

dumpcore()
{
	int i;

	for(i=0;i<pcmax;i++) /* print out contents of core byte by byte (symbolicaly) */
		printf("%s\n",getnam(*(core+i)));
}

nextsymbol(start,code) /* search for next instance of code */
int start,code;
{
	int i;

	for(i=start+1;i<pcmax;i++) /* sweep through memory from start + 1 to end */
		if(*(core+i)==code) /* look for code; if found */
			return i;  /* retrun its address */
	return -1;  /* otherwise returns -1 */
}

gotofn(pc) /* go to an address */
int *pc;
{
	trace2(*pc);  /* trace a 2 byte instruction */
	*pc = symtable[getcore(*pc + 1)].label; /* get label next to goto instruciton, set p.c. to it */
	if(*pc<0) /* if it's not a label */
		fatalerror("Attempt to jump to nonexistent label\n",""); /*error */
}

interpret(code)  /* interpret starting at code */
int code;
{
	int i,j;
	int start;

	start = symtable[code].label; /* get the label */
#ifdef DEBUG
	exit(0);
#endif
	switch(start) 
	{
	case -1: /* if label is -1, it's not a function */
		{
			fatalerror("Function %s does not exist.\n",getnam(code));
			return;
		}
		break;
	case -2: /* if its -2 */
		{
			if(code>(int)RPAR) { /* it's one of the built-in functions */
				(*opcodes[code].opfn)(code);
			} else {
				fatalerror
					("Function %s is not a built-in function.\n",getnam(code)); 
			}
			return;
		}
		break;
	default:  /* otherwise */
		/* execute while in the range of compiled instructions */
		for(i=start;i<pcmax;) {
			if((j=core[i])<(int)AMPERSAND) { /* if < AMPERSAND it's an opcode */
/*
#ifdef DEBUG4D
fprintf(stderr, ">%4d %8s %8s\n", i, getnam(j), getnam(core[i+1]));
#endif
*/
				(*opcodes[j].opfn)(&i); /* execute the opcode */
				switch(j) {
					case END: pushval(NULL);
					case ENDGLOB:
					case RETURN: return; /* if return, return */
				}
			} else {
				fatalerror("Attempt to execute a datum\n","");
			}
		}
	}
}

mycompile(s)   /* compile model */
char *s;
{
	struct ifile *errstat;

	printf("Compiling %s\n\n\n",s);
	initsymtab();   /* initialize symbol table */
	initliststruct();  /* initialize list structure */
	ftnout = fopen("FTN","w");  /* open listing file */
	errstat = compile(s,""); /* compile from file s */
	/* printf("After compile routine in mycompile\n");fflush(stdout);*/
	if (errstat != NULL) {   /* if error */
		fprintf(stderr,"\"%s\":line %d: Syntax error\n",
		errstat->fname,errstat->line_number); /* print message */
		exit(-2);  /* and exit */
	}

	fclose(ftnout);
}


corespace()  /* dynamically allocate corespace */
{
	int j;
	long intsz;

	coremax = coremax + 200;
	intsz = (long)coremax*sizeof(int);

	if (core == NULL)
		core = (int *)MemAlloc("core",intsz);
	else
		core = (int *)Reallocate("core",intsz,(char*)core);
	for (j = pcmax;j<coremax;j++)
		*(core+j) = 0;
}

symtabspace()  /* dynamically allocate symbol table space */
{
	int j,i;
	long symsiz;

	tabmax = tabmax + 40;
	symsiz = (long)tabmax*(sizeof(SYMBOL)+sizeof(SYMBOL*));

  	if (symtable == NULL) {
		symtable = (SYMBOL *)MemAlloc("SYMBOL",symsiz);
#ifdef DEBUG4D
	fprintf(stderr, "**** alloc symtable\n");
#endif
	} else
		symtable = (SYMBOL *)Reallocate("SYMBOL",symsiz,(char*)symtable);

	/* initialize symtabspace */
	for (i=tabmax-40;i< tabmax; i++) {
		symtable[i].name = NULL;
		symtable[i].label = 0;
		symtable[i].valueptr =  0;
		symtable[i].colnum   = -1;
		symtable[i].filenum  = -1;
		symtable[i].findex   = -1;
		symtable[i].x.xvars[0] = -1;
		symtable[i].x.xvars[1] = -1;
		symtable[i].x.xvars[2] = -1;
		symtable[i].x.xvars[3] = -1;
		symtable[i].x.xvars[4] =  0;
	}
}

char *wordfill(token)
char *token;
{
	char *p;
	int num;
	long strsize;

	strsize = (long)strlen(token) + 1;   /* length of string + 1 (for null byte ) */
	p=(char*)MemAlloc("token",strsize);  /* allocate a space to store a word */
	strcpy(p,token);
	return p;
}

dumpsymtable()
{
	int i;

	for (i=0;i< tabmax;i++)
	{
		printf ("%d  name = %s, label = %d, valueptr =%d\n",i,symtable[i].name,
		symtable[i].valueptr,symtable[i].label);
	}
}
