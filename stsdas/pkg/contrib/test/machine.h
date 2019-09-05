/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/


typedef struct symbol       /* entry in symbol table */
{
	Ptr name;           /* name of symbol */
	int label;          /* label of symbol */
	int valueptr;       /* pointer to stack containing value of symbol */
	int filenum;		/* file number of parameter */
	int colnum;			/* column number of parameter */
	union {
		int xvars[5];		/* pointers to index variables */  
		struct {
			double cnst;	/* constant for pushcon */
			int xvars4;		/* number of indices */
			int type;		/* variable type */
		} c;
	} x;
	int findex;			/* has this been fastindexed ? */
}
SYMBOL;

typedef struct opcode       /* entry in opcode table */
{
	char *opname;       /* name of opcode */
	int (*opfn)();      /* function that executes opcode */
}
OPCODE;

int nop();                  /* external functions executed by abstract machine */
int nop2();
int fcall();
int defpar();
int finis();
int finisglob();
int push();
int sto();
int add();
int sub();
int mpy();
int div();
int pwr();
int defarg();
int iffn();
int ifnot();
int pushcon();
int pushdim();
int gotofn();
int equalp();
int greaterp();
int lessp();
int differp();
int geqp();
int leqp();
int settrace0();
int settrace1();
int settrace2();
int settrace3();
int sqrtfn();
int truncfn();
int import();
int export();
int drop();
int defdat();
int defobs();
int defvar();
int dupl();
int notfn();
int orfn();
int andfn();
int exportconstraint();
int export2();
int logfn();
int triangularize();
int pushvec();
int stovec();
int defvec();
int sinfn();
int cosfn();
int atanfn();
int label0();
int label1();
int label2();
int defconst();
int chsfn();
int asinfn();
int acosfn();
int tanfn();
int expfn();
int log10fn();
int absfn();
int stripfn();
int defindex();
int defxcon();
int defxpar();

char *MemAlloc();
char *Reallocate();


static OPCODE opcodes[] = {           /* opcode table */
{ "ADD",add} ,
{ "ANDLP",andfn} ,
{ "BEGIN",nop} ,
{ "CHS",chsfn} ,
{ "DEFUN",nop2} ,
{ "DEFARG",defarg} ,
{ "DEFDAT",defdat} ,
{ "DEFOBS",defobs} ,
{ "DEFPAR",defpar} ,
{ "DEFVAR",defvar} ,
{ "DEFVEC",defvec} ,
{ "DEFCON",defconst} ,
/*
	{"DEFITM",defitm},
	{"DEFSET",defset},
	{"DEFITMCON",defitmconst},
	{"DEFSETCON",defsetconst},
*/
	{"DEFINDEX",defindex},
	{"DEFXCON",defxcon},
	{"DEFXPAR",defxpar},
	{"DIV",div},
	{"DIFFERP",differp},
	{"DROP",drop},
	{"DUPL",dupl},
	{"END",finis},
	{"ENDGLOB",finisglob},
	{"EQUALP",equalp},
	{"FCALL",fcall},
	{"GOTO",gotofn},
	{"GEQP",geqp},
	{"GREATERP",greaterp},
	{"IF",iffn},
	{"IFNOT",ifnot},
	{"LABEL",nop2},
	{"LABEL0",label0},
	{"LABEL1",label1},
	{"LABEL2",label2},
	{"LINE",nop2},
	{"LEQP",leqp},
	{"LESSP",lessp},
	{"MPY",mpy},
	{"NOT",notfn},
	{"OR",orfn},
	{"PUSH",push},
	{"PUSHCON",pushcon},
	{"PUSHDIM",pushdim},
	{"PUSHVEC",pushvec},
	{"PWR",pwr},
	{"RETURN",finis},
	{"STO",sto},
	{"STOVEC",stovec},
	{"SUB",sub},
	{"T0",settrace0},
	{"T1",settrace1},
	{"T2",settrace2},
	{"T3",settrace3},
	{"&",nop},
	{"(",nop},
	{")",nop},
	{"sqrt",sqrtfn},
	{"trunc",truncfn},
	{"log",logfn},
	{"import",import},
	{"export",export},
	{"export2",export2},
	{"exportconstraint",exportconstraint},
	{"triangularize",triangularize},
	{"sin",sinfn},
	{"cos",cosfn},	
	{"atan",atanfn},	
	{"asin",asinfn},	
	{"acos",acosfn},	
	{"tan",tanfn},	
	{"exp",expfn},	
	{"log10",log10fn},	
	{"abs",absfn},	
	{"strip",stripfn},	
	{"",nop}
};

enum {                          /* Tokens for opcodes */
ADD,
ANDLP,
BEGIN,
CHS,
DEFUN,
DEFARG,
DEFDAT,
DEFOBS,
DEFPAR,
DEFVAR,
DEFVEC,
DEFCON,
/*
	DEFITM,
	DEFSET,
	DEFITMCON,
	DEFSETCON,
*/
	DEFINDEX,
	DEFXCON,
	DEFXPAR,
	DIV,
	DIFFERP,
	DROP,
	DUPL,
	END,
	ENDGLOB,
	EQUALP,
	FCALL,
	GOTO,
	GEQP,
	GREATERP,
	IF,
	IFNOT,
	LABEL,
	LABEL0,
	LABEL1,
	LABEL2,
	LINE,
	LEQP,
	LESSP,
	MPY,
	NOT,
	OR,
	PUSH,
	PUSHCON,
	PUSHDIM,
	PUSHVEC,
	PWR,
	RETURN,
	STO,
	STOVEC,
	SUB,
	TRACEOP,
	TRACESTACK,
	UNTRACEOP,
	UNTRACESTACK,
	AMPERSAND,
	LPAR,
	RPAR
};
