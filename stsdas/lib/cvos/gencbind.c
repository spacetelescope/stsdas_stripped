/*
** To run the program:
**
**	gencbind OPTIONS
**
** where OPTIONS are one or more of
**	-f <master file name> 
**	-c <code generation option> 
**	-d <spp source code directory>
Default values for the options are:
**	master file name:		stdin
**	code generation option:		c
**	spp source code directory:	./
** Alternative values for the code generation option are c++ and idl.
** (However, idl doesn't do anything; it is equivalent to c.)
**
** Original code written by Allen Farris.
** M.D. De La Pena  24 February 1998 -- Modified C-binding generation code
** to map IRAF long ints to C ints.  In IRAF, long ints and ints have the
** same number of bits.
** M.D. De La Pena  13 January 1999 - Modified code such that parameters which 
** are multidimensional arrays and are NOT char, resolve to TYPE *. 
*/

# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <ctype.h>

/* These constants are the maximum size of a line, including any */
/* continuations, the maximum size of a name, and the maximum  */
/* number of parms in a procedure. */
#define maxlinesize 1024
#define maxnamesize 63
#define maxparms 64

enum spp_type { 
	NONE, INT, CHAR, REAL, DOUBLE, POINTER, LONG, SHORT, BOOL, COMPLEX, VOID };
const int SPP_SIZE[11] = { /* sizes of SPP data types in bytes */
	0,    4,   2,    4,    8,      4,       4,    2,     4,    8,       0 };

enum io_option { BOTH, INPUT_ONLY, OUTPUT_ONLY };

enum decl_obj { SCALAR, ARRAY, PROCEDURE };

enum code_generation_option { ANSI_C, C_PLUS_PLUS, IDL };

typedef enum spp_type SPP_TYPE;
typedef enum io_option IO_OPT;
typedef enum decl_obj DECL_OBJ;
typedef enum code_generation_option CODE_GEN_OPT;

/* Data structure for holding information about SPP procedures. */
/* This data structure is used to capture data from the master file */
/* and the SPP text file and is used to drive the code generation process. */
typedef struct { 
	char *source_code_name;		/* SPP source code filename */
	SPP_TYPE rtn_type; 		/* return type of SPP procedure */
	SPP_TYPE rtn_conv;		/* optional return conversion type */
	char *spp_name;			/* SPP procedure name */
	char *extern_spp;		/* external name of SPP procedure */
	char *c_name;			/* C function name */
	int no_parms;			/* number of parms */
	char *sppparm[maxparms];	/* the SPP parm name */
	char *cparm[maxparms];		/* the C parm name */
	IO_OPT option[maxparms]; 	/* I/O attributes of parms */
	SPP_TYPE parm_conv[maxparms];	/* optional parm conversion type */
	SPP_TYPE parm_type[maxparms];	/* data type of parm */
	char *parm_size[maxparms];	/* size of output char arrays */
	DECL_OBJ parm_arrayid[maxparms];/* array indicator of parm */
	int no_dims[maxparms];		/* # dimensions if parm is an array */
} ProcInfo;

/* a very unintelligent error termination procedure */
void err(char *s) {
	printf("%s\n",s);
	exit(0);
}

/* convert an SPP_TYPE to a printable string */
char * pr_SPP_TYPE(SPP_TYPE t) {
	char *o;
	switch (t) {
	    case NONE:		o = "none"; break;
	    case INT:		o = "int"; break;
	    case CHAR:		o = "char"; break;
	    case REAL:		o = "real"; break;
	    case DOUBLE:	o = "double"; break;
	    case POINTER:	o = "pointer"; break;
	    case LONG:		o = "long"; break;
	    case SHORT:		o = "short"; break;
	    case BOOL:		o = "bool"; break;
	    case COMPLEX:	o = "complex"; break;
	    case VOID:		o = "void"; break;
	}
	return o;
}

/* Given an SPP_TYPE, return a string that is the C data type */
const char *c_type(SPP_TYPE t) {
	switch (t) {
	    case NONE:	  return "****";
	    case INT:	  return "int"; 
	    case CHAR:	  return "char"; 
	    case REAL:	  return "float"; 
	    case DOUBLE:  return "double"; 
	    case POINTER: return "IRAFPointer"; 
/*MDD
	    case LONG:	  return "long"; 
*/
	    case LONG:	  return "int"; 
	    case SHORT:	  return "short"; 
	    case BOOL:	  return "Bool"; 
	    case COMPLEX: return "Complex"; 
	    case VOID:	  return "void"; 
	}
}

/* a useful debugging tool */
void prProcInfo(ProcInfo *proc) {
	int i;
	printf("%s%s%s%s<%s>%s%s%s%s\n",
		"ProcInfo: ------------- \nproc.source_code_name = ",
		proc->source_code_name,
		"\nproc.rtn_type = ",
		pr_SPP_TYPE(proc->rtn_type),
		pr_SPP_TYPE(proc->rtn_conv),
		"   proc->spp_name = ",
		proc->spp_name,
		"   proc.c_name = ",
		proc->c_name);
	for (i = 0; i < proc->no_parms; ++i) {
	    printf("%d:  %s<%s> [%s] %s(%s) ", i, 
		pr_SPP_TYPE(proc->parm_type[i]),
		pr_SPP_TYPE(proc->parm_conv[i]),
		(proc->parm_size[i] == 0 ? "0" : proc->parm_size[i]), 
		proc->sppparm[i], proc->cparm[i]);
	    switch(proc->parm_arrayid[i]) {
		case SCALAR:	printf("scalar "); break;
		case ARRAY:	printf("array %d",proc->no_dims[i]); break;
		case PROCEDURE:	printf("procedure "); break;
	    }
	    switch(proc->option[i]) {
		case BOTH:	  printf("both "); break;
		case INPUT_ONLY:  printf("input only "); break;
		case OUTPUT_ONLY: printf("output only "); break;
	    }
	    printf("\n");
	}
}

/* Given a string representing an SPP type, */
/* Return the SPP_TYPE */
SPP_TYPE isa_type(char *s) {
	if (strcmp(s,"int") == 0) return INT;
	if (strcmp(s,"char") == 0) return CHAR;
	if (strcmp(s,"real") == 0) return REAL;
	if (strcmp(s,"double") == 0) return DOUBLE;
	if (strcmp(s,"pointer") == 0) return POINTER;
	if (strcmp(s,"long") == 0) return LONG;
	if (strcmp(s,"short") == 0) return SHORT;
	if (strcmp(s,"bool") == 0) return BOOL;
	if (strcmp(s,"complex") == 0) return COMPLEX;
	if (strcmp(s,"void") == 0) return VOID;
	return NONE;
}


/* Given a string representing a type to convert an SPP type to, */
/* Note:  This allows conversion to void, which should rarely be used */
/* and can generate bad code.  In particular IRAF pointers should not */
/* be converted to type void because you must do arithmetic on them. */
/* Return the SPP_TYPE */
SPP_TYPE isa_conv(char *s) {
	if (strcmp(s,"i") == 0) return INT;
	if (strcmp(s,"c") == 0) return CHAR;
	if (strcmp(s,"r") == 0) return REAL;
	if (strcmp(s,"f") == 0) return REAL;
	if (strcmp(s,"d") == 0) return DOUBLE;
	if (strcmp(s,"p") == 0) return POINTER;
	if (strcmp(s,"l") == 0) return LONG;
	if (strcmp(s,"s") == 0) return SHORT;
	if (strcmp(s,"b") == 0) return BOOL;
	if (strcmp(s,"x") == 0) return COMPLEX;
	if (strcmp(s,"v") == 0) return VOID;
	if (strcmp(s,"int") == 0) return INT;
	if (strcmp(s,"char") == 0) return CHAR;
	if (strcmp(s,"real") == 0) return REAL;
	if (strcmp(s,"float") == 0) return REAL;
	if (strcmp(s,"double") == 0) return DOUBLE;
	if (strcmp(s,"pointer") == 0) return POINTER;
	if (strcmp(s,"long") == 0) return LONG;
	if (strcmp(s,"short") == 0) return SHORT;
	if (strcmp(s,"bool") == 0) return BOOL;
	if (strcmp(s,"complex") == 0) return COMPLEX;
	if (strcmp(s,"void") == 0) return VOID;
	return NONE;
}

/* Return the number of valid characters in a name. */
/* A valid name is a sequence of alphanumeric chars or '_', */
/* the first one of which is an alphabetic char. */
int get_name(char *s) {
	int n = 0;
	if (!isalpha(*s))
	    return 0;
	while (isalnum(*s) || *s == '_') {
	    ++n;
	    ++s;
	}
	return n;
}

/* 
** MAIN program
**
*/
int main(int argc, char **argv) {
	char line[maxlinesize];
	char cline[maxlinesize];
	int n;
	char *s;
	FILE *ifile; /* the input master file to be read */
	FILE *hfile; /* the .h file to be created */
	FILE *cfile; /* the .c file to be created */
	char *ofilename;
	ProcInfo proc; /* structure to capture parse results about a proc */
	void parse_proc_line(ProcInfo *, char *);
	void process_text(ProcInfo *, FILE *, FILE *, CODE_GEN_OPT, char *);
	CODE_GEN_OPT code_gen_opt;
	char *mastername;
	char *spp_directory;
	
	/* Set the default options */
	ifile = stdin; /* the default input master file */
	mastername = "stdin";
	code_gen_opt = ANSI_C;
	spp_directory = "./";

	for (n = 1; n < argc; ++n) {
	    s = argv[n];
	    if (*s == '-') ++s;
	    if (*s == 'f') {
	        ++s;
	        if (*s == '\0') {
	            ++n;
		    s = argv[n];
		}
		mastername = s;
		ifile = fopen(s,"r"); /* open the master file */
		if (ifile == NULL)
		    err("Could not open master file");
	    } else if (*s == 'c') {
	        ++s;
		if (*s == '\0') {
		    ++n;
		    s = argv[n];
		}
		if (strcmp(s,"C") == 0 || strcmp(s,"c") == 0)
		    code_gen_opt = ANSI_C;
		else if (strcmp(s,"C++") == 0 || strcmp(s,"c++") == 0)
		    code_gen_opt = C_PLUS_PLUS;
		else if (strcmp(s,"IDL") == 0 || strcmp(s,"idl") == 0)
		    code_gen_opt = IDL;
		else
		    err("Invalid option");
	    } else if (*s == 'd') {
		++s;
		if (*s == '\0') {
		    ++n;
		    s = argv[n];
		}
		spp_directory = s;
	    } else
	        err("Invalid syntax: cvos -f <filename> -c <option> -d <dir>");
	}
	printf("Processing master file %s with option ", mastername);
	switch (code_gen_opt) {
	    case ANSI_C:      printf("C"); break;
	    case C_PLUS_PLUS: printf("C++"); break;
	    case IDL:         printf("IDL"); break;
	}
	printf("\n");

	line[0] = '\0';	
	fgets(line,maxlinesize,ifile); /* get the output file prefix */
	n = strlen(line);
	if (n == 0)
	    err("Improper first line of input master file");
	/* trim the name */
	for (s = line; *s == ' ' || *s == '\t'; ++s) ;
	--n; if (line[n] == '\n') --n;
	for (; line[n] == ' ' || line[n] == '\t'; --n) ;
	++n;
	line[n] = '\0';
	ofilename = (char *)calloc(n + 2, sizeof(char));
	if (ofilename == NULL)
	    err("Could not allocate memory");
	strcpy(ofilename,line);
	strcat(ofilename,".h"); 
	hfile = fopen(ofilename,"w"); /* open the .h file */
	if (hfile == NULL)
	    err("Could not open .h output file");
	ofilename[n] = '\0';
	strcat(ofilename,".c");
	cfile = fopen(ofilename,"w"); /* open the .c file */
	if (cfile == NULL)
	    err("Could not open .c output file");

	/* read the lines from the master file and process the procedures */
	while ( fgets(line,maxlinesize,ifile) ) {
	    n = strlen(line);
	    /* eliminate any end-of-line char */
	    if (line[n - 1] == '\n') {
		--n;
		line[n] = '\0';
	    }
	    s = strchr(line,':');
	    /* eliminate comment lines and all blank lines */
	    if (line[0] == '#' || n == 0 || (s != 0 && s[1] == '#'))
	    	continue;
	    if (s) {
	        ++s;
	        while (*s == ' ' || *s == '\t') ++s;
	        if (*s == '#')
		    continue;
	    }
	    /* write any special lines to .h or .c files */
	    if (line[0] == '/' && line[2] == '/') {
		if (line[1] == 'h')
		    fprintf(hfile,"%s\n",&line[3]);
		else if (line[1] == 'c')
		    fprintf(cfile,"%s\n",&line[3]);
		else
		    err("Illegal line in master file");
		continue;
	    }
	    for (s = line; *s == ' ' || *s == '\t'; ++s) ;
	    if (*s == '\0' || *s == '#')
		continue;
	    /* check for continuation lines */
	    if (line[n - 1] == '\\') {
	    	do {
		    line[strlen(line) - 1] = '\0';
		    if (fgets(cline,maxlinesize,ifile) == NULL)
			err("Unexpected end-of-file in master file");
		    n = strlen(cline);
		    /* eliminate any end-of-line char */
		    if (cline[n - 1] == '\n') {
			--n;
			cline[n] = '\0';
		    }
		    if ((n + (int)strlen(line)) >= maxlinesize)
			err("Input line in master file is too long");
		    strcat(line,cline);
		} while (cline[n - 1] == '\\');
	    }
	    parse_proc_line(&proc,line);
	    process_text(&proc, hfile, cfile, code_gen_opt, spp_directory);
	}
	free(ofilename);
        return 0;
}

void parse_proc_line(ProcInfo *proc, char *line) {
	SPP_TYPE isa_type(char *s);
	SPP_TYPE isa_conv(char *s);
	char *s, *t, *x, *y;
	int i, n;

	/* initialize the ProcInfo structure */
	proc->source_code_name = 0;
	proc->rtn_type = NONE;
	proc->rtn_conv = NONE;
	proc->spp_name = 0;
	proc->extern_spp = 0;
	proc->c_name = 0;
	proc->no_parms = 0;
	for (i = 0; i < maxparms; ++i) {
	    proc->sppparm[i] = 0;
	    proc->cparm[i] = 0;
	    proc->option[i] = BOTH;
	    proc->parm_conv[i] = NONE;
	    proc->parm_type[i] = NONE;
	    proc->parm_size[i] = 0;
	    proc->parm_arrayid[i] = SCALAR;
	    proc->no_dims[i] = 0;
	}

	while (*line == ' ' || *line == '\t') ++line; /* skip white space */
	s = strchr(line,':'); /* get SPP source code file name */
	if (s == 0)
	    err("Invalid syntax in master file");
	*s = '\0';
	proc->source_code_name = line;

	++s;
	while (*s == ' ' || *s == '\t') ++s; /* skip white space */

	/* look for procedure name following procedure keyword */
	if (strncmp(s,"procedure",9) == 0) {
	    proc->rtn_type = VOID;
	    s += 9;
	} else {
	    x = s;
	    while (*s != ' ' && *s != '\t') ++s; /* find end of name */
	    *s = '\0';
	    /* search for possible pointer conversion */
	    if (*(s - 1) == '>') {
	        y = strchr(x,'<');
		if (y == 0)
		    err("Invalid syntax in master file");
		*y++ = '\0';
		*(s - 1) = '\0';
		proc->rtn_conv = isa_conv(y);
		if (proc->rtn_conv == NONE)
		    err("Invalid data type in conversion specification");
	    } else
		proc->rtn_conv = NONE;
	
	    proc->rtn_type = isa_type(x);
	    ++s;
	    while (*s == ' ' || *s == '\t') ++s; /* skip white space */
	    if (strncmp(s,"procedure",9) != 0)
		err("Invalid syntax in master file");
	    s += 9;
	}

	while (*s == ' ' || *s == '\t') ++s; /* skip white space */
	proc->spp_name = s; 	/* store SPP procedure name */
	while (*s != ' ' && *s != '\t' && *s != '(') ++s; /* find end of name */
	if (*s != '(')
	    *s++ = '\0';
	while (*s == ' ' || *s == '\t') ++s; /* skip white space */
	if (*s != '(') 	/* look for open paren, the start of parm list */
	    err("Invalid syntax in master file");	
	*s++ = '\0';
	proc->c_name = strchr(proc->spp_name,'|'); 	/* and C procedure name */
	if (proc->c_name == 0) 		/* which might be different */
	    proc->c_name = proc->spp_name;
	else
	    *proc->c_name++ = '\0';
	proc->extern_spp = strchr(proc->spp_name,'$'); /* and external spp name */
	if (proc->extern_spp != 0)
	    *proc->extern_spp++ = '\0';

	/* store each parm, together with attribute type */
	/* close paren marks end of parm list */
	t = s; /* search for matching closing paren */
	n = 1;
	while (*t != '#' && *t != '\0') {
	    if (*t == '(') ++n;
	    if (*t == ')') --n;
	    if (n == 0) break;
	    ++t;
	}
	if (*t != ')')
	    err("Invalid syntax in master file");
	for(;;) {
            if (proc->no_parms >= maxparms)
		err("Too many parameters.");
	    while (*s == ' ' || *s == '\t') ++s; /* skip white space */
	    if (*s == ')')
		break;
	    proc->sppparm[proc->no_parms] = s;
	    /* find end of name */
	    while (*s != ' ' && *s != '\t' && *s != ',' && *s != ')') ++s; 
	    if (*s != ',') {
		if (*s == ')') {
		    *s = '\0';
		    ++proc->no_parms;
		    break;
		}
		*s = '\0';
		++s;	    
	    	while (*s == ' ' || *s == '\t') ++s; /* skip white space */
		if (*s != ',')
	            err("Invalid syntax in master file");
	    } else
		*s = '\0';
	    ++s;
	    ++proc->no_parms;
	}

	for (i = 0; i < proc->no_parms; ++i) {
	    s = strchr(proc->sppparm[i],':');
	    if (s == 0) {
	        proc->option[i] = BOTH;
		s = proc->sppparm[i];
	    } else {
	        *s = '\0';
	        ++s;
	        if (*s == 'i' || *s == 'I')
	            proc->option[i] = INPUT_ONLY;
	        else if (*s == 'o' || *s == 'O')
	            proc->option[i] = OUTPUT_ONLY;
	        else
	            err("Illegal I/O specifier in master file");
		++s;
	    }
	    x = s; /* save position in string */
	    s = strchr(s,'<'); /* check for conversion specification */
	    if (s == 0)
		proc->parm_conv[i] = NONE;
	    else {
		*s = '\0';
	        ++s;
		y = strchr(s,'>');
		if (y == 0)
		    err("Invalid syntax in master file specifying conversion");
		*y = '\0';
		proc->parm_conv[i] = isa_conv(s);
		if (proc->parm_conv[i] == NONE)
		    err("Invalid data type in conversion specification");
	    }
	    x = strchr(x,'['); /* check for string size specification */
	    if (x != 0) {
		*x = '\0';
	        ++x;
		y = strchr(x,']');
		if (y == 0)
		    err("Invalid syntax in master file specifying size");
		*y = '\0';
		/* change braces to parens */
		y = strchr(x,'{');
		while (y != 0) {
		    *y = '(';
		    y = strchr(x,'{');
		}
		y = strchr(x,'}');
		while (y != 0) {
		    *y = ')';
		    y = strchr(x,'}');
		}
		proc->parm_size[i] = x;
	    }
	    proc->cparm[i] = strchr(proc->sppparm[i],'|'); /* get C name */
	    if (proc->cparm[i] == 0) 		/* which might be different */
	        proc->cparm[i] = proc->sppparm[i];
	    else
	        *(proc->cparm[i])++ = '\0';
	}

}

void process_text(ProcInfo *proc, FILE *hfile, FILE *cfile,
	CODE_GEN_OPT code_gen_opt, char *dir) {
	int  no_decl;			 /* these hold the results */
	SPP_TYPE decl_type;              /* from parsing a */
	char *decl_name[maxparms]; 	 /* declaration statement */
	DECL_OBJ decl_arrayid[maxparms];
	int decl_dims[maxparms];
	int no_saved_decl = 0;
	char *saved_decl[maxparms];		
	char *textname;
	FILE *text;			/* the SPP source code file */

	int i, j, n;
	char *s, *w1, *w2, *w3;
        char saveline[maxlinesize];

	char line[maxlinesize];
        int save_line, more;

	SPP_TYPE isa_type(char *s);
	int parse_decl(char *, FILE *, SPP_TYPE *, char **, DECL_OBJ *, int *);
	int generate_code(FILE *, FILE *, ProcInfo *, int, char **, 
		CODE_GEN_OPT);

	/* open the spp text file */
	n = strlen(dir);
	if (dir[n - 1] == '/') {
	    n += strlen(proc->source_code_name) + 1;
	    textname = (char *)calloc(n, sizeof(char));
	    if (textname == NULL)
	        err("Could not allocate memory");
	    strcpy(textname,dir);
	    strcat(textname,proc->source_code_name);
	} else {
	    n += strlen(proc->source_code_name) + 2;
	    textname = (char *)calloc(n, sizeof(char));
	    if (textname == NULL)
	        err("Could not allocate memory");
	    strcpy(textname,dir);
	    strcat(textname,"/");
	    strcat(textname,proc->source_code_name);
	}
	text = fopen(textname,"r");
	if (text == NULL) {
	    printf("Could not open file %s\n",proc->source_code_name);
	    exit(0);
	}
	free(textname);
	/* inform the user */
	printf("Processing file %s procedure %s\n",
		proc->source_code_name, proc->spp_name);


	/* find the proper procedure declaration statement */
	while ( fgets(line,maxlinesize,text) ) {

	    /* eliminate help blocks, comments, and blank lines */
	    if (strncmp(line,".help",5) == 0) {
		while ( fgets(line,maxlinesize,text) )
		    if (strncmp(line,".endhelp",8) == 0)
			break;
		if (line && (strncmp(line,".endhelp",8) != 0))
		    err("Invalid syntax in SPP source ocde file");
		continue;
	    }
	    n = strlen(line);
	    /* eliminate any end-of-line char */
	    if (line[n - 1] == '\n') {
		--n;
		line[n] = '\0';
	    }
	    s = line;
	    while (*s == ' ' || *s == '\t') ++s; /* skip white space */
	    if (n == 0 || *s == '\0' || *s == '#')
		continue;

	    /* search for procedure statement -- syntax is: */
	    /*     ^{WS}?{return-type}?"procedure"{WS}{spp-name} */
	    w1 = s;
	    if (*w1 == '\0') continue;
	    while (*s != ' ' && *s != '\t' && *s != '\0') ++s;
	    if (*s == '\0') continue;
	    *s = '\0';
	    if (strcmp(w1,"procedure") == 0) {
		w1 = "void";
		w2 = "procedure";
	    } else {
	        ++s;
	        while (*s == ' ' || *s == '\t') ++s; /* skip white space */
	        w2 = s;
		if (*w2 == '\0') continue;
	        while (*s != ' ' && *s != '\t' && *s != '\0') ++s;
	        *s = '\0';
	    }
	    if (strcmp(w2,"procedure") == 0) {
		++s;
	        while (*s == ' ' || *s == '\t') ++s; /* skip white space */
	        w3 = s;
		if (*w3 == '\0') continue;
/*changed       while (*s != ' ' && *s != '\t' && *s != '\0') ++s;*/
	        while (*s != ' ' && *s != '\t' && *s != '\0' && *s != '(') ++s;
	        *s = '\0';
	    } else
		w3 = 0;
	    if (strcmp(w2,"procedure") != 0 || (isa_type(w1) != proc->rtn_type) ||
	        strcmp(w3,proc->spp_name) != 0)
	    	continue;

	    /* Initialize the data type of the parms */
	    for (i = 0; i < proc->no_parms; ++i)
		proc->parm_type[i] = NONE;

	    /* OK, now process the declaration statements until the `begin' */
	    /* statement is reached or until all parms have been found */
	    while ( fgets(line,maxlinesize,text) ) {
	    	n = strlen(line);
	    	/* eliminate any end-of-line char */
	    	if (line[n - 1] == '\n') {
		    --n;
		    line[n] = '\0';
	    	}
                strcpy(saveline,line);
		n = parse_decl(line, text, &decl_type, decl_name, 
			decl_arrayid, decl_dims);
	        if (n == -1)
		    break;
	      if (n > 0) {
		/* match the declaration names with the parm names */
		save_line = 0;
		for (j = 0; j < n; ++j)
		    for (i = 0; i < proc->no_parms; ++i)
			if (strcmp(decl_name[j],proc->sppparm[i]) == 0) {
			    save_line = 1;
			    proc->parm_type[i] = decl_type;
			    proc->parm_arrayid[i] = decl_arrayid[j];
			    proc->no_dims[i] = decl_dims[j];
			    break;
			}
		for (i = 0; i < n; ++i) /* delete the allocated decl names */
                    free(decl_name[i]);
		if (save_line) {
		    saved_decl[no_saved_decl] = 
			(char *)calloc(strlen(saveline) + 1, sizeof(char));
                    if (saved_decl[no_saved_decl] == 0)
			err("Unable to allocate memory");
		    strcpy(saved_decl[no_saved_decl],saveline);
		    ++no_saved_decl;
                }
		/* check to see if we are done */
		more = 0;
		for (i = 0; i < proc->no_parms; ++i)
		    if (!proc->parm_type[i]) {
			more = 1;
			break;
		    }
		if (!more)
		    break;
	      }
	    }
	    break;
	} /* endwhile */
	/* check to see if all parms were found */
	more = 0;
	for (i = 0; i < proc->no_parms; ++i)
	    if (!proc->parm_type[i]) {
		more = 1;
		break;
	    }
	if (more)
	    err("Parm names do not match SPP declaration names");

	/* generate code */
	n = generate_code(hfile, cfile, proc, no_saved_decl, saved_decl, 
		code_gen_opt);

	for (i = 0; i < no_saved_decl; ++i)
            free(saved_decl[i]);

	fclose(text);
}

int parse_decl(char *line, FILE *text, SPP_TYPE *type,
	char **name, DECL_OBJ *arrayid, int *dims) {
/* Syntax: */
/*	^{data-type}{WS}{dcl-name}{WS}?{","{WS}?{dcl-name}{WS}?}* */
/*	dcl-name := {name}{{WS}? "[" {dimension-list} "]"}? */
/*	         OR {name}"()" */
/* Usual convention regarding continuation (last operator is ',') */
/* and comments. */


/* Returns the number of variable names (0 means there were no valid */
/*	declarations, -1 means the presence of a `begin' statement */
/* Other output variables: */
/*	data type */
/*	list of variable names and array indicators */

	char *s, *x;
	int n;
	char temp[maxnamesize + 1];
	int no_names;

	int get_name(char *);
	SPP_TYPE isa_type(char *);

	s = line;

	/* check to see if this is a begin statement */
	while (*s == ' ' || *s == '\t') ++s; /* skip white space */
	if (strncmp(s,"begin",5) == 0)
	    return -1;
	/* check if this is a comment */
	if (*s == '#')
	    return 0;

	/* get the first word and see if it is a data type */
	n = get_name(line);
	if (n > maxnamesize)
	    return 0;
	strncpy(temp,line,n);
	temp[n] = '\0';
	*type = isa_type(temp);
	if (*type == NONE)
	    return 0;

	/* OK, it's a data type keyword */
	no_names = 0;
	s = line + n;
	for(;;) {
	    if (no_names >= maxparms)
		err("Too many parameters in SPP procedure");
 	    while (*s == ' ' || *s == '\t') ++s; /* skip white space */
	    x = s;
	    n = get_name(s);
	    if (n > maxnamesize || n == 0)
		err("Syntax error in SPP procedure");
	    s += n;
	    while (*s == ' ' || *s == '\t') ++s; /* skip white space */
	    if (*s == '[') {
		name[no_names] = (char *)calloc(n + 1, sizeof(char));
		if (name[no_names] == 0)
		    err("Unable to allocate memory");
		strncpy(name[no_names],x,n);
                name[no_names][n] = '\0';
		arrayid[no_names] = ARRAY;
		dims[no_names] = 1;
		for (n = 1, ++s; *s; ++s) { /* find matching ']' */
		    if (*s == '[')
			++n;
		    if (*s == ']')
			--n;
		    if (n == 1 && *s == ',')
			++dims[no_names];
		    if (n == 0)
			break;
                }
		++no_names;
		++s;
	        while (*s == ' ' || *s == '\t') ++s; /* skip white space */
	    } else if (*s == '(') {
		for (n = 1, ++s; *s; ++s) { /* find matching ')' */
		    if (*s == '(')
			++n;
		    if (*s == ')')
			--n;
		    if (n == 0)
			break;
                }
		++s;
	        while (*s == ' ' || *s == '\t') ++s; /* skip white space */
	    } else {
		name[no_names] = (char *)calloc(n + 1, sizeof(char));
		if (name[no_names] == 0)
		    err("Unable to allocate memory");
		strncpy(name[no_names],x,n);
                name[no_names][n] = '\0';
		arrayid[no_names] = SCALAR;
                ++no_names;
	    }
	    if (*s == ',') {
		++s;
		while (*s == ' ' || *s == '\t') ++s; /* skip white space */
		if (*s == '#' || *s == '\0') {
		    if (fgets(line,maxlinesize,text) == NULL)
			err("Unexpected end of file in SPP procedure");
	    	    n = strlen(line);
	    	    /* eliminate any end-of-line char */
	    	    if (line[n - 1] == '\n') {
		        --n;
		        line[n] = '\0';
	    	    }
                    s = line;
                }
	    } else if (*s == '#' || *s == '\0')
		break;
	    else
		err("Syntax error in SPP procedure");
        }
	return no_names;
}


/*
*************************************************************************
Notes on Generating Code

a. SPP types are:
   char, bool, short, int, long, real, double, complex, pointer
   *** int and long have the same number of bits in SPP.
b. SPP parmaters may be input only, output only, or both input and
   output.
c. SPP paramters may be scalar or arrays of any dimension.
d. If values are returned from SPP procedures they must be scalars
   and can be of any type.

1. Only SPP types char and pointer require code to be generated to
   perform data conversions.
2. If an SPP parameter is not input only, or if that SPP parameter 
   is an array, then the C parameter must be entered as a pointer 
   to the basic C type.
3. An SPP pointer type may or may not be converted to a C pointer.
   If it is not, then the corresponding C type is an IRAFPointer,
   which is really a long.  If it is converted, then the basic C
   type is a C pointer.  It follows from these rules, then, that
   if an SPP pointer is an output parameter and it is converted 
   to a C pointer, then the corresponding C parameter is a pointer
   to a pointer.
4. For values returned from SPP procedures, the only type that may,
   optionally, be changed to a different C type are pointers.
*************************************************************************
*/

int generate_code(FILE *hfile, FILE *cfile, ProcInfo *proc,
	int no_saved_decl, char **saved_decl, CODE_GEN_OPT code_gen_opt) {

	int i, j;
	char *s;
	char *x;
	char *tmp;
	char spp_extern_name[8];
	int convert_rtnptr;  /* return value is a pointer needing conversion */
	int convert_rtnchar; /* return value is a char needing conversion */
	int convert_parmptr; /* a parm is a pointer needing conversion */
	int convert_out;     /* do we have any output conversions? */
	int convert_string;  /* do we have any strings to convert? */
	int no_string;	     /* current string number */
	int convert_char;    /* do we have scalar char parms to convert? */

	const char *c_type(SPP_TYPE);

	static char *errstr_before = 
		"\tclear_cvoserr();\n\txerpsh_();";
	static char *errstr_after = 
		"\tif (xerpoi_())\n\t    set_cvoserr();";
	if (proc->extern_spp != 0) {
	    for (i = 0, s = proc->extern_spp; i < 6 && *s != '\0'; ++s) {
		spp_extern_name[i] = *s;
		++i;
	    }
	} else {
	    /* generate the external SPP name using the SPP convention */
	    for (i = 0, s = proc->spp_name; i < 5 && *s != '\0' ; ++s) {
	        if (*s != '_') {
	            spp_extern_name[i] = *s;
		    ++i;
	        }
	    }
	    if (*s)
	        spp_extern_name[i++] = proc->spp_name[strlen(proc->spp_name) - 1];
	}
	spp_extern_name[i++] = '_';
	spp_extern_name[i] = '\0';

	/* external C names are generated with a "c_" prepended to the name */

	/* Determine the cases and set the flags */

	/* do we have any pointer conversions for return value? */
	convert_rtnptr = 0;
	if (proc->rtn_conv != NONE) {
	    if (proc->rtn_type == POINTER)
	    	convert_rtnptr = 1;
	    else
	        err("Conversion specified for non-pointer return value");
	}
	/* do we have a char as a return value? */
	convert_rtnchar = 0;
	if (proc->rtn_type == CHAR)
		convert_rtnchar = 1;

	convert_out = 0;  /* do we have any output conversions? */

	/* do we have any pointer parameters to convert? */
	convert_parmptr = 0;

	/* do we have any strings to convert, either on input or output? */
	convert_string = 0;
	no_string = 1;	    /* initialize the current string number */

	/* do we have any char scalar variables to deal with? */
	convert_char = 0; /* do we have scalar char parameters to convert? */

	/* now check each parameter for these questions */
	for (i = 0; i < proc->no_parms; ++i) {

 	    /* check for non-pointer data type conversions */
	    if (proc->parm_conv[i] != NONE && proc->parm_type[i] != POINTER) {
 		if (SPP_SIZE[proc->parm_conv[i]] != SPP_SIZE[proc->parm_type[i]]
		    && proc->parm_conv[i] != VOID)
		    err("Attempt to override data types of unequal size");
		proc->parm_type[i] = proc->parm_conv[i]; /* change type */
		proc->parm_conv[i] = NONE;
	    }

	    /* check for pointer to convert */
	    if (proc->parm_conv[i] != NONE && proc->parm_type[i] == POINTER) {
		convert_parmptr++;
		if (proc->option[i] != INPUT_ONLY)
		    convert_out++;
	    }

	    /* check for strings to convert */
	    if (proc->parm_arrayid[i] == ARRAY && proc->parm_type[i] == CHAR) {
		convert_string++;
		if (proc->option[i] != INPUT_ONLY)
		    convert_out++;
	    }

	    /* check for scalar chars to convert */
	    if (proc->parm_arrayid[i] != ARRAY && proc->parm_type[i] == CHAR) {
		convert_char++;
		if (proc->option[i] != INPUT_ONLY)
		    convert_out++;
	    }
	
	}
	if (convert_string > 4)
	    err("TOO MANY STRINGS TO CONVERT!");

/* 	---- for debugging ----
	prProcInfo(proc);
	for (i = 0; i < no_saved_decl; ++i)
	    printf("%d:  %s\n", i, saved_decl[i]);
	printf("\n\
	spp_extern_name = %s\n\
	convert_rtnptr = %d\n\
	convert_parmptr = %d\n\
	convert_out = %d\n\
	convert_string = %d\n\
	convert_rtnchar = %d\n\
	convert_char = %d\n",
		spp_extern_name,
		convert_rtnptr,
		convert_parmptr,
		convert_out,
		convert_string,
		convert_rtnchar,
		convert_char);
	printf("---------------------------------\n");
*/

	/* OK, the flags are all set */

	/* generate the hfile declaration */
	/* which, of course, must be compatible with the cfile version */
	if (convert_rtnptr)
	    fprintf(hfile, "%s *", c_type(proc->rtn_conv));
	else
	    fprintf(hfile, "%s", c_type(proc->rtn_type));
	fprintf(hfile, " c_%s(", proc->c_name);
	for (i = 0; i < proc->no_parms; ++i) {
	    /* algorithm for determining c data type:
	    ** 1. if spp_type is pointer and conversion is specified,
	    **        basic_c_type = converted_type *
	    **    else
	    **        basic_c_type = c version of spp_type
	    ** 2. if spp parm is !input_only or spp parm is array,
	    **        basic_c_type = basic_c_type *
	    ** 3. if spp parm is array and no_dims > 1,
	    **        for (j = 1; j < no_dims; ++j)
	    **            basic_c_type = basic_c_type *
	    */
	    if (proc->parm_conv[i] != NONE)
		fprintf(hfile, "%s *", c_type(proc->parm_conv[i]));
	    else
	        fprintf(hfile, "%s ", c_type(proc->parm_type[i]));
	    if (proc->parm_arrayid[i] == ARRAY || 
		proc->option[i] != INPUT_ONLY)
		    fprintf(hfile, "*");
	    if (proc->parm_arrayid[i] == ARRAY && proc->no_dims[i] > 1 && proc->parm_type[i] == CHAR) {
		for (j = 1; j < proc->no_dims[i]; ++j)
		    fprintf(hfile, "*");
	    }
	    fprintf(hfile, "%s", proc->cparm[i]);
	    if (i < (proc->no_parms - 1))
		fprintf(hfile, ", ");
	}
	fprintf(hfile, ");\n");
	for (i = 0; i < no_saved_decl; ++i) {
	    fprintf(hfile, "/* %s */\n", saved_decl[i]);
	}
 	fprintf(hfile, "\n");
	/* OK, we're finished generating code for the hfile */

	/* Now, we deal the the cfile */

	/* generate the ifdef to allow for no underscores */
	fprintf(cfile,"# if defined(NO_UNDERSCORE)\n# define %s ",
		spp_extern_name);
	for (i = 0; i < (strlen(spp_extern_name) - 1); ++i)
		fprintf(cfile,"%c",spp_extern_name[i]);
	fprintf(cfile,"\n# endif\n");

	/* generate the definition of the external SPP function name */
	/* if code is for C++, we must provide a linkage directive */
	fprintf(cfile, "\textern "); 
	if (code_gen_opt == C_PLUS_PLUS)
	    fprintf(cfile, "\"C\" ");
	if (proc->rtn_type == CHAR)
	    fprintf(cfile, "short");
	else
	    fprintf(cfile, "%s", c_type(proc->rtn_type));
	fprintf(cfile, " %s(", spp_extern_name);
	for (i = 0; i < proc->no_parms; ++i) {
	    if (proc->parm_type[i] == CHAR)
		fprintf(cfile, "short");
	    else
	        fprintf(cfile, "%s", c_type(proc->parm_type[i]));
	    fprintf(cfile, " *");
	    if (proc->parm_arrayid[i] == ARRAY && proc->no_dims[i] > 1 && proc->parm_type[i] == CHAR) {
		for (j = 1; j < proc->no_dims[i]; ++j)
		    fprintf(cfile, "*");
	    }
	    if (i < (proc->no_parms - 1))
		fprintf(cfile, ", ");
	}
	fprintf(cfile, ");\n");

	/* generate the opening function statement */
	if (convert_rtnptr)
	    fprintf(cfile, "%s *", c_type(proc->rtn_conv));
	else
	    fprintf(cfile, "%s", c_type(proc->rtn_type));
	fprintf(cfile, " c_%s(", proc->c_name);
	for (i = 0; i < proc->no_parms; ++i) {
	    if (proc->parm_conv[i] != NONE)
		fprintf(cfile, "%s *", c_type(proc->parm_conv[i]));
	    else
	        fprintf(cfile, "%s ", c_type(proc->parm_type[i]));
	    if (proc->parm_arrayid[i] == ARRAY || 
		proc->option[i] != INPUT_ONLY)
		    fprintf(cfile, "*");
	    if (proc->parm_arrayid[i] == ARRAY && proc->no_dims[i] > 1 && proc->parm_type[i] == CHAR) {
		for (j = 1; j < proc->no_dims[i]; ++j)
		    fprintf(cfile, "*");
	    }
	    fprintf(cfile, "%s", proc->cparm[i]);
	    if (i < (proc->no_parms - 1))
		fprintf(cfile, ", ");
	}
	fprintf(cfile, ") {\n");

	/* The remainder generates the function body. */

	/* If something is returned, generate a declaration for it */
	if (convert_rtnchar)
	    fprintf(cfile, "\tchar rtn;\n");
	else if (proc->rtn_type != VOID) {
	    fprintf(cfile, "\t");
	    if (convert_rtnptr)
		fprintf(cfile, "IRAFPointer");
	    else
		fprintf(cfile, "%s", c_type(proc->rtn_type));
	    fprintf(cfile, " rtn;\n");
	}

	/* Get the easy case out of the way - no parms to convert */
	if (!convert_char && !convert_string && !convert_parmptr) {	
	    fprintf(cfile,"%s\n",errstr_before);
	    fprintf(cfile, "\t");
	    if (proc->rtn_type != VOID)
		fprintf(cfile, "rtn = ");
	    if (convert_rtnchar)
		fprintf(cfile, "(char)");
	    fprintf(cfile, "%s(", spp_extern_name);

	    for (i = 0; i < proc->no_parms; ++i) {
	        if (proc->parm_arrayid[i] != ARRAY && 
			proc->option[i] == INPUT_ONLY)
		    fprintf(cfile, "&");
	        fprintf(cfile, "%s", proc->cparm[i]);
	        if (i < (proc->no_parms - 1))
		    fprintf(cfile, ", ");
	    }
	    fprintf(cfile, ");\n");

	    fprintf(cfile,"%s\n",errstr_after);
	    if (proc->rtn_type != VOID)
	        if (convert_rtnptr) {
		    fprintf(cfile, "\treturn (%s *)((%s *)mem_ + rtn - 1);\n",
			c_type(proc->rtn_conv), c_type(proc->rtn_conv));
	        } else
	            fprintf(cfile, "\treturn rtn;\n");

	    /* generate the closing to the SPP function */
	    fprintf(cfile, "}\n\n");

	    return 0;		      
	}

	/* OK, we've got conversions to contend with. */

	/* generate declarations */
	for (i = 0; i < proc->no_parms; ++i)
	    if (proc->parm_arrayid[i] != ARRAY && proc->parm_type[i] == CHAR)
		fprintf(cfile, "\tshort short_%s;\n", proc->cparm[i]);
	if (convert_parmptr) {
	    for (i = 0; i < proc->no_parms; ++i)
		if (proc->parm_conv[i] != NONE)
		    fprintf(cfile, "\tIRAFPointer IRAF_%s;\n", proc->cparm[i]);
	}

	/* generate buffer checking for output strings */
	/* and conversion for scalar chars */
	for (i = 0, no_string = 1; i < proc->no_parms; ++i) {
	    if (proc->parm_arrayid[i] == ARRAY && proc->parm_type[i] == CHAR) {
		if (proc->option[i] != INPUT_ONLY) {
		    if (proc->parm_size[i] != 0) {
			x = strchr(proc->parm_size[i],':');
			if (x != 0) {
			    tmp = calloc((strlen(proc->parm_size[i]) + 10), sizeof(char));
			    *x = '\0'; ++x;
			    strcpy(tmp,"(");
			    strcat(tmp,proc->parm_size[i]);
			    strcat(tmp," + 1) * ");
			    strcat(tmp,x);
		    	    fprintf(cfile, "\tCH2I_chk_buffer(%d,%s);\n", 
			    	no_string, tmp);
			    --x; *x = ':';
			    free(tmp);
			} else
		    	    fprintf(cfile, "\tCH2I_chk_buffer(%d,%s);\n", 
			    	no_string, proc->parm_size[i]);
		    } else {
		    	if (proc->parm_type[i + 1] != INT)
			    err("Output string not followed by INT and no size specified");
		    	fprintf(cfile, "\tCH2I_chk_buffer(%d,%s);\n", 
			    no_string, proc->cparm[i + 1]);
		    }
		}
		++no_string;
	    } else if (proc->parm_arrayid[i] != ARRAY && 
			proc->parm_type[i] == CHAR) {
		if (proc->option[i] != OUTPUT_ONLY) {
		    fprintf(cfile, "\tshort_%s = ", proc->cparm[i]);
		    if (proc->option[i] == BOTH)
			fprintf(cfile, "*");
		    fprintf(cfile, "%s;\n", proc->cparm[i]);
		}
	    }
	}

	/* generate input pointer conversions */
	if (convert_parmptr) {
	    for (i = 0; i < proc->no_parms; ++i)
		if (proc->parm_conv[i] != NONE) {
		    fprintf(cfile, "\tIRAF_%s", proc->cparm[i]);
		    if (proc->option[i] != OUTPUT_ONLY)
			fprintf(cfile, 
			    " = (IRAFPointer)(%s - (%s *)mem_ + 1);\n", 
			    proc->cparm[i], c_type(proc->parm_conv[i]));
		    else
			fprintf(cfile, " = 0;\n");
		}
	}

	/* generate the SPP function call */
	fprintf(cfile,"%s\n",errstr_before);
	fprintf(cfile, "\t");
	if (proc->rtn_type != VOID)
	    fprintf(cfile, "rtn = ");
	if (convert_rtnchar)
	    fprintf(cfile, "(char)");
	fprintf(cfile, "%s(", spp_extern_name);
	for (i = 0, no_string = 1; i < proc->no_parms; ++i) {

	    if ((proc->parm_conv[i] != NONE) ||
	       (proc->parm_arrayid[i] != ARRAY && proc->option[i] == INPUT_ONLY))
		fprintf(cfile, "&");

	    if (proc->parm_arrayid[i] == ARRAY && proc->parm_type[i] == CHAR) {
		if (proc->option[i] == INPUT_ONLY || proc->option[i] == BOTH) {
		    if (proc->parm_size[i] != 0) {
			x = strchr(proc->parm_size[i],':');
			if (x != 0) {
			    *x = ',';
		    	    fprintf(cfile, "twodchar2iraf(%s,%s,%d)", 
				proc->cparm[i], proc->parm_size[i], no_string);
			} else
		            fprintf(cfile, "char2iraf(%s,%d)", 
				proc->cparm[i], no_string);
		    } else
		        fprintf(cfile, "char2iraf(%s,%d)", proc->cparm[i],
			    no_string);
		} else if (proc->option[i] == OUTPUT_ONLY) {
		    if (proc->parm_size[i] != 0) {
			if (strchr(proc->parm_size[i],':') != 0)
			    fprintf(cfile,"(short **)");
		    }
		    fprintf(cfile, "CH2I_buffer[%d]", no_string);
		}
		no_string++;

	    } else if (proc->parm_arrayid[i] != ARRAY && 
				proc->parm_type[i] == CHAR) {
		if (proc->option[i] != INPUT_ONLY)
		    fprintf(cfile, "&");
		fprintf(cfile, "short_%s", proc->cparm[i]);

	    } else if (proc->parm_conv[i] != NONE)
		fprintf(cfile, "IRAF_%s", proc->cparm[i]);

	    else
		fprintf(cfile, "%s", proc->cparm[i]);

	    if (i < (proc->no_parms - 1))
		fprintf(cfile, ", ");
	}
	fprintf(cfile, ");\n");
	fprintf(cfile,"%s\n",errstr_after);

	/* generate the code to update output strings and scalar chars */
	if (convert_out) {
	    for (i = 0, no_string = 1; i < proc->no_parms; ++i) {
	        if (proc->parm_arrayid[i] == ARRAY && 
		    proc->parm_type[i] == CHAR) {
	            if (proc->option[i] == OUTPUT_ONLY || 
			proc->option[i] == BOTH) {
			if (proc->parm_size[i] != 0) {
			    x = strchr(proc->parm_size[i],':');
			    if (x != 0) {
				*x = ',';
	        	        fprintf(cfile, "\tiraf2twodchar(%s,%s,%d);\n", 
			            proc->cparm[i], proc->parm_size[i], no_string);
			    } else
	        	        fprintf(cfile, "\tiraf2char(%s,%s,%d);\n", 
			            proc->cparm[i], proc->parm_size[i], no_string);
			} else 
	        	    fprintf(cfile, "\tiraf2char(%s,%s,%d);\n", 
			        proc->cparm[i], proc->cparm[i + 1], no_string);
		    }
	            no_string++;
	        } else if (proc->parm_arrayid[i] != ARRAY && 
	    	           proc->parm_type[i] == CHAR) {
	            if (proc->option[i] == OUTPUT_ONLY || proc->option[i] == BOTH)
	    	    fprintf(cfile, "\t*%s = short_%s;\n", proc->cparm[i],
	    	        proc->cparm[i]);
	        }
	    }
	}

	/* generate code to update any output pointers */
	if (convert_parmptr) {
	    for (i = 0; i < proc->no_parms; ++i) {
		if (proc->parm_conv[i] != NONE && proc->option[i] != INPUT_ONLY)
		    fprintf(cfile, 
			"\t*%s = (%s *)((%s *)mem_ + IRAF_%s - 1);\n", 
			proc->cparm[i], c_type(proc->parm_conv[i]),
			c_type(proc->parm_conv[i]), proc->cparm[i]);
	    }
	}

	if (proc->rtn_type != VOID)
	    if (convert_rtnptr) {
		fprintf(cfile, "\treturn (%s *)((%s *)&mem_ + rtn - 1);\n",
			c_type(proc->rtn_conv), c_type(proc->rtn_conv));
	    } else
	        fprintf(cfile, "\treturn rtn;\n");

	/* generate the closing to the SPP function */
	fprintf(cfile, "}\n\n");

	return 0;
}
