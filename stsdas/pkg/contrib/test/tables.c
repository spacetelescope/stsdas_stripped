/*	GAUSS - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 
1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved. 
*/

#define  import_spp
#define  import_libc
#define  import_stdio
#include <iraf.h>

#include "defines.h"
#include "tables.h"
/*#include "com.h" 
#include "unix.h"  */
#include "strings.h"
#include "par2.h"

#define STRLEN 10                     /* length of a string */
#define LSTRLEN 64                    /* length of a string */
#define NCOL 50
  
static SPPCOL dat, prm[10];


static int sppdtb = 0;
static int sppptb[10]= {0,0,0,0,0,0,0,0,0,0};
            
midasopen(type,name,trows)
char *name;   
int type;                
int trows;
{
     	short tablename[64];
        int  iomode;               
        int rowpar = 21;  
	int colpar = 22;      
	static int pcnt = 0;

        iomode =  2;   /*  READ_WRITE; */
        strupk(name,tablename,&sxf);
        if (type == PARAM_TABLE)
           { 	
		zero_out(type,pcnt);
		sppptb[pcnt] = tbtopn(tablename,&iomode,&zero);
             	trows = prm[pcnt].nr = tbpsta(sppptb,&rowpar);  
		prm[pcnt].nc = tbpsta(sppptb,&colpar);
		load_colmnptr(type,pcnt);
		pcnt++;         
           }
        else
           { 
		zero_out(type,0);
		sppdtb = tbtopn(tablename,&iomode,&zero);
             	dat.nc = tbpsta(&sppdtb,&colpar);
		trows = dat.nr = tbpsta(&sppdtb,&rowpar);           
       		load_colmnptr(type,pcnt);
	   }
        return trows;
}
       
midasclose(type)            
int type;                     
{  
   int i, limit;          
   if (type == PARAM_TABLE)
      { 
	limit = getparamfilenum();
	for (i=0; i < limit; i++)
		tbtclo(&sppptb[i]); 
      }
   else
      { tbtclo(&sppdtb);}          
   return 0;
}

close_all_tables()
{
	if (sppdtb != 0)
		midasclose(DATA_TABLE);
	if (sppptb[0] != 0)
		midasclose(PARAM_TABLE);
	
}        
double getmidasval(type,name,row,fnum)
char *name;
int row;                        
int type;                              
int fnum;
{                         
    int sppptr;
    int colptr;      
    int nullflag;
    double dblenum;
    
	dblenum = 0.0;                           
        if (type == PARAM_TABLE)
            {  sppptr = sppptb[fnum];  }
        else
            {  sppptr = sppdtb;}
        colptr = colpointer(type,name,fnum,GET);                   
        nullflag = 0;
        if (colptr != 0)
		tbrgtd(&sppptr,&colptr,&dblenum,&nullflag,&one,&row); 
        return dblenum;
}                   

double getmidasrcval(type,colptr,row,fnum)
int colptr;
int row;          
int type;                              
int fnum;
{
    int sppptr;
    int nullflag;                                           
    double dblenum;
                  
    
    dblenum = 0.0;                           
    if (type == PARAM_TABLE)
            {  sppptr = sppptb[fnum];  }
    else
            {  sppptr = sppdtb;}
    nullflag = 0;
    tbrgtd(&sppptr,&colptr,&dblenum,&nullflag,&one,&row); 
    return dblenum;
}

putmidasval(type,name,row,value,fnum)
char *name;
int row;
double value;    
int type;                             
int fnum;
{                      
    double dblenum;
    int colptr;
    int sppptr;
    
        dblenum = value;
        
        if (type == PARAM_TABLE)
            {  sppptr = sppptb[fnum]; }
        else
            {  sppptr = sppdtb;}
        colptr =colpointer(type,name,fnum,PUT);
        tbrptd(&sppptr,&colptr,&dblenum,&one,&row);
}          

load_colmnptr(type,fnum)
int type;                  
int fnum;
{
    int i;
    short sppcolnm[64];
    int sppptr;       
    int fo = 41;
    int nt = 19;
    int anum;                        
    char fname[64], *pchar;
    SPPCOL hold;

    if (type == PARAM_TABLE) {  
		sppptr = sppptb[fnum];
		hold = prm[fnum];
	    }
        else {  
		sppptr = sppdtb;
		hold = dat;
	    } 
    for (i=0; i<hold.nc; i++)
        {                               
	 anum = i+1;
         hold.sppcolptr[i] = tbcnum(&sppptr,&anum);
         tbcigt(&hold.sppcolptr[i],&fo,sppcolnm,&nt);
	 strpak(sppcolnm,fname,&nt);
       /* printf("%d sppcolptr = %d  sppcolnm  = %s \n\n",i,
		hold.sppcolptr[i],fname);   */
	 pchar = fname;
	 strcpy (hold.sppcolname[i],pchar);
        }
   if (type == PARAM_TABLE)
   	prm[fnum] = hold;
    else
	dat = hold;
    return 1;
}
	
colpointer(type,name,fnum,action)
char*name; 
int type;                  
int fnum;                       
int action;
{
    int colptr;
    int i;
    short sppcol[64];
    short sppblank[64];
    char blank[64];
    int sppptr;       
    int lendata;
    int datatype;       
    SPPCOL hold;

    if (type == PARAM_TABLE)
            {  
		sppptr = sppptb[fnum];
		hold = prm[fnum];
	    }
        else
            {  
		sppptr = sppdtb;
		hold = dat;
	    } 
    for (i=0; i<hold.nc; i++)
      {
         if (strcmp(name,hold.sppcolname[i]) == 0)
              {
                 colptr = hold.sppcolptr[i];
                 return colptr;
              }
      }
    strupk(name,sppcol,&sxf);
    tbcfnd(&sppptr,sppcol,&colptr,&one); 
    if (colptr == NULL)
    {
	if  (action == GET)
       		return 0;	
    	else if  (action == PUT)
  	  {     
        	 strcpy(blank,"");
        	 strupk(blank,sppblank,&sxf);
         	 datatype = 7;
       	  	 lendata = 4;
        	 tbcdef(&sppptr,&colptr,sppcol,sppblank,sppblank,&datatype,
                	 &lendata,&one);
		 zero_fill(sppptr,colptr,hold.nr);
        }
    }
    strcpy (hold.sppcolname[hold.nc],name);
    hold.sppcolptr[hold.nc] = colptr;
    hold.nc++;
    if (type == PARAM_TABLE)
   	prm[fnum] = hold;
    else
	dat = hold;
    return colptr;
     
}             

zero_fill(sppptr,colptr,nrows)
int sppptr;
int colptr;
int nrows;
{
	int dblenum;
	int i;
	int row;

	dblenum = 0.0;
	for (i = 1;i <= nrows; i++)
	{  
		row = i;
	        tbrptd(&sppptr,&colptr,&dblenum,&one,&row);
	}
}

zero_out(type,fnum)
int type;
int fnum;
{                          
   int i;  
   SPPCOL load;

   for (i=0; i<NCOL; i++)
    {
      load.sppcolptr[i] = 0;
      strcpy(load.sppcolname[i]," ");
    }                       
    load.nc = 0;            
    load.nr = 0;            
    if (type == PARAM_TABLE)
   	prm[fnum] = load;
    else
	dat = load;

}
                 

          
char *wordalloc(word1)
char *word1;
{
       	char string[LSTRLEN];
	char *p;
	int num;
	long strsize;
	strsize = (long)(sizeof(string));
	p=(char*)MemAlloc("word",strsize);  /* allocate a space to store a word */
	if ((num=strcmp(word1,"#N/A")) == 0) /*	if it is undefined */
		strcpy(p,"UND"); /* then fill with UND */
	else
		strcpy(p,word1);
	return p;
}

getnumcols(type,fnum)
int type;
int fnum;
{         
	int cols= 0;

        if (type == PARAM_TABLE)
           {  	/*printf("prmnumcols = %d\n\n", prm[fnum].nc);*/
		return prm[fnum].nc;  } 
        else
            { 	/*printf("datnumcols = %d\n\n", dat.nc);*/
		return dat.nc;  } 
     }

char *getcolname(col,type,fnum)
int col;
int type;
int fnum;
{         

        if (type == PARAM_TABLE)
           { /*printf("pcolname = %s\n\n",prm[fnum].sppcolname[col]);*/
	     	return prm[fnum].sppcolname[col];}   
        else
            { /*printf("dcolname = %s\n\n",dat.sppcolname[col]);*/
	     return dat.sppcolname[col];}   
}
