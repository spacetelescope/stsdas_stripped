/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/


/* 
**  DEF.H    - Include file containing information about definitions
*/

#define YES		1
#define NO		0
#define TRUE		1
#define FALSE		0
#define MAX_STACK_SZ	256
#define MAX_INDENT_SZ	256


#define TY_COM		1	/* Beginning of a comment */
#define TY_LETTER	2	/* alphabetic letter	  */
#define TY_DIGIT	3	/* is a digit             */
#define TY_LP		4	/* left parenthesis       */
#define TY_RP		5	/* right parenthesis      */
#define TY_LB		6	/* left bracket           */
#define TY_RB		7	/* right bracket          */
#define TY_PUNC		8	/* punctuation mark       */
#define TY_MINUS	9	/* - sign - unary????     */
#define TY_NEWLINE     10       /* \n character           */
#define TY_EOST        11       /* ';' char or other EOST */

#define NM_SYS		1	/* name is a program reserved word */
#define NM_PARM		2	/* name is a variable of some sort */
#define NM_FUNC		3	/* name is a function name	   */
#define NM_VEC		4	/* name is defined as a vector     */


#define INDEX_DUMMY	"_dUmMy_InDeX_fOr_ThE_cOmPiLeR_"
