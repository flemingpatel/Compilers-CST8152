/**********************************************************************************************************
* File Name:	table.h
* Compiler:		MS Visual Studio 2013
* Author:		Sv.Ranev Modified by Fleming Patel & Arin Poray
* Course:		CST 8152 - Compilers, Lab Section: 011
* Assignment:	2
* Date:			31/10/2016
* Professor:	Sv.Ranev
* Purpose:		Contains all the defination of scanner functions and  Transition Table definations
**********************************************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
*    '\0' or only one of the folowing constants: 255, 0xFF , EOF
*/
#define SEOF 255

/*  Single-lexeme tokens processed separately one by one
*  in the token-driven part of the scanner
*  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
*       space
*  !<comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', # ,
*  .AND., .OR. , SEOF, 'wrong symbol',
*/

#define NL '\n' /* new Line character */

#define ES  12 /* Error state */
#define IS -1    /* Inavalid state */

/* State transition table definition */


#define TABLE_COLUMNS 7
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */{ 1, 6, 4, 4, IS, IS, IS },
	/* State 1 */{ 1, 1, 1, 1, 2, 3, 2 },
	/* State 2 */{ IS, IS, IS, IS, IS, IS, IS },
	/* State 3 */{ IS, IS, IS, IS, IS, IS, IS },
	/* State 4 */{ ES, 4, 4, 4, 7, 5, 5 },
	/* State 5 */{ IS, IS, IS, IS, IS, IS, IS },
	/* State 6 */{ ES, ES, 9, ES, 7, ES, 5 },
	/* State 7 */{ 8, 7, 7, 7, 8, 8, 8 },
	/* State 8 */{ IS, IS, IS, IS, IS, IS, IS },
	/* State 9 */{ ES, 9, 9, ES, ES, ES, 10 },
	/* State 10 */{ IS, IS, IS, IS, IS, IS },
	/* State 11 */{ IS, IS, IS, IS, IS, IS },
	/* State 12 */{ IS, IS, IS, IS, IS, IS },
};

/* Accepting state table definition */
#define ASWR 1  /* accepting state with retract */
#define ASNR 3  /* accepting state with no retract */
#define NOAS 0  /* not accepting state */

int as_table[] = {
	/* State 0 */NOAS,
	/* State 1 */NOAS,
	/* State 2 */ASWR,
	/* State 3 */ASNR,
	/* State 4 */NOAS,
	/* State 5 */ASWR,
	/* State 6 */NOAS,
	/* State 7 */NOAS,
	/* State 8 */ASWR,
	/* State 9 */NOAS,
	/* State 10 */ASWR,
	/* State 11 */ASNR,
	/* State 12 */ASNR
};

/* Accepting action function declarations */
Token aa_func02(char *lexeme);
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func10(char *lexeme);
//Token aa_func11(char *lexeme);
Token aa_func12(char *lexeme);

/* defining a new type: pointer to function (of one char * argument)
returning Token
*/

typedef Token(*PTR_AAF)(char *lexeme);

/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
* Token (*aa_table[])(char lexeme[]) = {
*/

PTR_AAF aa_table[] = {
	/* State 0 */NULL,
	/* State 1 */NULL,
	/* State 2 */aa_func02,
	/* State 3 */aa_func03,
	/* State 4 */NULL,
	/* State 5 */aa_func05,
	/* State 6 */NULL,
	/* State 7 */NULL,
	/* State 8 */aa_func08,
	/* State 9 */NULL,
	/* State 10 */aa_func10,
	/* State 11 */NULL,
	/* State 12 */aa_func12
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  8

char * kw_table[] = {
	"ELSE",
	"IF",
	"INPUT",
	"OUTPUT",
	"PLATYPUS",
	"REPEAT",
	"THEN",
	"USING"
};

#endif
