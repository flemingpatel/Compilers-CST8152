/**********************************************************************************************************
* File Name:	stable.h
* Compiler:		MS Visual Studio 2013
* Author:		Fleming Patel & Arin Poray
* Course:		CST 8152 - Compilers
* Assignment:	1
* Date:			11/24/2016
* Professor:	Sv.Ranev
* Purpose:		Contains all preprocessor directives, type declarations and
*				prototypes necessary for buffer implementation
**********************************************************************************************************/
#ifndef  STABLE_H_
#define  STABLE_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <string.h>

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#define EOS '\0'
#define DEFAULT_VID_LEN 9 /* Default variable identifier length */
#define DEFAULT_BUFFER_MODE 'a' /* Default buffer mode to additive */
#define DEFAULT_INC_FACTOR 15 /* Default buffer increment factor */

#define DEFAULT_INT_SI 'I' /* Default Integer String identifier */
#define DEFAULT_FLOAT_SI 'F' /* Default Float String identifier */
#define DEFAULT_STRING_I 'S' /* Default String identifier */

#define DEFAULT  0xFFF8		 /* 1111 1111 1111 1000 */
#define DEFAULTZ 0x0000		 /* 0000 0000 0000 0000 */
#define SET_IL   0xFFFC		 /* 1111 1111 1111 1100 */
#define SET_FPL  0xFFFA		 /* 1111 1111 1111 1010 */
#define SET_LSB	 0x0001      /* 0000 0000 0000 0001 */
#define SET_SL   0xFFFF		 /* 1111 1111 1111 1110 */
#define CHK_LSB	 0x0001      /* 0000 0000 0000 0001 */
#define CHK_IL	 0x0004      /* 0000 0000 0000 0100 */
#define CHK_FPL	 0x0002      /* 0000 0000 0000 0010 */
#define CHK_SL	 0x0007      /* 0010 0000 0000 0111 */

#define SR_FAIL1 -1 /* Symbol Table fail return value */
#define SR_FAIL2 -2 /* Symbol Table fail return value */

typedef union InitialValue{
	int int_val;    /* integer variable initial value */
	float fpl_val;	/* floating-point variable initial value */
	int str_offset;	/* string variable initial value */
}InitialValue;

typedef struct SymbolTableVidRecord {
	unsigned short status_field; /* variable record status field*/
	char * plex;				 /* pointer to lexeme (VID name) in CA */
	int o_line;					 /* line of first occurrence */
	InitialValue i_value;		 /* variable initial value */
	size_t reserved;			 /*reserved for future use*/
}STVR;

typedef struct SymbolTableDescriptor {
	STVR *pstvr;   /* pointer to array of STVR */
	int st_size;   /* size in number of STVR elements */
	int st_offset; /*offset in number of STVR elements */
	Buffer *plsBD; /* pointer to the lexeme storage buffer descriptor */
} STD;

extern STD sym_table; /* Symbol Table */

/* function declarations */
STD st_create(int st_size);
int st_install(STD sym_table, char *lexeme, char type, int line);
int st_lookup(STD sym_table, char *lexeme);
int st_update_type(STD sym_table, int vid_offset, char v_type);
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value);
char st_get_type(STD sym_table, int vid_offset);
void st_destroy(STD sym_table);
int st_print(STD sym_table);
static void st_setsize(void);
static void st_incoffset(void);
int st_store(STD sym_table);
int st_sort(STD sym_table, char s_order);

#endif
