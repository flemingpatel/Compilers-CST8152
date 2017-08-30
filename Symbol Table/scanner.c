/**********************************************************************************************************
* File Name:	scanner.c
* Compiler:		MS Visual Studio 2013
* Author:		Fleming Patel & Arin Poray
* Course:		CST 8152 - Compilers, Lab Section: 011
* Assignment:	2
* Date:			31/10/2016
* Professor:	Sv.Ranev
* Purpose:		Contains definition of scanner utility functions to perform various scanner functions.
* Function list: b_create(), b_addc(), b_reset(),
*				 b_free(), b_getc(), b_retract(), b_retract_to_mark(), b_getcoffset(), b_cbhead()
*				 aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func10(), aa_func12(),
*				 atool(), isalpha(), isdigit(), char_class()
**********************************************************************************************************/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */
/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
int flag;
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
//static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

/**********************************************************************************************************
* Purpose:			(1) Sets the addc_offset for the input buffer (sc_buf) to 0
*					(2) Resets the string literal buffer (str_LTBL), and
*					(3) initializes the line number to 1
* Author:			Svillen Ranev
* History/Versions: 1.0
* Called Function:	b_empyt(), b_setmark(), b_retract_to_mark(), b_reset()
* Parameters:		sc_buf		 Buffer*	    Points to a valid buffer structure in the heap
* Return Value:		On Success
*						EXIT_SUCCESS
*					On Failure
*						EXIT_FAILURE
**********************************************************************************************************/
int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
	/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/**********************************************************************************************************
* Purpose:			Performs token recognition by reading lexeme from the input stream (sc_buf)
* Author:			Fleming Patel, Arin Kumar Poray
* History/Versions:	1.0
* Called Function:	b_getc(), b_retract(), b_getcoffset(), b_setmark(), b_retract_to_mark(), b_create(),
*					b_addc(), b_cbhead(), b_free()
* Parameters:		sc_buf		 Buffer*	    Points to a valid buffer structure in the heap
* Return Value:		t			 Token			Token Structure containing a token code and the token
*												attribute.
* Algorithm:		Performs token recognition by
*					(1) reads one character at a time from the input buffer (sc_buf)
*					(2) if the character read is equal to SEOF, breaks out of the while loop
*					(3) else loops through, processing tokens one by one (for special cases or exceptions)
*					(4) or using transition table (for SVID, AVID, DIL, FIL, OIL, Comment)
*					(5) returns a token structure if a token pattern (as defined in the lexical grammar)
*						matches with a lexeme found in the stream of input symbols.
*					(6) else returns a Error Token if a error occurs
**********************************************************************************************************/
Token mlwpar_next_token(Buffer * sc_buf)
{
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart; /*start offset of a lexeme in the input buffer */
	short lexend; /*end   offset of a lexeme in the input buffer */
	int accept = NOAS; /* type of state - initially not accepting */
	int i;

	/*
	lexstart is the offset from the beginning of the char buffer of the
	input buffer (sc_buf) to the first character of the current lexeme,
	which is being processed by the scanner.
	lexend is the offset from the beginning of the char buffer of the
	input buffer (sc_buf) to the last character of the current lexeme,
	which is being processed by the scanner.

	*/
	int err_t_ch_index = 0; /* index of character in the error token attribute, err_lex[] array */
	while (1){ /* endless loop broken by token returns it will generate a warning */

		c = b_getc(sc_buf);
		/* special cases or token driven processing */

		/* Checking for Source-End-Of-File token */
		if (c == (unsigned char)SEOF || c == EOS){
			t.code = SEOF_T;
			break;
		}
		/* Checking for white space, Horizontal Tab, Vertical Tab, Form feed */
		if (c == ' ' || c == '\t' || c == '\v' || c == '\f'){
			continue;
		}
		/* Checking for New Line character token */
		if (c == NL){
			line++;
			continue;
		}
		/* Checking for '=' and '==' token */
		if (c == '='){
			c = b_getc(sc_buf);
			if (c == '='){/* Checks TRUE if the next character in buffer stream is '='*/

				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			b_retract(sc_buf); /* If above condition fails then retract to the previous character */
			t.code = ASS_OP_T;
			return t;
		}
		/* Checking for left parenthesis token */
		if (c == '('){
			t.code = LPR_T;
			return t;
		}
		/* Checking for right parenthesis token */
		if (c == ')'){
			t.code = RPR_T;
			return t;
		}
		/* Checking for left brace token */
		if (c == '{'){
			t.code = LBR_T;
			return t;
		}
		/* Checking for right brace token */
		if (c == '}'){
			t.code = RBR_T;
			return t;
		}
		/* Checking for Less Than '<' operator or Not Equal To '<>' operator token */
		if (c == '<'){
			c = b_getc(sc_buf);
			if (c == '>'){ /* Checks TRUEif the next character in buffer stream is '>' */
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			}
			b_retract(sc_buf);/* If above condition fails then retract to the previous character */
			t.code = REL_OP_T;
			t.attribute.rel_op = LT;
			return t;
		}
		/* Checking for Greater Than ">" operator token */
		if (c == '>'){
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		}
		/* Checking for Comma operator token */
		if (c == ','){
			t.code = COM_T;
			return t;
		}
		/* Checking for End-Of-Statement token */
		if (c == ';'){
			t.code = EOS_T;
			return t;
		}
		/* Checking for String Concatenation */
		if (c == '#'){
			t.code = SCC_OP_T;
			return t;
		}

		/*********************************************************
		*	Chceking for ArithMetic Operator
		**********************************************************/

		/* Cheking for Addition '+' operator token */
		if (c == '+'){
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		}
		/* Cheking for Subtraction '-' operator token */
		if (c == '-'){
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		}
		/* Cheking for Multication '*' operator token */
		if (c == '*'){
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		}
		/* Cheking for Division '/' operator token */
		if (c == '/'){
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		}

		/*********************************************************
		*	Checking for logical .AND. and Logical .OR. operators
		**********************************************************/
		if (c == '.'){
			err_t_ch_index = 0; /* initializing the index of the error token attribute, err_lex[] array to 0 */
			int i = 0; /* Loop counter */
			int num_retract = 0; /* Counter to store the number of characters are ahead in buffer from '.' */
			t.attribute.err_lex[err_t_ch_index++] = c; /* Assigning '.' in err_lex[] array at index 0 */
			c = b_getc(sc_buf); /* Getting next character */
			num_retract++;
			/* If next character is 'A' then buffer increase one character,
			* and checks the next letter. If Letter 'N' is encountered
			* then buffer increase one character again and checks the next letter
			* If letter 'D' is encountered then buffer increase one character
			* again and checks the next letter . If DOT'.' is encountered
			* then sets log_op to AND
			*/
			if (c == 'A'){
				c = b_getc(sc_buf);
				num_retract++;
				if (c == 'N'){
					c = b_getc(sc_buf);
					num_retract++;
					if (c == 'D'){
						c = b_getc(sc_buf);
						num_retract++;
					}
					if (c == '.'){
						t.code = LOG_OP_T;
						t.attribute.log_op = AND;
						return t;
					}
				}
			}
			/* else if next character is 'O' then buffer increases one character,
			* and checks the next letter. If Letter 'R' is encountered
			* then buffer increase one character again and checks
			* the next letter. If DOT '.' is encountered
			* then sets log_op to OR
			*/
			else if (c == 'O'){
				c = b_getc(sc_buf);
				num_retract++;
				if (c == 'R'){
					c = b_getc(sc_buf);
					num_retract++;
					if (c == '.'){
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
					}
				}
			}
			/* Since every time buffer increses by one more character to get next character hence
			*  if condition fails, then the buffer must retract back to the character after '.'
			*/
			for (i = 0; i < num_retract; i++){
				b_retract(sc_buf);
			}
			t.code = ERR_T; /* Error Token */
			t.attribute.err_lex[err_t_ch_index] = EOS;
			return t;

		}

		/*********************************************************
		*	Chceking for the " !< " mens comment operator
		**********************************************************/
		if (c == '!'){
			c = b_getc(sc_buf);
			if (c == '<'){ /* Checks TRUE if the next character is '<' */
				while (c != NL){
					/* If condition passes all characters will be skipped in that line  */
					c = b_getc(sc_buf);
					if (c == (unsigned char)SEOF){
						t.code = SEOF_T;
						return t;
					}
				}
				line++;
				continue;
			}
			/* Else returns an error token attribute, err_lex[] with
			*  character ! in index
			*  character following '!' in index 1
			*  '\0' in index 2 to make c type string.
			*/
			else{
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = EOS;
				while (c != NL){
					c = b_getc(sc_buf); /* Consuming all characters till the end of that line  */
					if (c == (unsigned char)SEOF){
						t.code = SEOF_T;
						return t;
					}
				}
				line++;
				return t;
			}
		}

		/*******************************************************
		check for string literal
		*********************************************************/
		if (c == '"'){
			int i = 0;
			lexstart = b_getcoffset(sc_buf);
			b_setmark(sc_buf, lexstart);
			/* Checking for strings which must start with '"' and end with '"'.
			* In this algorithm, loops through until last '"' mark to define
			* String literal token. In SEOF is encountered
			* then it must return an error token along with INVALID string
			* If a new line is encountered then it must increase the line number.
			* If '"' is encountered it must return string literal token
			* with the specific string.
			*/
			do{
				c = b_getc(sc_buf);
				/* If SEOF is encountered then
				* returns a error token. According to the specification for return
				* string, if string size is greater than 20 then it returns
				* the first 17 characters along with three '.' (Dot).At the end, '\0'
				* is stored to make c type string.
				*/
				if (c == (unsigned char)SEOF){
					lexend = b_getcoffset(sc_buf);
					b_retract_to_mark(sc_buf);
					lexstart = b_retract(sc_buf);
					t.code = ERR_T;
					if ((lexend - lexstart) > 20){ /* Checking for string length */
						for (i = 0; i < 17; i++){
							c = b_getc(sc_buf);
							t.attribute.err_lex[i] = c;
						}
						t.attribute.err_lex[i++] = '.';
						t.attribute.err_lex[i++] = '.';
						t.attribute.err_lex[i++] = '.';
						t.attribute.err_lex[i] = EOS;
					}
					else{
						int j = 0;
						for (i = 0; i < 20; i++){
							c = b_getc(sc_buf);
							if (c == NL)
								continue;
							t.attribute.err_lex[j++] = c;
						}
						t.attribute.err_lex[j] = EOS;
					}
					if (c == (unsigned char)SEOF){
						b_retract(sc_buf);
					}
					else{
						while (c != (unsigned char)SEOF){
							c = b_getc(sc_buf);
						}
						b_retract(sc_buf);
					}
					return t;
				}

				/* If a New Line Character is encountered then increases the line number*/
				else if (c == NL){
					line++;
				}

				/* If '"' character is encountered then it must return the
				* String Literal Token. According to specification for return string,
				* must store the characters between starting " and ending " in str_LTBL
				*/
				else if (c == '"'){

					b_retract(sc_buf);
					lexend = b_getcoffset(sc_buf);
					b_retract_to_mark(sc_buf);
					t.code = STR_T;
					t.attribute.str_offset = b_size(str_LTBL);
					for (i = lexstart; i < lexend; i++){
						b_addc(str_LTBL, b_getc(sc_buf));
					}
					b_addc(str_LTBL, EOS);
					b_getc(sc_buf);
					return t;
				}
			} while (c != '"');
		}

		/*********************************************************
		*	Process state transition table
		**********************************************************/

		if (isalpha(c) || isdigit(c)){

			b_retract(sc_buf);
			lexstart = b_getcoffset(sc_buf); /* Setting lexstart to the index in input buffer containing the
											 first character  */
			b_setmark(sc_buf, lexstart); /* Setting mark_offset of the input buffer to lexstart */
			c = b_getc(sc_buf);

			while (accept == NOAS){ /* loops till state remains in Non Accepting State */
				state = get_next_state(state, c, &accept);
				if (accept != NOAS)
					break;
				c = b_getc(sc_buf);
			}

			if (accept == ASWR){
				b_retract(sc_buf); /* retracts by one index */
			}

			lexstart = b_mark(sc_buf);
			lexend = b_getcoffset(sc_buf); /* Setting lexend to the index in input buffer containing the
										   last character  */

			lex_buf = b_create((lexend - lexstart) + 1, 0, 'f');
			if (lex_buf == NULL){ /* If lex_buf creation fails, returns a error token */
				scerrnum = 1;
				aa_func12("RUN TIME ERROR");
			}
			b_retract_to_mark(sc_buf); /* retracts to the first character in the lexeme */
			for (i = lexstart; i < lexend; i++){
				b_addc(lex_buf, b_getc(sc_buf)); /* copies characters from input buffer to lex_buf */
			}
			b_addc(lex_buf, EOS); /* Adds SEOF at the end of the current lexeme to make C-type String */
			t = aa_table[state](b_cbhead(lex_buf)); /* Calls the corresponding accepting function */
			b_free(lex_buf); /* freeing the memory of lex_buf */
			return t; /* returning the lexeme token */
		}
		/* If the above condition fails, returns error token */
		t.code = ERR_T;
		t.attribute.err_lex[0] = c;
		t.attribute.err_lex[1] = EOS;
		return t;
	}
	return t;
}


/**********************************************************************************************************
* Purpose:			Computes the next state depending on the current state and input character from the
*					input buffer
* Author:			Svillen Ranev
* History/Versions:	-
* Called Function:	char_class()
* Parameters:		state	int		the current state
*					c		char	input character from input buffer (sc_buf)
*					accept	int*	pointer to a int holding value corresponding to NOAS, ASNR, ASWR, ES or IS
* Return Value:		next	int		The next state
* Algorithm:		None
**********************************************************************************************************/
int get_next_state(int state, char c, int *accept){
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:

	Assertion failed: test, file filename, line linenum

	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
#ifdef DEBUG
	if (next == IS){
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	//printf("next : %d\n", next);
	*accept = as_table[next];
	return next;
}

/**********************************************************************************************************
* Purpose:			Computes the class (i.e., the column index in transition table) for the input character
* Author:			Fleming Patel
* History/Versions:	1.0
* Called Function:	isaplpha(), isdigit()
* Parameters:		c		char	the input character
* Return Value:		val		int		the column index in transition table to which the input character
*									belongs to
* Algorithm:		None
**********************************************************************************************************/
int char_class(char c){

	int val = 6;
	/* Column 1 [a-zA-Z] */
	if (isalpha(c)){
		val = 0;
	}
	/* Column 2 '0' */
	else if (c == '0'){
		val = 1;
	}
	/* Column 3 [1 - 9] / Column 2 [8,9] */
	else if (isdigit(c)){ /* Column 3 [1 - 9] */
		if (c == '8' || c == '9'){
			val = 3;
		}
		else{ /* Column 2 [8,9] */
			val = 2;
		}
	}
	/* Column 5 '.' */
	else if (c == '.'){
		val = 4;
	}
	/* Column 6 '%' */
	else if (c == '%'){
		val = 5;
	}
	/* Column 6 other symboles */
	return val;
}
/**********************************************************************************************************
* Purpose:			Checks for KEYWORD and AVID
* Author:			Arin Kumar Poray
* History/Versions:	1.0
* Called Function:  strcmp(), strlen()
* Parameters:		lexeme	char[]	character array containing a lexeme
* Return Value:		Keyword Token or AVID Token
*
* Algorithm:		(1) Checks the input (lexeme) for Keyword by comparing it with keyword table string
*					(2) if the input lexeme matches with a keyword in keyword table, returns a keyword
*					token
*					(3) else it checks the First letter of lexeme if it meets the requirnment of be an
*						Integer or Float it will directly added to in symbol table. Else it exit with
*						exit failure.
**********************************************************************************************************/
Token aa_func02(char lexeme[]){

	Token t;
	unsigned int i = 0;
	char first_letter_lexeme;

	for (i = 0; i < KWT_SIZE; i++){
		/* Checking for Keyword by comparing lexeme with keyword table string */
		if (strcmp(lexeme, kw_table[i]) == 0){
			t.code = KW_T;
			t.attribute.kwt_idx = i;
			return t;
		}
	}
	
	/* Adding lexeme to Symbol tabel */
	first_letter_lexeme = lexeme[0];

	/* Checking first letter of lexeme to meet the requirnment of be a an Integer */
	if (first_letter_lexeme == 'i' || first_letter_lexeme == 'o' || first_letter_lexeme == 'd' || first_letter_lexeme == 'w'){
		t.attribute.vid_offset = st_install(sym_table,lexeme,DEFAULT_INT_SI,line); /* If it meets than it will be added in symbol table */
	}
	else{ /* Else it would be a float */
		t.attribute.vid_offset = st_install(sym_table, lexeme, DEFAULT_FLOAT_SI, line); /* If it meets than it will be added in symbol table */
	}
	/* Checking for symbol table is empty */
	if (t.attribute.vid_offset == SR_FAIL2){
		exit(SR_FAIL1);
	}
	/* Checking for symbole table is full or not */
	if (t.attribute.vid_offset == SR_FAIL1){
		printf("\nError: The Symbol Table is full - install failed.\n");
		st_store(sym_table);
		exit(SR_FAIL1);
	}

	t.code = AVID_T; /* Assigning to AVID_T */
	return t;
}

/**********************************************************************************************************
* Purpose:			Checks for SVID
* Author:			Fleming Patel
* History/Versions:	1.0
* Called Function:  strlen()
* Parameters:		lexeme	char[]	character array containing a lexeme
* Return Value:		SVID token
*
* Algorithm:		(1) Token code is set to SVID_T.
*					(2) It will be store SVID to directly in the Symbol Table
**********************************************************************************************************/
Token aa_func03(char lexeme[]){

	Token t;
	/* Adding the String in the Symbol Table */
	t.attribute.vid_offset = st_install(sym_table, lexeme, DEFAULT_STRING_I, line); 

	/* Checking for symbol table is empty */
	if (t.attribute.vid_offset == SR_FAIL2){
		exit(SR_FAIL1);
	}

	/* Checking for symbole table is full or not */
	if (t.attribute.vid_offset == SR_FAIL1){
		printf("\nError: The Symbol Table is full - install failed.\n");
		st_store(sym_table);
		exit(SR_FAIL1);
	}
	t.code = SVID_T; /* Assigning to string variable identifier token */
	return t;
}

/**********************************************************************************************************
* Purpose:			Checks for FIL
* Author:			Fleming Patel
* History/Versions:	1.0
* Called Function:	strtod(), aa_func12()
* Parameters:		lexeme	char[]	character array containing a lexeme
* Return Value:		FIL token
* Algorithm:		(1) Converts the input (lexeme) to double value.
*					(2) If the value of the double not in the range as the value of 4-byte float in c,
*						then the function returns an error token.
*					(3) else the token code is set to FPL_T and token attribute, flt_value is set to the
*						float value.
*					(4) returns the FIL token
**********************************************************************************************************/
Token aa_func08(char lexeme[]){

	Token t;
	double f_value;
	f_value = strtod(lexeme, NULL); /* Convert string to double */

	/* If the value of the variable not in the range
	* as the value of 4-byte float in c. In that case the function
	* must return an error token.
	*/
	if ((f_value > 0 && (f_value > FLT_MAX || f_value < FLT_MIN)) || (f_value < 0 && (f_value < -FLT_MAX || f_value > -FLT_MIN))){
		return aa_func12(lexeme);
	}

	t.code = FPL_T; /* Assigning Floating Point Literal token */
	t.attribute.flt_value = (float)f_value; /* Assigning specific token related attribute */
	return t;
}

/**********************************************************************************************************
* Purpose:			Checks for IL
* Author:			Arin Kumar Poray
* History/Versions:	1.0
* Called Function:	atol(), aa_func12()
* Parameters:		lexeme	char[]	character array containing a lexeme
* Return Value:		INL token
* Algorithm:		(1) Converts the input (lexeme) to long integer value.
*					(2) If the value of the long int not in the range as the value of 2-byte short in c,
*						then the function returns an error token.
*					(3) else the token code is set to INL_T and token attribute, int_value is set to the
*						int value.
*					(4) returns the INL token
**********************************************************************************************************/
Token aa_func05(char lexeme[]){

	Token t;
	long d_value;
	d_value = atol(lexeme); /* Converting string to long int */
	/* If the value of variable not be in the range
	* as the value of 2-byte int in c. In case the function
	* must return an error token.
	*/
	if (d_value > SHRT_MAX || d_value < SHRT_MIN){
		return aa_func12(lexeme);
	}

	t.code = INL_T; /* Assigning Integer literal token */
	t.attribute.int_value = (int)d_value;
	return t;
}

/**********************************************************************************************************
* Purpose:			Checks for OIL
* Author:			Arin Kumar Poray
* History/Versions:	1.0
* Called Function:	atool(), aa_func12()
* Parameters:		lexeme	char[]	character array containing a lexeme
* Return Value:		OIL token
* Algorithm:		(1) Converts the input (lexeme) to Octal value and stored in long int
*					(2) If the value of the long int not in the range as the value of 2-byte short in c,
*						then the function returns an error token.
*					(3) else the token code is set to OIL_T and token attribute, int_value is set to the
*						int value.
*					(4) returns the OIL token
**********************************************************************************************************/
Token aa_func10(char lexeme[]){

	Token t;
	long oc_di_value;
	oc_di_value = atool(lexeme); /* Assigning converted octal value to decimal value */

	/* If the value of variable not be in the range
	* as the value of 2-byte int in c. In case the function
	* must return an error token.
	*/
	if (oc_di_value > SHRT_MAX || oc_di_value < SHRT_MIN){
		return aa_func12(lexeme);
	}

	t.code = INL_T; /* Assigning Integer literal token */
	t.attribute.int_value = (int)oc_di_value;
	return t;
}

/**********************************************************************************************************
* Purpose:			Formats the Error token
* Author:			Fleming Patel
* History/Versions:	1.0
* Called Function:	strlen()
* Parameters:		lexeme	char[]	character array containing a lexeme
* Return Value:		ERR token
* Algorithm:		(1) Token code is set to ERR_T
*					(2) if the size of the input (lexeme) is greater than ERR_LEN, then only first
*						ERR_LEN characters get stored in the token attribute err_lex[] and '\0' at
*						index ERR_LEN+1 to make it a C-type String
*					(3)	else all the characters from the input (lexeme) is stored in the the token
*						attribute err_lex[] and '\0' at the next index to make it a C-type String
*					(4) returns the ERR token
**********************************************************************************************************/
Token aa_func12(char lexeme[]){

	Token t;
	unsigned int i = 0;
	t.code = ERR_T; /* Assigning Error token */

	/* IF THE lexeme IS LONGER than ERR_LEN(see token.h) CHARACTERS,
	* The FIRST ERR_LEN means 20 CHARACTERS ARE STORED
	* INTO THE error token attribite ARRAY err_lex[] and
	* ADD \0 AT THE END TO MAKE A C - type STRING.
	*/
	if (strlen(lexeme) > ERR_LEN){
		for (i = 0; i < ERR_LEN; i++){
			t.attribute.err_lex[i] = lexeme[i];
		}
		t.attribute.err_lex[i] = EOS;
		return t;
	}

	/* Else lexeme size string will be stored
	* into the error token attribite ARRAY err_lex[],
	* and ADD \0 AT THE END TO MAKE A C-type STRING.
	*/
	for (i = 0; i < strlen(lexeme); i++){
		t.attribute.err_lex[i] = lexeme[i];
	}
	t.attribute.err_lex[i] = EOS;
	return t;
}

/**********************************************************************************************************
* Purpose:			Converts the octal to long integer
* Author:			Arin Kumar Poray
* History/Versions: 1.0
* Called Function: strtol()
* Parameters:      lexeme	char[]	character array containing a lexeme
* Return Value:    long integer
* Algorithm:	   None
**********************************************************************************************************/
long atool(char * lexeme){

	long it_c_value;
	it_c_value = strtol(lexeme, NULL, 8);
	return it_c_value;
}