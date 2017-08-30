/**********************************************************************************************************
* File Name:	stable.c
* Compiler:		MS Visual Studio 2013
* Author:		Fleming Patel & Arin Poray
* Course:		CST 8152 - Compilers, Lab Section: 011
* Assignment:	03
* Date:			24/11/2016
* Professor:	Sv.Ranev
* Purpose:		Contains definition of scanner utility functions to perform various scanner functions.
* Function list: b_create(), b_addc(), b_free(), b_cbhead(), st_create(), st_install(), st_lookup(), st_update_type(),
*				 st_update_value(), st_get_type(), st_destroy(), st_print(), st_setsize(), st_incoffset(), st_store(),
*				 st_sort()
**********************************************************************************************************/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS

/* project header files */
#include "stable.h"

/**********************************************************************************************************
* Purpose:			Creates a symbol table structure in the heap and initializes the symbol table's values
*					using the parameters passed
* Author:			Fleming Patel
* History/Versions: 1.0
* Called Function:	b_create()
* Parameters:		st_size			int		number of STVRs that can be stored in the dynamic memory
*											allocated in the heap, pointed to by pstvr
* Return Value:		temp_descriptor	STD		points to a valid STVR structure in the heap
* Algorithm:		(1) checks whether the value passed as st_size is greater than ZERO or not
					(2) if NO(i.e., less than ZERO), returns a STD pointer with its st_size set to ZERO
					(3) if YES(i.e., greater than ZERO), allocates dynamic memory for storage of STVR
					(4) allocates dynamic memory for storing the lexemes of each STVR
					(5) sets st_offset to ZERO, and st_size to the value passed
					(6) if any of the allocation fails, sets st_size to ZERO
					(7) returns a pointer to the STD
**********************************************************************************************************/
STD st_create(int st_size){
	
	STD temp_descriptor;
	short init_capacity = 0;
	/* Checking if st_size is in range or not */
	if (st_size < 0){
		temp_descriptor.st_size = 0;
		return temp_descriptor;
	}

	/* Allocating dynamic memory for pstvr with the size of st_size */
	if ((temp_descriptor.pstvr = (STVR *)malloc(sizeof(STVR) * st_size)) == NULL){
		temp_descriptor.st_size = 0;
		return temp_descriptor;
	}
	init_capacity = (short)((long)(st_size * DEFAULT_VID_LEN)); /* Assigning capacity to related vid lenth */

	/* Using buffer utilities to create a buffer */
	if ((temp_descriptor.plsBD = b_create(init_capacity, DEFAULT_INC_FACTOR, DEFAULT_BUFFER_MODE)) == NULL){
		temp_descriptor.st_size = 0;
		free(temp_descriptor.pstvr);
		return temp_descriptor;
	}

	temp_descriptor.st_offset = 0; /* Setting the set offset to zero (0) */
	temp_descriptor.st_size = st_size; /* Setting the initial st_size to st_size */
	return temp_descriptor;
}

/**********************************************************************************************************
* Purpose:			Installs a new entry (VID record) in the symbol table(i.e.,sym_table)
*
* Author:			Arin Kumar Poray and Fleming Patel
* History/Versions:	1.0
* Called Function:	st_lookup(), b_cb_head, b_size(), b_addc(), b_rflag(), st_incoffset()
* Parameters:		sym_table	STD		points to a symbol table descriptor structure in the heap
*					lexeme		char*	points to a char array in the memory
*					type		char	signifies the datatype of the lexeme
*					line		int		denotes the line number in the source code where the lexeme first
*										encountered
* Return Value:		temp_offset	int		index in the array of STVR(the array pointed by pstvr) where the
*										lexeme is stored or already exists
* Algorithm:		(1) calls the st_lookup() function to search for the lexeme (variable name) in
*						sys_table
*					(2) if lexeme not found
*						(2.1) installs the new entry at the current st_offset
*						(2.2) sets plex and o_line to their corresponding values, and the status_field to its
*							  default value
*						(2.3) sets the data type indicator to a value corresponding to the type of the variable
*						      specified by the parameter type using bitwise operation
*						(2.4) sets the i_value to ZERO for integer and floating-point variables, and to –1 for
*						      string variables
*						(2.5) returns the current offset of that entry from the beginning of the array of STVR
*							  (the array pointed by pstvr)
*					(3) if lexeme present
*						(3.1) returns the corresponding offset
*					(4) if sys_table is full, returns –1.
**********************************************************************************************************/
int st_install(STD sym_table, char *lexeme, char type, int line){
	
	int length = 0; 
	int index = 0;
	int is_Found;
	int temp_offset;

	if (sym_table.st_size == 0){ /* Checking if Symbole Table is empty */
		return SR_FAIL2;
	}
	is_Found = st_lookup(sym_table, lexeme); /* Checking if lexeme is in Symbole Table or not */
	
	if (is_Found == -1){ /* i.e., lexeme not found in the sym_table */

		if (sym_table.st_offset == sym_table.st_size){
			return SR_FAIL1;
		}

		/* setting plex to point to the position at which the new lexeme will be stored in plsBD, the Character Array (CA) */
		sym_table.pstvr[sym_table.st_offset].plex = b_cbhead(sym_table.plsBD) + b_size(sym_table.plsBD);

		/* setting the o_line to the line number in the source file where the lexeme first occured */
		sym_table.pstvr[sym_table.st_offset].o_line = line;

		/* resetting status_field to ZERO */
		sym_table.pstvr[sym_table.st_offset].status_field &= DEFAULTZ;

		/* setting the status_field to DEFAULT value */
		sym_table.pstvr[sym_table.st_offset].status_field |= DEFAULT;

		if (type == DEFAULT_INT_SI){
			/* setting the status_field to denote that the new lexeme is a VID of INTEGER type */
			sym_table.pstvr[sym_table.st_offset].status_field |= SET_IL;

			/* setting the i_value to 0 */
			sym_table.pstvr[sym_table.st_offset].i_value.int_val = 0;
		}

		if (type == DEFAULT_FLOAT_SI){
			/* setting the status_field to denote that the new lexeme is a VID of FLOAT POINT type */
			sym_table.pstvr[sym_table.st_offset].status_field |= SET_FPL;

			/* setting the i_value to 0.0 */
			sym_table.pstvr[sym_table.st_offset].i_value.fpl_val = 0;
		}

		if (type == DEFAULT_STRING_I){
			/* setting the LSB to 1 */
			sym_table.pstvr[sym_table.st_offset].status_field |= SET_LSB;

			/* setting the status_field to denote that the new lexeme is a VID of STRING type */
			sym_table.pstvr[sym_table.st_offset].status_field |= SET_SL; /* setting status_field to 1111 1111 1111 1111 */

			/* setting the i_value to -1 */
			sym_table.pstvr[sym_table.st_offset].i_value.str_offset = -1;
		}
		char* temp = b_cbhead(sym_table.plsBD);
		length = strlen(lexeme);
		/* storing the new lexeme in plsBD */
		for (index = 0; index < length; index++){
			b_addc(sym_table.plsBD, lexeme[index]);
			/* After copying lexeme checking for buffer reallocation flag inside b_addc function */
			if (b_rflag(sym_table.plsBD) == SET_R_FLAG){
				int i = 0;
				int offset = b_cbhead(sym_table.plsBD) - temp;
				for (i = 0; i <= sym_table.st_offset; i++){
					sym_table.pstvr[i].plex += offset;
				}
				temp = b_cbhead(sym_table.plsBD);
			}
		}
		
		b_addc(sym_table.plsBD, EOS); /* Adding '\0' in last to make c type string */
		temp_offset = sym_table.st_offset; /* temporarily storing st_offset to return later */
		st_incoffset();	/* incrementing st_offset by 1 */
		return temp_offset;		/* returning the index in pstvr array at which the new STVR is stored */
	}
	return is_Found;
}

/**********************************************************************************************************
* Purpose:			Searches for a lexeme (variable name) in the symbol table(i.e.,sym_table)
*
* Author:			Arin Kumar Poray
* History/Versions:	1.0
* Called Function:	No user defined function called
* Parameters:		sym_table	STD		points to a symbol table descriptor structure in the heap
*					lexeme		char*	points to a char array in the memory
* Return Value:		i			int		the offset of the entry from the beginning of the array
*										of STVR (i.e., lexeme present)
*										-1 (i.e., lexeme not present)
* Algorithm:		(1) search is performed backward from the last entry to the beginning of the array of
*						STVR
*					(2) if lexeme present, returns the offset of the entry from the beginning of the array
*						of STVR
*					(3) otherwise, returns –1
**********************************************************************************************************/
int st_lookup(STD sym_table, char *lexeme){
	
	int i = 0;
	/* starting from the last entry to the beginning of pstvr, searches for a variable name in sym_table that matches with lexeme */
	for (i = sym_table.st_offset - 1; i >= 0; i--){
		if (strcmp(sym_table.pstvr[i].plex,lexeme) == 0){
			return i;/* returns the index in pstvr at which the matching lexeme is found */
		}
	}
	return SR_FAIL1;/* returns -1 if the lexeme doesn't match with any variable name in sym_table */
}

/**********************************************************************************************************
* Purpose:			Updates the data type indicator in the variable entry (STVR) specified by vid_offset
*
* Author:			Fleming Patel
* History/Versions:	1.0
* Called Function:	None
* Parameters:		sym_table	STD		points to a symbol table descriptor structure in the heap
*					vid_offset	int		index in the array of STVR(the array pointed by pstvr) to
*										updated
*					v_type		char	datatype to which the STVR is to be updated
* Return Value:		vid_offset	int		the same vid_offset received on successful update
*										-1 if STVR already updated previously
*										-2 on failure
* Algorithm:		(1) checks the update flag (LSB) of the status_field of the entry
*					(2) if equals to 1, the type has been already updated and returns –1
*					(3)	Otherwise, the function updates the data type indicator of the status_field,
*						sets the LSB of the status_field to 1, and returns vid_offset using bitwise
*						operation
**********************************************************************************************************/
int st_update_type(STD sym_table, int vid_offset, char v_type){

	/* Checking if Symbole Table is empty */
	if (sym_table.st_size == 0){
		return SR_FAIL2;
	}

	/* Checking vid offset is in the range */
	if (vid_offset >= sym_table.st_offset){
		return SR_FAIL1;
	}

	/* checks TRUE if update flag in status_field is set to 1 */
	if ((sym_table.pstvr[vid_offset].status_field & CHK_LSB) == 1){
		return SR_FAIL1;
	}

	/* RESETTING the status_field */
	sym_table.pstvr[vid_offset].status_field &= DEFAULTZ; /* setting status_field to 0000 0000 0000 0000 */

	sym_table.pstvr[vid_offset].status_field |= DEFAULT; /* setting status_field to 1111 1111 1111 1000 */

	if (v_type == DEFAULT_INT_SI){
		/* setting the status_field of the VID to Integer */
		sym_table.pstvr[vid_offset].status_field |= SET_IL; /* setting status_field to 1111 1111 1111 1100 */
	}

	if (v_type == DEFAULT_FLOAT_SI){
		/* setting the status_field of the VID to Float */
		sym_table.pstvr[vid_offset].status_field |= SET_FPL; /* setting status_field to 1111 1111 1111 1010 */
	}

	/* SETTING the update flag in status_field to 1 to denote UPDATE */
	sym_table.pstvr[vid_offset].status_field |= SET_LSB;

	return vid_offset;
}

/**********************************************************************************************************
* Purpose:			Updates the i_value of the variable specified by vid_offset
*
* Author:			Arin Kumar Poray
* History/Versions:	1.0
* Called Function:	None
* Parameters:		sym_table	STD				points to a symbol table descriptor structure in the heap
*					vid_offset	int				index in the array of STVR(the array pointed by pstvr) to
*												updated
*					i_value		InitialValue	the value to which the variable will be updated to
* Return Value:		vid_offset	int				the same vid_offset received on successful update
*												-1 or -2 on failure
* Algorithm:		(1) updates the i_value of the variable specified by vid_offset
*					(2) On success, returns vid_offset
*					(3) On failure, returns –1
**********************************************************************************************************/
int st_update_value(STD sym_table,int vid_offset,InitialValue i_value){

	/* Checking if Symbole Table is empty */
	if (sym_table.st_size == 0){
		return SR_FAIL2;
	}

	/* Checking vid offset is in the range */
	if (vid_offset >= sym_table.st_offset){
		return SR_FAIL1;
	}

	sym_table.pstvr[vid_offset].i_value = i_value;
	return vid_offset;
}

/**********************************************************************************************************
* Purpose:			Returns the type of the variable specified by vid_offset
*
* Author:			Fleming Patel
* History/Versions:	1.0
* Called Function:` None
* Parameters:		sym_table	STD		points to a symbol table descriptor structure in the heap
*					vid_offset	int		index in the array of STVR(the array pointed by pstvr)
* Return Value:		'F'			char	for floating-point type,
*					'I'			char	for integer type,
*					'S'			char	for string type
*					-1			char	on failure
* Algorithm:		None
**********************************************************************************************************/
char st_get_type(STD sym_table,int vid_offset){

	
	/* Checking if Symbole Table is empty */
	if (sym_table.st_size == 0){
		return SR_FAIL2;
	}
	
	/* Checking vid offset is in the range */
	if (vid_offset >= sym_table.st_offset){
		return SR_FAIL1;
	}

	/* Checking for integer vid */
	if ((sym_table.pstvr[vid_offset].status_field & CHK_IL) == 4){
		return DEFAULT_INT_SI;
	}
	/* Checking for Float vid */
	else if ((sym_table.pstvr[vid_offset].status_field & CHK_FPL) == 2){
		return DEFAULT_FLOAT_SI;
	}
	/* Checking for String vid */
	else if ((sym_table.pstvr[vid_offset].status_field & CHK_SL) == 7){
		return DEFAULT_STRING_I;
	}
	return R_FAIL1;
}

/**********************************************************************************************************
* Purpose:			Frees the memory occupied by the symbol table dynamic areas and sets st_size to 0
* Author:			Arin Kumar Poray
* History/Versions:	1.0
* Called Function:	b_free(), st_setsize()
* Parameters:		sym_table	STD		points to a symbol table descriptor structure in the heap
* Return Value:		None(i.e., void)
* Algorithm:		None
**********************************************************************************************************/
void st_destroy(STD sym_table){

	st_setsize(); /* Calling static st_setsize method */
	b_free(sym_table.plsBD);
	free(sym_table.pstvr);
	sym_table.pstvr = NULL;
}

/**********************************************************************************************************
* Purpose:			Prints the contents of the symbol table to the standard output (screen) in
*
* Author:			Fleming Patel
* History/Versions:	1.0
* Called Function:	None
* Parameters:		sym_table	STD		points to a symbol table descriptor structure in the heap
* Return Value:		numEntries	int		number of variables and its attributes that is printed
* Algorithm:		None
**********************************************************************************************************/
int st_print(STD sym_table){
	
	int i = 0;
	int numEntries = 0;
	if (sym_table.st_size == 0){
		return SR_FAIL2;
	}
	
	printf("\nSymbol Table\n");
	printf("____________\n\n");
	printf("Line Number Variable Identifier\n");

	for (i = 0; i < sym_table.st_offset; i++){
		printf("%2d%9s %s\n", sym_table.pstvr[i].o_line, "", sym_table.pstvr[i].plex);
		numEntries++;
	}
		
	return numEntries;
}

/**********************************************************************************************************
* Purpose:			This “internal” function sets st_size to 0,
*					Used when one wants to set st_size to 0 in some function which does not have access to
*					the global sym_table variable
*
* Author:			Flemming Patel
* History/Versions:	1.0
* Called Function:	None
* Parameters:		None
* Return Value:		None
* Algorithm:		None
**********************************************************************************************************/
static void st_setsize(void){
	/* Setting size to zero */
	sym_table.st_size = 0;
}

/**********************************************************************************************************
* Purpose:			This “internal” function increments st_offset by 1
*					Used when one wants to increment st_offset by 1 in some function which does not have
*					access to the global sym_table variable
*
* Author:			Flemming Patel
* History/Versions:	1.0
* Called Function:	None
* Parameters:		None
* Return Value:		None
* Algorithm:		None
**********************************************************************************************************/
static void st_incoffset(void){
	sym_table.st_offset++;/* Incrementing st_offset by one */
}

/**********************************************************************************************************
* Purpose:			Stores the symbol table into a text file named $stable.ste
*
* Author:			Arin Kumar Poray
* History/Versions: 1.0
* Called Function:	st_get_type()
* Parameters:		sym_table		STD		points to a symbol table descriptor structure in the heap
* Return Value:		numofRecords	int		number of variable stored in the text file named $stable.ste
* Algorithm:		(1) If the file already exists in the current directory, overwrites it
*					(2) uses fprintf() to write to the file
*					(3)	first, writes st_size
*					(4)	Then for each symbol table entry,
*						(4.1) writes the status_field (in hex format),
*						(4.2) the length of the lexeme,
*						(4.3) the lexeme,
*						(4.4) the line number,
*						(4.5) and the initial value
*					(5) On success, prints a message “Symbol Table stored” and returns the number of
*						records stored
*					(6) returns –1 on failure
**********************************************************************************************************/
int st_store(STD sym_table){
	int numofRecord = 0;
	FILE *fp;
	int i = 0;
	STVR *temp;
	int j = 0;

	/* Creating File as "$stable.ste.txt" */
	if ((fp = fopen("$stable.ste", "wt")) == NULL){
		perror("File creation error");
		return SR_FAIL1;
	}

	/* First fprintf is for printing size of symbolTable */
	fprintf(fp,"%d ",sym_table.st_size);
	for (i = 0; i < sym_table.st_offset; i++){
		temp = sym_table.pstvr + i;
		/* If retur type character is 'I' */
		if ((st_get_type(sym_table,i)) == 'I'){
			fprintf(fp,"%04X %d %s %d %d",temp->status_field,strlen(temp->plex),temp->plex,temp->o_line,temp->i_value);
		}
		/* If retur type character is 'F' */
		else if ((st_get_type(sym_table, i)) == 'F'){
			fprintf(fp,"%04X %d %s %d %.2f", temp->status_field, strlen(temp->plex), temp->plex, temp->o_line, temp->i_value);
		}
		/* If retur type character is 'S' */
		else if ((st_get_type(sym_table, i)) == 'S'){
			fprintf(fp,"%04X %d %s %d %d", temp->status_field, strlen(temp->plex), temp->plex, temp->o_line, sym_table.st_offset);
		}
		/* If an error return -1 */
		else{
			return SR_FAIL1;
		}
		j++;
		if (j < sym_table.st_offset){
			fprintf(fp," ");
		}
		numofRecord++;
	}
	/* Closing the file */
	fclose(fp);
	printf("\nSymbol Table stored.\n");
	return numofRecord;
}

/**********************************************************************************************************
* Purpose:
*
* Author:
* History/Versions:
* Called Function:
* Parameters:
* Return Value:
* Algorithm:
**********************************************************************************************************/
int st_sort(STD sym_table, char s_order){
	if (sym_table.st_size == 0)
		return SR_FAIL1;

	return 0;
}