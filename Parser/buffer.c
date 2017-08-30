#include "buffer.h"
/**********************************************************************************************************
* File Name:	buffer.c
* Compiler:		MS Visual Studio 2013
* Author:		Fleming Patel
* Course:		CST 8152 - Compilers, Lab Section: 011
* Assignment:	1
* Date:			29/9/2016
* Professor:	Sv.Ranev
* Purpose:		Contains defination of buffer functions to create and
*               manipulate the buffer.
* Function list:malloc(), calloc(), realloc(), b_create(), b_addc(), b_reset(),
*               b_free(), b_isfull(), b_size(), b_capacity(),
*				b_setmark(), b_mark(), b_mode(), b_incfactor(), b_load(),
*               b_isempty(), b_eob(), b_getc(), b_print(), b_pack(), b_rflag(),
*				b_retract(), b_retract_to_mark(), b_getcoffset(), b_cbhead().
**********************************************************************************************************/

/**********************************************************************************************************
* Purpose:			Creates the buffer structure in the heap(dynamically) and filled the structure values
*					using parameters.
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	malloc(), b_free(), calloc()
* Parameters:		init_capacity	-short	Not less then zero. Range
*											between shortmax and shortmin
*					inc_factor		-char	Must be positive. Range
*											between 0 to 255 inclusive unsigned cast
*					o_mode			-char	must be letter in "f,a,m".
* Return Value:		tempArray		*Buffer	Points to valid buffer structure.
*
* Algorithm:		Checking all bad parameters. Checking init_capacity between in range.
*					If it more than shrtmax it must be negative so it return null.
*					Checkig dynamic memory allocation functions. If malloc/calloc did not work,
*					it must return null.
*					If o_mde is "f" and inc_factor is "0" and also inc_factor is not equal 0 then
*						set the mode to 0 and set inc_factor o 0.
*					If o_mode is "a" and inc_factor is in between 1 to 255
*						set the mode to 1 and inc_factor to inc_factor which passed by parameter
*					If o_mode is "m" and inc_factor is in between 1 to 100
*						set the mode to -1 and inc_factor to inc_factor which passed by parameter
**********************************************************************************************************/
Buffer *b_create(short init_capacity, char inc_factor, char o_mode){

	Buffer * tempArray = NULL; /* Pointer to Buffer Structure */
	/* Checking if init_capacity is not less 0 and not more than it's limit */
	if (init_capacity < 0){
		return NULL;
	}
	/* Fixed Sized buffer can not be created with capacity 0 */
	else if (init_capacity == 0 && o_mode == 'f'){
		return NULL;
	}
	/* Using calloc allocating memory for one structure and assigning values to by 0 for pointers it's NULL */
	if ((tempArray = (Buffer *)calloc(1, sizeof(Buffer))) == NULL){
		return NULL;
	}
	/* Allocating dynamic memory for cb_head with the size of init_capactiy */
	if ((tempArray->cb_head = (char*)malloc(sizeof(char)* init_capacity)) == NULL){
		free(tempArray);
		return NULL;
	}
	if (o_mode == 'f' || (unsigned char)inc_factor == 0){
		tempArray->mode = 0;
		tempArray->inc_factor = 0;
	}
	else if (o_mode == 'a' && (unsigned char)inc_factor <= ADDITIVE_INC_FACTOR_MAX_RANGE && (unsigned char)inc_factor >= ADDITIVE_INC_FACTOR_MIN_RANGE){
		tempArray->mode = 1;
		tempArray->inc_factor = (unsigned char)inc_factor;
	}
	else if (o_mode == 'm' && inc_factor >= MULTIPLICATIVE_INC_FACTOR_MIN_RANGE && inc_factor <= MULTIPLICATIVE_INC_FACTOR_MAX_RANGE){
		tempArray->mode = -1;
		tempArray->inc_factor = inc_factor;
	}
	else{ /* If upper condition did not match then clear memory for cb_head and assigning null to tempArray using b_free method */
		b_free(tempArray); /* Preventing Memory leaks */
		tempArray = NULL; /* Preventing Dangling Pointers */
		return NULL;
	}
	tempArray->capacity = init_capacity; /* If any condition worked then, assigning capcatiy to intial structure capacity */
	return tempArray;
}
/**********************************************************************************************************
* Purpose:			Adding character value to the buffer and if buffer needs more space,
*                   then assign appropriate space
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	b_isfull(),realloc()
* Parameters:		pBD		-*Bufferconst	Points to valid buffer Structure. Other wise,
*                                           value would be null.
*											Const buffer pointer can not be dereference able.
*					symbol	-char			Range between 0 to 255
* Return Value:		pBD		-*Buffer			Points to valid buffer structure
*
* Algorithm:		If pBD is not pointing to valid structure then it returns null.
*					If buffer is not full then adding symbols in available space. else checking modes,
*					if mode is 0 then it fixed sized and can not be incremented. If mode is 1 then
*						algorithm is	n_Capacity = pBD->capacity + (unsigned char)pBD->inc_factor.
*						if it's more than shrtmax then return fail otherwise assigning n_capcatiy to
*                       intial structure capacity
*					if mode is -1 then algorithm is
*						available_space = SHRT_MAX - pBD->capacity
*						new_increment = (available_space * (unsigned char)pBD->inc_factor) / 100
*						n_Capacity = pBD->capacity + new_increment
*						if newincrement is equals to 0 then asigning short max values to
*                       initial structure capcaity, other wise
*						assigning n_capcatiy to intial structure capacity
*					If any mode is 1 or -1 and pass through without fail then realloc assign,
*                       new dynamic memory for buffer
**********************************************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol){

	short n_Capacity; /* local variable where we assigning new memory for buffer */
	char *n_Array = NULL; /* holds newly created memory address of buffer */
	short available_space; /* holds newly created available_space */
	short new_increment; /* holds newly created  new_increment */
	char *temp_cbhead = pBD->cb_head;

	if (pBD == NULL){
		return NULL;
	}
	if (b_isfull(pBD) == 0){ /* Checking buffer is full or not */
		pBD->r_flag = 0;
		*(pBD->cb_head + pBD->addc_offset) = symbol; /* Adding symbol in empty space */
		pBD->addc_offset++;
		return pBD;
	}
	if (pBD->mode == FIXED){ /* Fixed Mode */
		return NULL;
	}
	else if (pBD->mode == ADDITIVE){ /* Additive Mode */
		n_Capacity = pBD->capacity + (unsigned char)pBD->inc_factor; /* Calculating new capacity */
		if (n_Capacity < 0){
			return NULL;
		}
		pBD->capacity = n_Capacity;
	}
	else if (pBD->mode == MULTIPLICATIVE){ /* MultiPlicative Mode */
		if (pBD->capacity == SHRT_MAX){
			return NULL;
		}
		else{
			available_space = SHRT_MAX - pBD->capacity;
			new_increment = (short)((long)(available_space * pBD->inc_factor) / 100);
			n_Capacity = pBD->capacity + new_increment; /* Calculating new capacity */
			/* When available space is 6 so, new increment value must be 0 so instead of checking
			* n_Capacity checking for the new_increment. If it's zero then initial structure capacity
			* as SHRT_MAX.
			*/
			if (new_increment == 0){
				pBD->capacity = SHRT_MAX;
			}
			else{
				pBD->capacity = n_Capacity;
			}
		}
	}
	if (pBD->mode == 1 || pBD->mode == -1){ /* Checking if buffer is additive or multiplicative */
		if ((n_Array = (char *)realloc(pBD->cb_head, sizeof(char)* pBD->capacity)) == NULL){ /* With the help of new capacity creating new memory block in heap using realloc() */
			b_free(pBD);
			return NULL;
		}
		pBD->cb_head = n_Array;
		if (temp_cbhead != pBD->cb_head){
			pBD->r_flag = SET_R_FLAG;
		}
		*(pBD->cb_head + pBD->addc_offset) = symbol; /* Adding symbol in empty space */
		pBD->addc_offset++;
	}
	return pBD;
}
/**********************************************************************************************************
* Purpose:			Reseting all the details of buffer space means not deleting memory.
*                   After this buffer can add symbol from the starting
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD		-*Buffer		Points to valid buffer structure
* Return Value:		TRUE, R_FAIL1
* Algorithm:		None
**********************************************************************************************************/
int b_reset(Buffer * const pBD){

	if (pBD == NULL){
		return R_FAIL1;
	}
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->mark_offset = 0;
	pBD->r_flag = 0;
	pBD->eob = 0;
	return TRUE;
}
/**********************************************************************************************************
* Purpose:			Free all the memory were occupied by the character buffer cb_head and Buffer structure
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	free()
* Parameters:		pBD		-*Buffer		Points to valid buffer structure
* Return Value:		None.
*					But if pBD is not valid then return with exit faliure
* Algorithm:		None
**********************************************************************************************************/
void b_free(Buffer * const pBD){

	if (pBD == NULL){
		return;
	}
	if (pBD->cb_head != NULL){
		free(pBD->cb_head);
		pBD->cb_head = NULL;
	}
	free(pBD);
}
/**********************************************************************************************************
* Purpose:			Checking if the buffer reach it's limit or not.
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD		-*Buffer		Points to valid buffer structure
* Return Value:		R_FAIL1, TRUE, FALSE
* Algorithm:		None
**********************************************************************************************************/
int b_isfull(Buffer * const pBD){

	if (pBD == NULL){
		return R_FAIL1;
	}
	else if ((sizeof(char)* pBD->addc_offset) == (unsigned)pBD->capacity){
		return TRUE;
	}
	return FALSE;
}
/**********************************************************************************************************
* Purpose:			Calculating and returning current size of buffer in bytes
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD			-*Buffer	Points to valid buffer structure
* Return Value:		addc_offset	-short		variable of buffer structure who holds the size of buffer
*					R_FAIL1
* Algorithm:		Nones
**********************************************************************************************************/
short b_size(Buffer* const pBD){

	if (pBD == NULL){
		return R_FAIL1;
	}
	return pBD->addc_offset;
}
/**********************************************************************************************************
* Purpose:			Returning current capacity of buffer in bytes
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD		 -*Buffer	Points to valid buffer structure
* Return Value:		capacity -*Buffer	variable of buffer structure who holds the capacity of buffer
*					R_FAIL1
* Algorithm:		None
**********************************************************************************************************/
short b_capacity(Buffer * const pBD){


	if (pBD == NULL){
		return R_FAIL1;
	}
	return pBD->capacity;
}
/**********************************************************************************************************
* Purpose:			Set the value of mark_offset value
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD		-*Buffer	Points to valid buffer structure
*					mark	-short		Range between o to addc_offset value
* Return Value:		mark_offset			variable of buffer structure who holds the mark of buffer
*					R_FAIL1, R_FAIL1
* Algorithm:		None
**********************************************************************************************************/
short b_setmark(Buffer * const pBD, short mark){

	if (pBD == NULL){
		return R_FAIL1;
	}
	/* Checking mark value. It supposed to o to addc_offset value in bewtween */
	else if (mark >= 0 && mark <= pBD->addc_offset){
		pBD->mark_offset = mark;
		return pBD->mark_offset;
	}
	return R_FAIL1;
}
/**********************************************************************************************************
* Purpose:			Reuturning mark_offset value
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD			-*Buffer	Points to valid buffer structure
* Return Value:		mark_offset	-short		variable of buffer structure who holds the mark of buffer
*					R_FAIL1
* Algorithm:		None
**********************************************************************************************************/
short b_mark(Buffer * const pBD){

	if (pBD == NULL){
		return R_FAIL1;
	}
	return pBD->mark_offset;
}
/**********************************************************************************************************
* Purpose:			Returning the current buffer mode
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD			-*Buffer		Points to valid buffer structure
* Return Value:		pBD->mode					variable of buffer structure who holds
*                                               the current mode of buffer
*					R_FAIL1
* Algorithm:		None
**********************************************************************************************************/
int b_mode(Buffer * const pBD){

	if (pBD == NULL){
		return R_FAIL2;
	}
	return pBD->mode;
}
/***********************************************************************************************************
* Purpose:			Returning the current buffer increment fectore
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD			-*Buffer	Points to valid buffer structure
* Return Value:		inc_factor	-short		variable of buffer structure who holds
*                                           the increment fectore of buffer
*					SIZE_OF_INC_FACTOR
* Algorithm:		None
**********************************************************************************************************/
size_t b_incfactor(Buffer * const pBD){

	if (pBD == NULL){
		return SIZE_OF_INC_FACTOR;
	}
	return (unsigned char)pBD->inc_factor;
}
/**********************************************************************************************************
* Purpose:
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	fgetc(), b_addc(), feof()
* Parameters:		pBD			-*Buffer		Points to valid buffer structure
* Return Value:		numChar		-int			reutrns how many character are add into buffer from file
*					R_FAIL1
* Algorithm:		Until we reach end of the file, adding character into the buffer,
*					if error occur then returns the LOAD_FAIL
**********************************************************************************************************/
int b_load(FILE *const fi, Buffer * const pBD){

	int numChar = 0;	/* to keep track how many character are added in buffer */
	char file_character; /* Holds the file character */
	if (pBD == NULL || fi == NULL){
		return R_FAIL1;
	}
	while (!feof(fi)){ /* Loop go through the end of the file */
		file_character = (char)fgetc(fi); /* Store one file character at a time in file_character */
		if (!feof(fi)){
			if (b_addc(pBD, file_character) == NULL){ /* Adding character in the buffer */
				return LOAD_FAIL;
			}
			else{
				numChar++;
			}
		}
	}
	return numChar;
}
/**********************************************************************************************************
* Purpose:			Checking if buffer is empty or not
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD			-*Buffer		Points to valid buffer structure
* Return Value:		R_FAIL1, TRUE, FALSE
* Algorithm:		None
**********************************************************************************************************/
int b_isempty(Buffer * const pBD){

	if (pBD == NULL){
		return R_FAIL1;
	}
	else if (pBD->addc_offset == 0){
		return TRUE;
	}
	return FALSE;
}
/**********************************************************************************************************
* Purpose:			Returning end of buffer means b_eob value
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD			-*Buffer		Points to valid buffer structure
* Return Value:		eob			-int			flag variable of buffer structure who indicate
*                                               end of the buffer
*					R_FAIL1
* Algorithm:		None
**********************************************************************************************************/
int b_eob(Buffer * const pBD){

	if (pBD == NULL){
		return R_FAIL1;
	}
	return pBD->eob;
}
/**********************************************************************************************************
* Purpose:			Returning character symbol on specified location
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD			 -*Buffer	Points to valid buffer structure
* Return Value:		c_OnPosition -char		holds the character symbol
*					R_FAIL2, R_FAIL1
* Algorithm:		None
**********************************************************************************************************/
char b_getc(Buffer * const pBD){

	char c_OnPosition; /* holds character from the buffer */
	if (pBD == NULL){
		return R_FAIL2;
	}
	/* If this condition is matched then buffer reached it's limit */
	else if (pBD->getc_offset == pBD->addc_offset){
		pBD->eob = 1;
		return R_FAIL1;
	}
	else{
		pBD->eob = 0;
		/* Assigning buffer character in c_onPosition regarding their perticular address */
		c_OnPosition = *(pBD->cb_head + pBD->getc_offset);
		pBD->getc_offset++;
		return c_OnPosition;
	}
}
/**********************************************************************************************************
* Purpose:			Printing all character from the buffer and returning total number of character in buffer
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	b_getc()
* Parameters:		pBD			  -*Buffer	Points to valid buffer structure
* Return Value:		num_character -int		Holds how many characters are in the buffer
*					R_FAIL1
* Algorithm:		Until we get the end of the buffer, we calling b_getc() method and printing each charcater in the buffer.
*					Here we are assigning initial structure getc_offset value to local variable and assigning 0 to
*					initial structure getc_offset. So it worked as a loop. In the last we reassigned initial getc_offset value from
*					the local variable.
**********************************************************************************************************/
int b_print(Buffer * const pBD){

	int numChar = 0;
	short tempGetCOffset;
	char inChar;
	if (pBD == NULL){
		return R_FAIL1;
	}
	if (b_isempty(pBD) == TRUE){
		printf("The buffer is empty.\n");
		return R_FAIL1;
	}
	else{
		tempGetCOffset = pBD->getc_offset;
		pBD->getc_offset = 0;
		do{
			inChar = (char)b_getc(pBD);
			if (pBD->eob != TRUE){
				printf("%c", inChar);
				numChar++;
			}
		} while (b_eob(pBD) != TRUE);
		printf("\n");
		pBD->getc_offset = tempGetCOffset;
		return numChar;
	}
}
/***********************************************************************************************************
* Purpose:			Resize the buffer with the inital size with one extra space
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	realloc()
* Parameters:		pBD			-*Buffer		Points to valid buffer structure
* Return Value:		pBD			-*Buffer		Points to valid buffer structure.
* Algorithm:		Adding one more space in the initial capcacity
*						n_Capacity = pBD->addc_offset + 1
*					if n_capacity is more than shrtmax limit then it returns null
*					Otherwise with the help of realloc we are assigning new memory block for buffer
**********************************************************************************************************/
Buffer *b_pack(Buffer *const pBD){

	char *n_Array = NULL; /*  holds newly created memory address of buffer */
	short n_Capacity = 0; /* holds the newly calculated capacity for the buffer */
	n_Capacity = (sizeof(char)* pBD->addc_offset) + 1; /* calculating capacity */
	char *temp_cbhead = pBD->cb_head;
	if (n_Capacity < 0){ /* If capacity is more than SHRT_MAX then it returns null */
		return NULL;
	}
	/* Creating new memory block in the heap using realloc() */
	else if ((n_Array = (char *)realloc(pBD->cb_head, sizeof(char)* n_Capacity)) == NULL){
		b_free(pBD);
		return NULL;
	}
	pBD->cb_head = n_Array; /* Assigning new memory block to structure initial cb_head */
	pBD->capacity = n_Capacity; /* Assigning new capacity to structure initial capacity */
	if (temp_cbhead != pBD->cb_head){
		pBD->r_flag = SET_R_FLAG; /* Change the flag value because buffer was modified */
	}
	return pBD;
}
/**********************************************************************************************************
* Purpose:			Returns the reallocation flag of buffer
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD			-*Buffer	Points to valid buffer structure
* Return Value:		r_flag		-char		flag variable of buffer structure who indicate
*                                           the buffer capacity modified or not
*					R_FAIL1
* Algorithm:		None
**********************************************************************************************************/
char b_rflag(Buffer * const pBD){

	if (pBD == NULL){
		return R_FAIL1;
	}
	return pBD->r_flag;
}
/**********************************************************************************************************
* Purpose:			It decrements getc_offset by 1
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD			-*Buffer	Points to valid buffer structure
* Return Value:		getc_offset	-short		variable of buffer structure who holds the location of character from the beginning
*					R_FAIL1
* Algorithm:
**********************************************************************************************************/
short b_retract(Buffer * const pBD){

	if (pBD == NULL){
		return R_FAIL1;
	}
	else if (pBD->getc_offset == 0){
		return R_FAIL1;
	}
	else{
		pBD->getc_offset--; /* Decrementing getc_offset value by 1 */
		return pBD->getc_offset;
	}
}
/***********************************************************************************************************
* Purpose:			Mark specific character locaion and return as a getc_offset
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD			-*Buffer	Points to valid buffer structure
* Return Value:		getc_offset	-short		variable of buffer structure who holds the location of character from the beginning
R_FAIL1
* Algorithm:
**********************************************************************************************************/
short b_retract_to_mark(Buffer * const pBD){

	if (pBD == NULL){
		return R_FAIL1;
	}
	pBD->getc_offset = pBD->mark_offset;
	return pBD->getc_offset;
}
/**********************************************************************************************************
* Purpose:			Returns the value of getc_offset
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD			-*Buffer	Points to valid buffer structure
* Return Value:		getc_offset	-short		variable of buffer structure who holds the location of character from the beginning
R_FAIL1
* Algorithm:		None
**********************************************************************************************************/
short b_getcoffset(Buffer * const pBD){

	if (pBD == NULL){
		return R_FAIL1;
	}
	return pBD->getc_offset;
}
/**********************************************************************************************************
* Purpose:			Returning head of character array means where cb_head is pointing in heap
* Author:			Fleming Patel
* History/Versions:	29/9/2016
* Called Function:	None
* Parameters:		pBD			-*Buffer		Points to valid buffer structure
* Return Value:		cb_head		-pointer
*								 to char		pointer to the beginning of character array
* Algorithm:		None
**********************************************************************************************************/
char * b_cbhead(Buffer * const pBD){

	if (pBD == NULL){

		return NULL;
	}
	return pBD->cb_head;
}