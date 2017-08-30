/**********************************************************************************************************
* File Name:	parser.c
* Compiler:		MS Visual Studio 2013
* Author:		Fleming Patel & Arin Poray
* Course:		CST 8152 - Compilers, Lab Section: 011
* Assignment:	04
* Date:			08/12/2016
* Professor:	Sv.Ranev
* Purpose:		Contains definition of parser utility functions to perform various parser functions.
* Function list:  parser(Buffer*) match(int, int) syn_eh(int) syn_printe() gen_incode() program() opt_statements()
statements() statement() statements_prime() assignment_statement() assignment_expression()
arithmatic_expression() unary_arithmatic_expression() primary_arithmatic_expression()
additive_arithmatic_expression() multiplicative_arithmatic_expression()
multiplicative_arithmatic_expression_prime() additive_arithmatic_expression_prime()
string_expression() selection_statement() iteration_statement() input_statement()
variable_list() variable_identifier() variable_list_prime() output_statement()
output_list() input_statement() conditional_list() logical_OR_expression()
logical_AND_expression() logical_OR_expression_prime() logical_AND_expression_prime()
relational_expression() primary_a_relational_expression()
primary_a_relational_expression_prime() primary_s_relational_expression()
primary_s_relational_expression_prime() primary_string_expression()
string_expression_prime() conditional_expression()

**********************************************************************************************************/

/* project header files */
#include "parser.h"


/***********************************************************************************
*Purpose :			match token passed to the token in lookahead
*Author :			Fleming patel
*History/Version :	1.0
*Called functions :
*parameters :		pr_token_code: token code
*pr_token_attribute: token attribute
*Return value :		none
*Algorithm :			Check for token code and attribute if present and then also check
SEOF and call next token
***********************************************************************************/
void parser(Buffer* in_buff){
	sc_buf = in_buff;
	lookahead = mlwpar_next_token(sc_buf);
	program();
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed\n");
}

/**********************************************************************************************************
* Purpose:			matches two tokens: the current input token (lookahead) and the token required by the
*					parser
* Author:			Fleming Patel
* History/Versions: 1.0
* Called Function:	mlwpar_next_token(), syn_eh(), syn_printe()
* Parameters:		pr_token_code		int		token code of the Token required by the parser
*					pr_token_attribute	int		token attribute of the Token required by the parser
* Return Value:		None
* Algorithm:
**********************************************************************************************************/
void match(int pr_token_code, int pr_token_attribute){

	if (pr_token_code != lookahead.code){
		syn_eh(pr_token_code);
		return;
	}

	if (lookahead.code == SEOF_T){
		return;
	}

	switch (pr_token_code){
	case KW_T:
		if (pr_token_attribute != lookahead.attribute.kwt_idx){
			syn_eh(pr_token_code);
			return;
		}
		break;
	case LOG_OP_T:
		if (pr_token_attribute != lookahead.attribute.log_op){
			syn_eh(pr_token_code);
			return;
		}
		break;
	case ART_OP_T:
		if (pr_token_attribute != lookahead.attribute.arr_op){
			syn_eh(pr_token_code);
			return;
		}
		break;
	case REL_OP_T:
		if (pr_token_attribute != lookahead.attribute.rel_op){
			syn_eh(pr_token_code);
			return;
		}
		break;

	default:
		break;
	}

	lookahead = mlwpar_next_token(sc_buf);

	if (lookahead.code == ERR_T){
		syn_printe();
		lookahead = mlwpar_next_token(sc_buf);
		synerrno++;
		return;
	}
}

/**********************************************************************************************************
* Purpose:			Implements a simple panic mode error recovery
* Author:			Arin Poray
* History/Versions: 1.0
* Called Function:	mlwpar_next_token(), syn_printe()
* Parameters:		sync_token_code		int		token code of the Token required by the parser
* Return Value:		None
* Algorithm:
**********************************************************************************************************/

void syn_eh(int sync_token_code){

	syn_printe();
	synerrno++;

	do{
		lookahead = mlwpar_next_token(sc_buf);

		if (sync_token_code == lookahead.code){
			lookahead = mlwpar_next_token(sc_buf);
			return;
		}

		if (lookahead.code == SEOF_T){
			exit(synerrno);
			return;
		}
	} while (sync_token_code != lookahead.code);
}

/*********************************************************************************
Parser error printing function, Assignmet 4, F16
************************************************************************************/
void syn_printe(){
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", sym_table.pstvr[t.attribute.get_int].plex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_cbhead(str_LTBL) + t.attribute.str_offset);
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/* end switch*/
}/* end syn_printe()*/

void gen_incode(char* printMessage){
	printf("%s", printMessage);
}

/**********************************************************************************
* <program>  -> PLATYPUS {<opt_statements>}
* FIRST(<program>) = {KW_T(PLATYPUS)}
***********************************************************************************/
void program(void){
	match(KW_T, PLATYPUS); 
	match(LBR_T, NO_ATTR); 
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed\n");
}

/*
* < opt_statements >  -> <statements>|E
* FIRST(<opt_statements>) = {E,AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT), KW_T(OUTPUT)}
* Author : Fleming Patel
*/
void opt_statements(void){
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code){
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT here and in
		statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT){
			statements();
			break;
		}
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Opt_statements parsed\n");
	}
}

/*
* <statements> -> <statement><statements’>
* FIRST(<statements>) = {AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT), KW_T(OUTPUT)}
* Author : Fleming Patel
*/
void statements(void){
	statement();
	statements_prime();
}

/*
* Author :Fleming Patel
*<statements’> -> <statement><statements’> | E
*FIRST(<statements>) = {E, AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT), KW_T(OUTPUT)}
*/
void statements_prime(void){
	switch (lookahead.code){
		case KW_T:
			switch (lookahead.attribute.kwt_idx){
				case PLATYPUS:
				case ELSE:
				case THEN:
				case REPEAT:
					return;
				default:
					break;
			}
		case AVID_T:
		case SVID_T:
			statement();
			statements_prime();
			break;
		}
}

/*
* Author : Fleming Patel
*<statement> -> <assignment statement> | <selection statement> |<iteration statement> | <input statement> |<output statement>
*FIRST(<statement>) = {AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT),
*/
void statement(void){
	switch (lookahead.code){
	case AVID_T:
	case SVID_T: assignment_statement();
		break;
	case KW_T:
		switch (lookahead.attribute.kwt_idx){
		case IF:
			selection_statement();
			break;
		case USING:
			iteration_statement();
			break;
		case INPUT:
			input_statement();
			break;
		case OUTPUT:
			output_statement();
			break;
		default:
			syn_printe();
		}
		break;
	default:
		syn_printe();
	}
}

/*
* Author : Fleming Patel
*<assignment statement> -> <assignment expression>;
*FIRST(<assignment statement>) = {AVID_T, SVID_T}
*/
void assignment_statement(void){
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed\n");
}

/*
* Author : Fleming Patel
*< assignment expression> ->AVID = <arithmetic expression> | SVID = <string expression>
*FIRST(<assignment expression>) = {AVID_T, SVID_T}
*/
void assignment_expression(void){
	switch (lookahead.code){
	case AVID_T:
		match(AVID_T, NO_ATTR); // required for advancing the lookahead to hold the next token
		match(ASS_OP_T, EQ);
		arithmatic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed\n");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR); // required for advancing the lookahead to hold the next token
		match(ASS_OP_T, EQ);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed\n");
		break;
	default:
		syn_printe();
		return;
	}
}

/*
* Author : Fleming Patel
*<arithmetic expression> - > <unary arithmetic expression> | <additive arithmetic expression>
*FIRST(<arithmetic expression>) = { ART_OP_T(PLUS), ART_OP_T(MINUS), AVID_T, FPL_T, INL_T, LPR_T }
*/
void arithmatic_expression(void){
	switch (lookahead.code){
	case ART_OP_T:
		switch (lookahead.attribute.arr_op){
		case MULT:
		case DIV:
			syn_printe(); /* illegal match */
			return;
		default:
			break;
		}
		unary_arithmatic_expression();
		break;

	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmatic_expression();
		break;

	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Arithmetic expression parsed\n");
}

/*
* Author : Fleming Patel
*<unary arithmetic expression> -> -  <primary arithmetic expression> | + <primary arithmetic expression>
*FIRST(<unary arithmetic expression>) = { ART_OP_T(PLUS), ART_OP_T(MINUS)}
*/
void unary_arithmatic_expression(void){
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case MULT:
		case DIV:
			syn_printe();
			return;
		default:
			break;
		}
		match(lookahead.code, lookahead.attribute.arr_op);
		primary_arithmatic_expression();
		gen_incode("PLATY: Unary arithmetic expression parsed\n");
		break;
	default:	/* no match */
		syn_printe();
		return;
	}
}

/*
* Author :Fleming Patel
*<primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)
*FIRST(<primary arithmetic expression>) = { AVID_T, FPL_T, INL_T, LPR_T }
*/
void primary_arithmatic_expression(void){
	switch (lookahead.code){
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmatic_expression();
		match(RPR_T, NO_ATTR);
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed\n");
}

/*
* Author : Fleming Patel
*<additive arithmetic expression> -> <multiplicative arithmetic expression><additive arithmetic expression’>
*FIRST(<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T, LPR_T }
*/
void additive_arithmatic_expression(void){
	multiplicative_arithmatic_expression();
	additive_arithmatic_expression_prime();
}


/*
* Author : Fleming Patel
*<multiplicative arithmetic expression> -> <primary arithmetic expression> <multiplicative arithmetic expression’>
*FIRST(<multiplicative arithmetic expression>) = { AVID_T, FPL_T, INL_T, LPR_T }
*/
void multiplicative_arithmatic_expression(void){
	primary_arithmatic_expression();
	multiplicative_arithmatic_expression_prime();

}


/*
* Author : Fleming Patel
*<multiplicative arithmetic expression’> -> *<primary arithmetic expression> <multiplicative arithmetic expression’>| /<primary arithmetic expression> <multiplicative arithmetic expression’>| E
*FIRST(<multiplicative arithmetic expression’>) = {E, ART_OP_T(DIV), ART_OP_T(MULT)}
*/
void multiplicative_arithmatic_expression_prime(void){
	switch (lookahead.code){
	case ART_OP_T:
		switch (lookahead.attribute.arr_op){
		case MULT:
			match(ART_OP_T, MULT);
			primary_arithmatic_expression();
			multiplicative_arithmatic_expression_prime();
			break;
		case DIV:
			match(ART_OP_T, DIV);
			primary_arithmatic_expression();
			multiplicative_arithmatic_expression_prime();
			break;
		default:
			return;
		}
		break;
	default:
		return;
	}
	gen_incode("PLATY: Multiplicative arithmetic expression parsed\n");
}

/*
* Author : Fleming Patel
*<additive arithmetic expression’> -> +<multiplicative arithmetic expression><additive arithmetic expression’>|-<multiplicative arithmetic expression><additive arithmetic expression’>| E
*FIRST(<additive arithmetic expression’>) = { E, ART_OP_T(PLUS), ART_OP_T(MINUS)}

*/
void additive_arithmatic_expression_prime(void){
	switch (lookahead.code){
	case ART_OP_T:
		switch (lookahead.attribute.arr_op){
		case PLUS:
			match(ART_OP_T, PLUS);
			multiplicative_arithmatic_expression();
			additive_arithmatic_expression_prime();
			break;
		case MINUS:
			match(ART_OP_T, MINUS);
			multiplicative_arithmatic_expression();
			additive_arithmatic_expression_prime();
			break;

		default:
			return;
		}
		break;
	default:
		return;
	}
	gen_incode("PLATY: Additive arithmetic expression parsed\n");
}

/*
* Author : Arin Kumar Poray
*<selection statement> -> IF (<conditional expression>)  THEN  <opt_statements> ELSE { <opt_statements> } ;
*FIRST(<selection statement>) = {KW_T(IF)}
*/
void selection_statement(void){
	match(KW_T, IF);	// required for advancing the lookahead to hold the next token after IF
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	opt_statements();
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: IF statement parsed\n");
}

/*
* Author : Fleming Patel
*<conditional expression> -> <logical OR  expression>
*FIRST(<conditional expression>) = { STR_T, SVID_T, AVID_T, FPL_T, INL_T }
*/
void conditional_expression(void){
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed\n");
}

/*
* Author : Fleming Patel
*<logical  OR expression> -> <logical AND expression><logical OR expression’>
*FIRST(<logical  OR expression>) = { STR_T, SVID_T, AVID_T, FPL_T, INL_T }
*/
void logical_OR_expression(void){

	logical_AND_expression();
	logical_OR_expression_prime();
}

/*
* Author : Fleming Patel
*<logical  AND expression> -> <relational expression><logical AND expression’>
*FIRST(<logical AND expression>) = { STR_T, SVID_T, AVID_T, FPL_T, INL_T }
*/
void logical_AND_expression(void){
	relational_expression();
	logical_AND_expression_prime();
}

/*
* Author : Fleming Patel
*<logical OR expression’> -> .OR.<logical AND expression><logical OR expression’> | E
*FIRST(<logical OR expression’>) = { E, LOG_OP_T(OR) }
*/
void logical_OR_expression_prime(void){

	if (lookahead.code == LOG_OP_T)
	{
		switch (lookahead.attribute.log_op)
		{
		case AND:
			return;
		default:
			break;
		}
		match(lookahead.code, lookahead.attribute.arr_op);
		logical_AND_expression();
		logical_OR_expression_prime();
		gen_incode("PLATY: Logical OR expression parsed\n");
	}
}

/*
* Author : Fleming Patel
*<logical AND expression’> -> .AND.<relational expression><logical AND expression’> | E
*FIRST(<logical AND expression’>) = { E, LOG_OP_T(AND)  }
*/
void logical_AND_expression_prime(void){

	switch (lookahead.code){
	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case AND:
			match(LOG_OP_T, AND);
			relational_expression();
			logical_AND_expression_prime();
			gen_incode("PLATY: Logical AND expression parsed\n");
			break;

		default:
			break;
		}
	default:
		break;
	}
}


/*
* Author : Fleming Patel
*<relational expression> -> <primary a_relational expression><primary a_relational expression tail>| <primary s_relational expression><primary s_relational expression tail>
*FIRST(<relational expression>) = { STR_T, SVID_T, AVID_T, FPL_T, INL_T }
*/
void relational_expression(void){
	switch (lookahead.code){
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		primary_a_relational_expression_prime();
		break;

	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		primary_s_relational_expression_prime();
		break;

	default:
		syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed\n");
}

/*
* Author : Arin Kumar Poray
*<primary a_relational expression> -> AVID_T | FPL_T | INL_T
*FIRST(<primary a_relational expression>) = { AVID_T, FPL_T, INL_T }
*/
void primary_a_relational_expression(void){
	switch (lookahead.code){
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;

	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed\n");
}

/*
* Author : Fleming Patel
*<primary a_relational expression'> -> == <primary a_relational expression>| <> <primary a_relational expression>|  <  <primary a_relational expression>|  >  <primary a_relational expression>
*FIRST(<primary a_relational expression'>) = { REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(LT), REL_OP_T(GT) }
*/
void primary_a_relational_expression_prime(void){
	if (lookahead.code == REL_OP_T){
		switch (lookahead.attribute.rel_op){
		case EQ:
			match(REL_OP_T, EQ);
			primary_a_relational_expression();
			break;
		case NE:
			match(REL_OP_T, NE);
			primary_a_relational_expression();
			break;
		case GT:
			match(REL_OP_T, GT);
			primary_a_relational_expression();
			break;
		case LT:
			match(REL_OP_T, LT);
			primary_a_relational_expression();
			break;

		default:
			syn_printe();
		}

	}
	else
		syn_printe();
}

/*
* Author : Arin Kumar Poray
*<primary s_relational expression> -> <primary string expression>
*FIRST(<primary s_relational expression>) = { STR_T, SVID_T }
*/
void primary_s_relational_expression(void){
	switch (lookahead.code){
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		break;
	}
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed\n");
}

/*
* Author : Arin Kumar Poray
*<primary s_relational expression'> -> == <primary s_relational expression>| <> <primary s_relational expression>|  <  <primary s_relational expression>|  >  <primary s_relational expression>
*FIRST(<primary s_relational expression'>) = { REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(LT), REL_OP_T(GT) }
*/
void primary_s_relational_expression_prime(void){
	if (lookahead.code == REL_OP_T){
		switch (lookahead.attribute.rel_op){
		case EQ:
			match(REL_OP_T, EQ);
			primary_s_relational_expression();
			break;
		case NE:
			match(REL_OP_T, NE);
			primary_s_relational_expression();
			break;
		case GT:
			match(REL_OP_T, GT);
			primary_s_relational_expression();
			break;
		case LT:
			match(REL_OP_T, LT);
			primary_s_relational_expression();
			break;

		default:
			syn_printe();
		}

	}
	else{
		syn_printe();
	}
}

/*
* Author : Arin Kumar Poray
*<iteration statement> -> USING  (<assignment expression> , <conditional expression> , <assignment  expression> )REPEAT {<opt_statements>};
*FIRST(<iteration statement>) = {KW_T(USING)}
*/
void iteration_statement(void){
	match(KW_T, USING);	// required for advancing the lookahead to hold the next token after USING
	match(LPR_T, NO_ATTR);
	assignment_expression();
	match(COM_T, NO_ATTR);
	conditional_expression();
	match(COM_T, NO_ATTR);
	assignment_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: USING statement parsed\n");
}

/*
* Author : Arin Kumar Poray
*<input statement> -> INPUT (<variable list>);
*FIRST(<input statement>) = {KW_T(INPUT)}
*/
void input_statement(void){
	match(KW_T, INPUT); // required for advancing the lookahead to hold the next token after INPUT
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed\n");
}

/*
* Author : Arin Kumar Poray
*<variable list> -> <variable identifier><variable list’>
*FIRST(<variable list>) = {AVID_T, SVID_T}
*/
void variable_list(void){
	variable_identifier();
	variable_list_prime();
	gen_incode("PLATY: Variable list parsed\n");
}

/*
* Author : Arin Kumar Poray
*<variable identifier> ->AVID_T | SVID_T
*FIRST(<variable identifier>) = { AVID_T, SVID_T }
*/
void variable_identifier(void){
	switch (lookahead.code){
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	default:
		syn_printe();
		return;
	}
}

/*
* Author : Arin Kumar Poray
*<variable list’> -> ,<variable identifier><variable list’> | E
*FIRST(<variable list’>) = {E, COM_T}
*/
void variable_list_prime(void){
		switch (lookahead.code){
		case COM_T:
			match(COM_T, NO_ATTR);
			variable_identifier();
			variable_list_prime();
			break;

		default:
			break;
		}
}

/*
* Author : Arin Kumar Poray
*<output statement> -> OUTPUT (<output list>);
*FIRST(<output statement>) = {KW_T(OUTPUT)}
*/
void output_statement(void){
	match(KW_T, OUTPUT);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed\n");
}

/*
* Author : Arin Kumar Poray
*<output list> -> <variable list> | STR_T | E
*FIRST(<output list>) = {E, AVID_T, SVID_T, STR_T}
*/
void output_list(void){
	switch (lookahead.code){
	case AVID_T:
	case SVID_T:
		variable_list();
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed\n");
		break;

	default:
		gen_incode("PLATY: Output list (empty) parsed\n");
		break;
	}
}

/*
* Author : Arin Kumar Poray
*<string expression’> -> # <primary string expression><string expression’> | E
*FIRST(<string expression’>) = { E, SCC_OP_T }
*/
void string_expression(void){
	primary_string_expression();
	string_expression_prime();
}

/*
* Author : Arin Kumar Poray
*<primary string expression> -> SVID_T | STR_T
*FIRST(<primary string expression>) = { SVID_T, STR_T }
*/
void primary_string_expression(void){
	switch (lookahead.code){
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		break;
	}
	gen_incode("PLATY: Primary string expression parsed\n");
}

/*
* Author : Arin Kumar Poray
*<string expression’> -> # <primary string expression><string expression’> | E
*FIRST(<string expression’>) = { E, SCC_OP_T }
*/
void string_expression_prime(void){
	switch (lookahead.code){
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_prime();
		break;
	default:
		gen_incode("PLATY: String expression parsed\n");
	}
}

