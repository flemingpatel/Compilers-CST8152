#ifndef PARSER_H_
#define PARSER_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef STABLE_H_
#include "stable.h"
#endif


/* user-defined constants */
#define NO_ATTR -1

#define ELSE 0
#define IF 1
#define INPUT 2
#define OUTPUT 3
#define PLATYPUS 4
#define REPEAT 5
#define THEN 6
#define USING 7

#define PLUS 0
#define MINUS 1
#define MULT 2
#define DIV 3

#define EQ 0
#define NE 1
#define GT 2
#define LT 3

#define AND 0
#define OR 1

Token lookahead;
Buffer* sc_buf;
int synerrno;


/*external objects */
extern Buffer * str_LTBL; /*String literal table */
extern int line;
extern STD sym_table;
extern Token mlwpar_next_token(Buffer*);
extern char * kw_table[];


/* function declarations */
void parser(Buffer*);
void match(int, int);
void syn_eh(int);
void syn_printe();
void gen_incode(char*);
void program(void);
void opt_statements(void);
void statements(void);
void statement(void);
void statements_prime(void);
void assignment_statement(void);
void assignment_expression(void);
void arithmatic_expression(void);
void unary_arithmatic_expression(void);
void primary_arithmatic_expression(void);
void additive_arithmatic_expression(void);
void multiplicative_arithmatic_expression(void);
void multiplicative_arithmatic_expression_prime(void);
void additive_arithmatic_expression_prime(void);
void string_expression(void);
void selection_statement(void);
void iteration_statement(void);
void input_statement(void);
void variable_list(void);
void variable_identifier(void);
void variable_list_prime(void);
void output_statement(void);
void output_list(void);
void input_statement(void);
void conditional_list(void);
void logical_OR_expression(void);
void logical_AND_expression(void);
void logical_OR_expression_prime(void);
void logical_AND_expression_prime(void);
void relational_expression(void);
void primary_a_relational_expression(void);
void primary_a_relational_expression_prime(void);
void primary_s_relational_expression(void);
void primary_s_relational_expression_prime(void);
void primary_string_expression(void);
void string_expression_prime(void);
void conditional_expression(void);

#endif
