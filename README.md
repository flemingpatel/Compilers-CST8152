# PLATYPUS Compiler Project.

Introduction During the course of a semester at Algonquin college in the CST8152 Compilers class, I built the front end of a compiler to process a custom language designed by our professor, Svillen Ranev. This was a project in which I learnt about and applied knowledge associated with:

1.) Describe the nature of a programming language and the process of converting a program written in a highlevel
    programming language into executable code.
  - identify the theoretical and practical differences between existing programming paradigms and
    programming languages;
  - describe the function and the operation of editors, preprocessors, code generators, code optimizers,
    librarians, linkers, and other compiler supporting tools;
  - describe the function and the operation of the building blocks of Integrated Development Environments;
  - explain the how and when different compiler outputs and run-time environments are used. Detail the
    difference between compiler and interpreter.
    
2.) Explain the process of compiling a computer program and the parts of a compiler.
  - identify the main building blocks of a compiler and an interpreter;
  - explain the role of the lexical analyzer (scanner), syntax analyzer (parser), code optimizer, and code
    generator.
    
3.) Differentiate the process of and the tools for lexical analysis, parsing, error handling, and interpretation.
  - explain the main characteristics of a programming language and how it relates to a natural language;
  - identify and describe the different techniques and tools used for lexical, syntactical, and semantic analysis
    of a program written in specific language.
    
4.) Understand regular expressions and basic programming and scripting language grammars.
  - identify the role of the descriptive notations like regular expressions and grammars to describe a
    programming or scripting language;
  - use regular expressions to define the lexical part of a programming language;
  - use BNF grammar to define the syntactical part of a programming language;
  - implement a lexical analyzer (scanner) based on regular expressions;
  - use syntax-directed translation approach to implement a syntax analyzer (parser) based on a grammar
    description.
    
5.) Build and manage a mid-size programming project.
  - use the C programming language to write a complex program implementing the front end of a compiler;
  - use different compiler platforms to write effective, compact androbust multi-file programs;
  - coordinate properly the compilation and linking of multiple files containing local, static and global data
    structures and functions;
  - build code for modification and re-use applying modularity and defensive rogramming.
  - select and use appropriate tools and technology for building a project.

## Here you will find implementation of compiler components.

### Assignemnt 1: Buffer
    I was tasked to implement a buffer that can operate in three different modes: a “fixedsize”
    buffer, an “additive self-incrementing” buffer, and a “multiplicative self-incrementing” buffer.
    The buffer implementation was based on two associated data structures: a Buffer Descriptor (or
    Buffer Handle) and an array of characters (the actual character buffer). Both structures were
    created “on demand” at run time and also they were allocated dynamically. The Buffer
    Descriptor or Buffer Handle - the names suggest the purpose of this buffer control data structure -
    contains all the necessary information about the array of characters: a pointer to the beginning of
    the character array location in memory, the current size, the next character entry position, the
    increment factor, the operational mode and some additional parameters.
    
### Implemented and compilation source files are:
    - buffer.c
    - buffer.h
    - platy_st.c
 
### Assignemnt 2: Scanner
    I was tasked to build Lexical Analyzer (Scanner). With the help of buffer utility functions, 
    the scanner produces a stream of token from streams of character which already loaded in buffer and 
    produced an output file with specific print statement. I build a finite state machine for recognizing 
    only identifiers, keywords and literals. The transitions from state to state being governed by the 
    Transition table which is in table.h header file.
    
    In Scanner you will find PLATYPUS Language Specification and Informal Language Specification
    
### Implemented and compilation source files are:
    - buffer.c
    - buffer.h
    - scanner.c
    - table.h
    - token.h
    - platy_st.c
 
### Assignment 3: Symbol Table
    I was tasked to create a Symbol table component in PLATYPUS compiler. The Symbol table component 
    consists of two parts: (1) A Symbol Table Manager (STM) and (2) A Symbol Table Database (STDB).
    The STM provides utilities (service functions) for manipulation of the STBD. The STDB is a repository 
    for VID attributes. Each variable identifier is associated with one record in the database. 
    Five VID attributes will be defined in the symbol table: variable name, type, initial value, line number,
    and one reserved attribute. To capture this information scanner code was slightly modified. 
    When scanner identified a token and if it would be AVID or SVID then scanner stores in the symbol table 
    only four of those attributes for that VID.
    
### Implemented and compilation source files are:
    - buffer.c
    - buffer.h
    - scanner.c
    - table.h
    - token.h
    - stable.c
    - stable.h
    - platy_tt.c
    
### Assignment 4: Parser

    In Parser you will find PLATYPUS Language grammar transformed into Recursive Descent Predictive Parsing (LL grammar).

### Author

### Fleming Patel
