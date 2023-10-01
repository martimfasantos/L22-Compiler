%option c++ prefix="l22_scanner_" outfile="l22_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <vector>
#include <sstream>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "l22_parser.tab.h"

static bool restart_functions = false;

static int lLevelsToPop = 0, lIndex = 0;
static std::vector<int> indents;   // known indentation levels
static std::vector<int> deferred;  // deferred tokens

// output stream for building string literals
static std::ostringstream strlit;

inline bool valid(int indent) {
    lLevelsToPop = lIndex = -1;
    bool indentOk = false;
    for (size_t ix = 0; ix < indents.size(); ix++) {
        if (indent == indents[ix]) {
            lLevelsToPop = indents.size() - ix - 1;
            lIndex = ix;
            indentOk = true;
        }
    }
    return indentOk;
}

// don't change this
#define yyerror LexerError
%}

SPACE     [ ]

%x X_COMMENT
%x X_STRING X_BACKSLASH
%x X_NEWLINE

%%

  yydebug=1;

  {
        //std::cerr << "################" << std::endl;
        set_debug(true);
        if (restart_functions) {
          std::cerr << "POPPING FUNCTIONS (" << indents.size() << ")" << std::endl;
          indents.pop_back();
          if (indents.size() == 0) restart_functions = false;
          return '}';
        }

        else {
          if (lLevelsToPop > 0) {
            std::cerr << "POPPING LEVELS (" << lLevelsToPop << ")" << std::endl;
            lLevelsToPop--;
            indents.pop_back();
            return '}';
          }
        }
        
  }

  /* ====================================================================== */
  /* ====[                      2.2 - COMMENTS                        ]==== */
  /* ====================================================================== */

{SPACE}*"..."\n        ; /* ignore continuation marks */

";".*                ; /* ignore comments */

"(*"                   yy_push_state(X_COMMENT);
  /* TODO AQUI*/
<X_COMMENT>"(*"        yy_push_state(X_COMMENT);
<X_COMMENT>"*)"        yy_pop_state();
<X_COMMENT>.|"\n"      ;  /* ignore comments content */

  /* ====================================================================== */
  /* ====[                      2.2 - INDENTATION                     ]==== */
  /* ====================================================================== */

\n                   yy_push_state(X_NEWLINE); /* delay decision */

<X_NEWLINE>^{SPACE}+  {
    yy_pop_state();

    if (valid(yyleng)) {
          std::cerr << "CASE 1" << std::endl;
          // inside the block
          if (lLevelsToPop == 0) {
            std::cerr << "CASE 1a (levels to pop = 0)" << std::endl;
            return ';';
          }
          else {
            std::cerr << "DEDENT (levels to pop = " << lLevelsToPop << ")" << std::endl;
            if (lLevelsToPop > 0) {
              std::cerr << "POPPING LEVELS (" << lLevelsToPop << ")" << std::endl;
              lLevelsToPop--;
              indents.pop_back();
              return '}';
            }
          }
    }
    // first block to open
    else if (indents.size() == 0) {
          std::cerr << "FIRST BLOCK" << std::endl;
          indents.push_back(yyleng);
          return '{';
    }

    else if (yyleng > indents.back()) {
          std::cerr << "BLOCK INSIDE BLOCK" << std::endl;
          indents.push_back(yyleng);
          return '{';
    }

    // indent smaller than all previous ondes - impossible
    else {
          std::cerr << "bad indent at line " << yylineno;
    }

}
<X_NEWLINE>\n      ; 
<X_NEWLINE>.       {
        yyless(0);
        yy_pop_state();
        if (indents.size() > 0) {
          std::cerr << "should restart " << indents.size() << " functions at " << yylineno << std::endl;
          indents.pop_back();
          if (indents.size() > 0) { restart_functions = true; }
          return '}';
        }
        else return ';'; // top-level declarations
}

  /* ====================================================================== */
  /* ====[                  3.5   - Tipos de dados                    ]==== */
  /* ====[                  3.9.1 - Null pointers                     ]==== */
  /* ====================================================================== */

"var"                  return tTYPE_VAR;
"int"                  return tTYPE_INT;
"double"               return tTYPE_REAL;
"text"                 return tTYPE_STRING;
"void"                 return tTYPE_VOID;
"null"                 return tNULL;
 /* retorno de função, indexação e reserva de memória */
[<>\[\]]               return *yytext;

  /* ====================================================================== */
  /* ====[                  3.4.2 - Símbolos globais                  ]==== */
  /* ====================================================================== */

"use"                  return tUSE;
"public"               return tPUBLIC;
"foreign"              return tFOREIGN;

  /* ====================================================================== */
  /* ====[                 6.2 - Instrução condicional                ]==== */
  /* ====================================================================== */

"if"                   return tIF;
"then:"                return tTHEN;
"elif"                 return tELIF;
"else:"                return tELSE;


  /* ====================================================================== */
  /* ====[                 6.3 - Instrução de iteração                ]==== */
  /* ====[                 6.4 - Instrução de terminação              ]==== */
  /* ====[                 6.5 - Instrução de continuação             ]==== */
  /* ====================================================================== */

"while"                return tWHILE;
"do:"                  return tDO;
"stop"                 return tSTOP;
"again"                return tAGAIN;

  /* ====================================================================== */
  /* ====[                 7 - Operadores de expressões               ]==== */
  /* ====================================================================== */

  /* soma, sub, mul, div, mod, recursão, parenteses e virgulas */
[-+*/%?=(),<>]         return *yytext;

"=="                   return tEQ;
"!="                   return tNE;
"<="                   return tLE;
">="                   return tGE;
"and"                  return tAND;
"or"                   return tOR;
"not"                  return tNOT;

  /* ====================================================================== */
  /* ====[              3   - Delimitadores e separadores             ]==== */
  /* ====[              4.3 - Corpo (de função)                       ]==== */
  /* ====[              5.3 - Instrução de retorno                    ]==== */
  /* ====================================================================== */

[#():!,]              return *yytext;
"return"              return tRETURN;
"write"               return tWRITE;
"writeln"             return tWRITELN;
"input"               return tINPUT;
"sizeof"              return tSIZEOF;
"begin"               return tBEGIN;
"end"                 return tEND;

  /* ====================================================================== */
  /* ====[                   3.9.1 - Inteiros                         ]==== */
  /* ====================================================================== */

  /* TODO AQUI */
0[0-6]+              { try {
                      yylval.i = strtoul(yytext, NULL, 7);
                      return tINTEGER;
                      } catch (std::out_of_range& e) {
                        yyerror("Integer out of range.");
                      }
                     }

0[0-9]+              yyerror("Invalid base 7 number.");

0|[1-9][0-9]*        { try {
                      yylval.i = strtoul(yytext, NULL, 10);
                      return tINTEGER;
                      } catch (std::out_of_range& e) {
                        yyerror("Integer out of range.");
                      }
                     }

  /* ====================================================================== */
  /* ====[                   3.9.2 - Reais                            ]==== */
  /* ====================================================================== */
  /* TODO AQUI*/
([0-9]*\.[0-9]+)([Ee]([-+])?[0-9]+)? { try {
                                        yylval.d = strtod(yytext, NULL);
                                        return tREAL;
                                       } catch (std::out_of_range& e) {
                                        yyerror("Real out of range.");
                                       }
                                     }

([0-9]+\.[0-9]*)([Ee]([-+])?[0-9]+)? { try {
                                        yylval.d = strtod(yytext, NULL);
                                        return tREAL;
                                       } catch (std::out_of_range& e) {
                                        yyerror("Real out of range.");
                                       }
                                     }

([0-9]+)([Ee]([-+])?[0-9]+)?         { try {
                                        yylval.d = strtod(yytext, NULL);
                                        return tREAL;
                                       } catch (std::out_of_range& e) {
                                        yyerror("Real out of range.");
                                       }
                                     }

  /* ====================================================================== */
  /* ====[                3.8 - Identificadores (nomes)               ]==== */
  /* ====================================================================== */
  /* TODO AQUI*/
[A-Za-z][A-Za-z0-9_]*            yylval.s = new std::string(yytext); return tID;
"@"                              yylval.s = new std::string(yytext); return tRECUR;

 /* ====================================================================== */
  /* ====[              3.9.3 - Cadeias de caracteres                 ]==== */
  /* ====================================================================== */

"\""                    yy_push_state(X_STRING);
<X_STRING>\\            yy_push_state(X_BACKSLASH);
<X_STRING>"\""         {
                         yylval.s = new std::string(strlit.str());
                         strlit.str("");
                         yy_pop_state();
                         return tSTRING;
                       }
<X_STRING>.            strlit << *yytext;

<X_BACKSLASH>n         strlit << '\n'; yy_pop_state();
<X_BACKSLASH>r         strlit << '\r'; yy_pop_state();
<X_BACKSLASH>t         strlit << '\t'; yy_pop_state();
<X_BACKSLASH>"\""      strlit << '"' ; yy_pop_state();
<X_BACKSLASH>\\        strlit << '\\'; yy_pop_state();

  /* TODO não tenho a certeza disso do 255 mas disseram-me de fonte segura */
<X_BACKSLASH>[0-6]{1,3} {
                          unsigned long int char_size = strtoul(yytext, NULL, 7);
                          if (char_size > 255) {
                            yyerror("Base 7 character too big");
                          }
                          else {
                            strlit << (char)(unsigned char)char_size;
                            yy_pop_state();
                          }
                        }
<X_BACKSLASH>.                 strlit << *yytext; yy_pop_state();


  /* ====================================================================== */
  /* ====[                    (other characters)                      ]==== */
  /* ====================================================================== */

<<EOF>>             return 0;

[[:space:]]         /* ignore remaining white space inside the program */
.                   yyerror(yytext);    /* all other chars: error! */

  /* ====================================================================== */
  /* ====[                           THE END                          ]==== */
  /* ====================================================================== */

%%