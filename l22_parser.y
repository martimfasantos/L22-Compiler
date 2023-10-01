%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;	/* integer value */
  double                d;    /* double value */
  std::string          *s;	/* symbol name or string literal */
  cdk::basic_node      *node;	/* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;
  std::vector<std::shared_ptr<cdk::basic_type>> *vtype;

  l22::block_node      *block;
};

%token tAND tOR tNOT tGE tLE tEQ tNE tSIZEOF
%token tTYPE_VAR tTYPE_INT tTYPE_REAL tTYPE_STRING tTYPE_VOID
%token tFOREIGN tUSE tPUBLIC tPRIVATE
%token tIF tTHEN tELIF tELSE 
%token tWHILE tDO
%token tSTOP tAGAIN tRETURN

%token tBEGIN tEND
%token tINPUT tWRITE tWRITELN

%token <i> tINTEGER
%token <d> tREAL
%token <s> tID tSTRING tRECUR
%token <expression> tNULL;

%nonassoc tIF tWHILE
%nonassoc tTHEN tDO
%nonassoc tELIF tELSE

%right '='
%left tOR
%left tAND
%right tNOT
%left tNE tEQ
%left '<' tLE tGE '>' 
%left '+' '-'
%left '*' '/' '%'
%right tUMINUS

%type<node> program instr elsif block_instr
%type<sequence> file instrs opt_instrs exprs opt_exprs
%type<expression> expr int real opt_init fundef funcall block_expr
%type<lvalue> lval
%type<block> block

%type<node> argdecl decl
%type<sequence> decls argdecls opt_decls

%type<s> string
%type<type> data_type
%type<vtype> data_types

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : opt_decls              { compiler->ast($$ = $1); }
     | opt_decls program      { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
     ;

program   : tBEGIN block tEND     { $$ = new l22::program_node(LINE, $2); }
	     ;

block     :'{' opt_decls  opt_instrs '}'     { $$ = new l22::block_node(LINE, $2, $3); }
          ;

opt_decls : /* empty */ { $$ = nullptr; }
          | decls        { $$ = $1; }
          ;

decls     : decl                    { $$ = new cdk::sequence_node(LINE, $1);}
          | decl decls              { std::reverse($2->nodes().begin(), $2->nodes().end()); 
                                        $$ = new cdk::sequence_node(LINE, $1, $2); 
                                        std::reverse($$->nodes().begin(), $$->nodes().end());  }
          ;
          
decl      :          data_type tID    opt_init        { $$ = new l22::declaration_node(LINE, tPRIVATE, $1, *$2, $3); delete $2; }
          | tPUBLIC  data_type tID    opt_init        { $$ = new l22::declaration_node(LINE, tPUBLIC,  $2, *$3, $4); delete $3; }
          | tUSE     data_type tID    ';'             { $$ = new l22::declaration_node(LINE, tUSE,  $2, *$3, nullptr); delete $3; }
          | tFOREIGN data_type tID    ';'             { $$ = new l22::declaration_node(LINE, tFOREIGN,  $2, *$3, nullptr); delete $3; }
          | tPUBLIC            tID    opt_init        { $$ = new l22::declaration_node(LINE, tPUBLIC,  nullptr, *$2, $3); delete $2; }
          | tPUBLIC tTYPE_VAR  tID    '=' expr ';'    { $$ = new l22::declaration_node(LINE, tPUBLIC,  nullptr, *$3, $5); delete $3; }
          |         tTYPE_VAR  tID    '=' expr ';'    { $$ = new l22::declaration_node(LINE, tPRIVATE,  nullptr, *$2, $4); delete $2; }
          | tPUBLIC tTYPE_VAR  tID    '=' block_expr  { $$ = new l22::declaration_node(LINE, tPUBLIC,  nullptr, *$3, $5); delete $3; }
          |         tTYPE_VAR  tID    '=' block_expr  { $$ = new l22::declaration_node(LINE, tPRIVATE,  nullptr, *$2, $4); delete $2; }
          ;

data_types : /* empty */                     { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); }
           | data_type                       { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
           | data_types ',' data_type        { $$ = $1; $$->push_back($3); }
          
data_type : tTYPE_STRING                     { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING);  }
          | tTYPE_INT                        { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT);     }
          | tTYPE_REAL                       { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE);  }
          | tTYPE_VOID                       { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID);}
          | '[' data_type ']'                { $$ = cdk::reference_type::create(4, $2); }
          | data_type '<' data_types '>'     { $$ = cdk::functional_type::create(*$3, $1); delete $3;}
          ;

opt_init  : ';'                              { $$ = nullptr;}
          | '=' expr ';'                     { $$ = $2; }
          | '=' block_expr                   { $$ = $2; }
          ;

argdecls : /* empty */             { $$ = new cdk::sequence_node(LINE);  }
         | argdecl                 { $$ = new cdk::sequence_node(LINE, $1);     }
         | argdecls ',' argdecl    { $$ = new cdk::sequence_node(LINE, $3, $1); }
         ;

argdecl  : data_type tID { $$ = new l22::declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); }
         ;

opt_instrs     : /* empty */  { $$ = new cdk::sequence_node(LINE); }
               | instrs       { $$ = $1; }
               ;

instrs    : instr                            { $$ = new cdk::sequence_node(LINE, $1); }
          | block_instr                      { $$ = new cdk::sequence_node(LINE, $1); }
          | instr ';' instrs                 {  std::reverse($3->nodes().begin(), $3->nodes().end()); 
                                                  $$ = new cdk::sequence_node(LINE, $1, $3); 
                                                  std::reverse($$->nodes().begin(), $$->nodes().end());  }
          | block_instr instrs               {  std::reverse($2->nodes().begin(), $2->nodes().end());
                                                  $$ = new cdk::sequence_node(LINE, $1, $2);
                                                  std::reverse($$->nodes().begin(), $$->nodes().end()); }
          ;

instr     : expr                                  { $$ = new l22::evaluation_node(LINE, $1); }
          | tWRITE exprs                          { $$ = new l22::print_node(LINE, $2, false); }
          | tWRITELN exprs                        { $$ = new l22::print_node(LINE, $2, true); }
          | tSTOP                                 { $$ = new l22::stop_node(LINE);  }
          | tAGAIN                                { $$ = new l22::again_node(LINE); }
          | tRETURN                               { $$ = new l22::return_node(LINE, nullptr); }
          | tRETURN expr                          { $$ = new l22::return_node(LINE, $2); }
          ;

block_instr    : tIF '(' expr ')' tTHEN block          { $$ = new l22::if_node(LINE, $3, $6); }
               | tIF '(' expr ')' tTHEN block elsif    { $$ = new l22::if_else_node(LINE, $3, $6, $7); }
               | tWHILE '(' expr ')' tDO block         { $$ = new l22::while_node(LINE, $3, $6); }
               | tRETURN fundef                        { $$ = new l22::return_node(LINE, $2); }
               | lval '=' block_expr                   { $$ = new cdk::assignment_node(LINE, $1, $3); }
               | block                                 { $$ = $1; }
               ;

elsif     : tELSE block                                { $$ = $2; }
          | tELIF '(' expr ')' tTHEN block             { $$ = new l22::if_node(LINE, $3, $6); }
          | tELIF '(' expr ')' tTHEN block elsif       { $$ = new l22::if_else_node(LINE, $3, $6, $7); }
          ;

lval : tID                                        { $$ = new cdk::variable_node(LINE, $1); delete $1; }
     | tRECUR                                     { $$ = new cdk::variable_node(LINE, $1); delete $1; }
     | lval '[' expr ']'                          { $$ = new l22::index_node(LINE, new cdk::rvalue_node(LINE, $1), $3); }
     | '(' expr ')' '[' expr ']'                  { $$ = new l22::index_node(LINE, $2, $5); }
     ;

fundef   : '(' argdecls ')' '-''>' data_type ':' block { $$ = new l22::function_definition_node(LINE, $2, $6, $8); }
         ;

funcall : lval '(' opt_exprs ')'             { $$ = new l22::function_call_node(LINE, new cdk::rvalue_node(LINE, $1), $3); }
        | '(' block_expr ')' '(' opt_exprs ')'     { $$ = new l22::function_call_node(LINE, $2, $5); }
        ;

opt_exprs : /* empty */              { $$ = new cdk::sequence_node(LINE); }
          | exprs                    { $$ = $1; }
          ;

exprs     : expr                     { $$ = new cdk::sequence_node(LINE, $1);     }
          | exprs ',' expr           { $$ = new cdk::sequence_node(LINE, $3, $1); }
          | block_expr                   { $$ = new cdk::sequence_node(LINE, $1);   }
          | exprs ',' block_expr         { $$ = new cdk::sequence_node(LINE, $3, $1); }
          ;


block_expr: fundef                      { $$ = $1; }  
          | lval '=' block_expr         { $$ = new cdk::assignment_node(LINE, $1, $3); }
          ;

expr : int                                   { $$ = $1; }
     | real                                  { $$ = $1; }
     | string                                { $$ = new cdk::string_node(LINE, $1); }
     | tNULL                                 { $$ = new l22::nullptr_node(LINE); }
     /* LEFT VALUES */
     | lval                                  { $$ = new cdk::rvalue_node(LINE, $1); }
     /* ASSIGNMENTS */
     | lval '=' expr                         { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | lval '=' '[' expr ']'                 { $$ = new l22::stack_alloc_node(LINE, $4); }
     /* ARITHMETIC EXPRESSIONS */
     | expr '+' expr	                    { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr	                    { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr	                    { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr	                    { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr	                    { $$ = new cdk::mod_node(LINE, $1, $3); }
     /* LOGICAL EXPRESSIONS */
     | expr '<' expr	                    { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr	                    { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr	                    { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr                         { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr	                    { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr	                    { $$ = new cdk::eq_node(LINE, $1, $3); }
     | expr tAND expr                        { $$ = new cdk::and_node(LINE, $1, $3); }
     | expr tOR expr                         { $$ = new cdk::or_node (LINE, $1, $3); }
     /* UNARY EXPRESSIONS */
     | '-' expr %prec tUMINUS                { $$ = new cdk::neg_node(LINE, $2); }
     | '+' expr %prec tUMINUS                { $$ = new l22::identity_node(LINE, $2); }
     | tNOT expr                             { $$ = new cdk::not_node(LINE, $2); }
     /* FUNCTION EXPRESSIONS */
     | funcall                               { $$ = $1; }
     /* OTHER EXPRESSIONS */
     | tINPUT                                { $$ = new l22::read_node(LINE); }
     | tSIZEOF '(' expr ')'                  { $$ = new l22::sizeof_node(LINE, $3); }
     | '(' expr ')'                          { $$ = $2; }
     | lval '?'                              { $$ = new l22::address_of_node(LINE, $1); }
     ;

int       : tINTEGER                      { $$ = new cdk::integer_node(LINE, $1); };
real      : tREAL                         { $$ = new cdk::double_node(LINE, $1); };
string    : tSTRING                       { $$ = $1; }
          | string tSTRING                { $$ = $1; $$->append(*$2); delete $2; }
          ;

%%