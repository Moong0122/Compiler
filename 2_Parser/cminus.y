/****************************************************/
/* File: tiny.y                                     */
/* The TINY Yacc/Bison specification file           */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/
%{
#define YYPARSER /* distinguishes Yacc output from other code files */

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

#define YYSTYPE TreeNode *
static char * savedName; /* for use in assignments */
//static int savedLineNo;  /* ditto */
static TreeNode * savedTree; /* stores syntax tree for later return */
static int yylex(void);
int yyerror(char * message);
static int savedSize;
static char * savedType;
int lineStack[10];
int lineStackIndex = 0;
int lineStackPop();


%}

%token IF ELSE INT RETURN VOID
%token ID NUM 
%token ASSIGN EQ LT LE NE GT GE COMMA SEMI PLUS MINUS TIMES OVER LBRACE RBRACE LCURLY RCURLY LPAREN RPAREN
%token ERROR 
%token THEN END WHILE REPEAT UNTIL READ WRITE 

%% /* Grammar for TINY */

program     : decl_list
                 { savedTree = $1;} 
            ;
decl_list    : decl_list decl
                 { YYSTYPE t = $1;
                   if (t != NULL)
                   { while (t->sibling != NULL)
                        t = t->sibling;
                     t->sibling = $2;
                     $$ = $1; }
                     else $$ = $2;
                 }
            | decl  { $$ = $1; } 
            ;
decl        : var_decl
                 { $$ = $1; }
            | fun_decl
                 { $$ = $1; }
            ;
var_decl    : type_spec vars SEMI
               { $$=$2; }               
            ;
vars        : vars COMMA var_check
                { YYSTYPE t = $1;
                   if (t != NULL)
                   { while (t->sibling != NULL)
                        t = t->sibling;
                     t->sibling = $3;
                     $$ = $1; }
                     else $$ = $3;
                 }
            | var_check
                { $$ = $1; }
            ;

var_check   : id 
                { $$ = newStmtNode(ValDecK);
                  $$->attr.name = savedName;
                  $$->attr.type = savedType;
                  $$->lineno = lineStackPop();
                }
            | id LBRACE size RBRACE
                { $$ = newStmtNode(arrValDecK);
                  $$->attr.name = savedName;
                  $$->attr.val = savedSize;
                  $$->attr.type = savedType;
                  $$->lineno = lineStackPop();
                }
            ;

var         : id { $$ = newExpNode(IdK);
                   $$->attr.name = savedName;
                   $$->lineno = lineStackPop(); }
            | id { $$ = newExpNode(arrIdK);
                   $$->attr.name = savedName;
                   }
                LBRACE simple_exp RBRACE
               { $$ = $2;
                 $$->child[0] = $4;
                 $$->lineno = lineStackPop();
               }
            ;

id          : ID 
                {savedName = copyString(tokenString);
                 }
            ;

size        : NUM
                {savedSize = atoi(tokenString);
                }
            ;

type_spec   : INT
                { savedType = copyString(tokenString);}
            | VOID 
                { savedType = copyString(tokenString);}
            ;

params      : param_list | param_empty
            ;

param_list  : param_list COMMA param
                { YYSTYPE t = $1;
                        if (t != NULL)
                        { while (t->sibling != NULL)
                                t = t->sibling;
                        t->sibling = $3;
                        $$ = $1; }
                        else $$ = $3;
                }
            | param 
               { $$ = $1; }
            ;

param_empty : VOID 
               { $$ = newStmtNode(ParamK);
                 $$->attr.name = "(null)";
                 $$->attr.type = "void";
                 $$->lineno = lineno;
               }
            |  { $$ = newStmtNode(ParamK);
                 $$->attr.name = "(null)";
                 $$->attr.type = "void";
                 $$->lineno = lineno;
               }
            ;

param       : type_spec id
               { $$ = newStmtNode(ParamK);
                 $$->attr.name = savedName;
                 $$->attr.type = savedType;
                 $$->lineno = lineStackPop();
               }
            | type_spec id LBRACE RBRACE
               { $$ = newStmtNode(arrParamK);
                 $$->attr.name = savedName;
                 $$->attr.type = savedType;
                 $$->lineno = lineStackPop();
               }
            ;

fun_decl    : type_spec id { $$ = newStmtNode(FunK);
                            $$->attr.name = savedName;
                            $$->attr.type = savedType;}
                LPAREN params RPAREN compound
                { 
                  $$ = $3;
                  $$->lineno = lineStackPop();
                  $$->child[0] = $5;
                  $$->child[1] = $7;
                }
            ;

local_decl  : local_decl var_decl
                { YYSTYPE t = $1;
                    if (t != NULL)
                    { while (t->sibling != NULL)
                        t = t->sibling;
                        t->sibling = $2;
                        $$ = $1; }
                    else $$ = $2;
                }
            | { $$ = NULL; }
            ;

stmt        : exp_stmt { $$ = $1; }
            | compound { $$ = $1; }
            | if_stmt { $$ = $1; }
            | while_stmt { $$ = $1; }
            | return_stmt { $$ = $1; }
            | error { $$ = NULL; }
            ;

stmt_list   : stmt_list stmt 
               { YYSTYPE t = $1;
                    if (t != NULL)
                    { while (t->sibling != NULL)
                        t = t->sibling;
                        t->sibling = $2;
                        $$ = $1; }
                    else $$ = $2;
                }
            | { $$ = NULL; }
            ;

exp_stmt    : exp SEMI
               { $$ = $1; }
            | SEMI
               { $$ = NULL; }
            ;

if_stmt     : IF LPAREN exp RPAREN stmt
                 { $$ = newStmtNode(IfK);
                   $$->child[0] = $3;
                   $$->child[1] = $5;
                   $$->lineno = $3->lineno;
                 }
            | IF LPAREN exp RPAREN stmt ELSE stmt
                 { $$ = newStmtNode(IfK);
                   $$->child[0] = $3;
                   $$->child[1] = $5;
                   $$->child[2] = $7;
                   $$->lineno = $3->lineno;
                 }
            ;

while_stmt  : WHILE LPAREN exp RPAREN stmt
                 { $$ = newStmtNode(WhileK);
                   $$->child[0] = $3;
                   $$->child[1] = $5;
                   $$->lineno = $3->lineno;
                 }
            ;

return_stmt : RETURN SEMI
                { $$ = newStmtNode(ReturnK);
                  }
            | RETURN exp SEMI
                { $$ = newStmtNode(ReturnK);
                  $$->child[0] = $2;
                  $$->lineno = $2->lineno;
                }
            ;

compound    : LCURLY local_decl stmt_list RCURLY
               { $$ = newStmtNode(CompK);
                 $$->child[0] = $2;
                 $$->child[1] = $3;
                }
            ;

exp         : var { $$ = newStmtNode(AssignK);
                    $$->attr.name = savedName; }
              ASSIGN exp
                 { 
                   $$ = $2;
                   $$->child[0] = $1;
                   $$->child[1] = $4;
                   $$->lineno = $1->lineno;
                 }
            | simple_exp EQ simple_exp
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = EQ;
                   $$->lineno = $1->lineno;
                 }
            | simple_exp NE simple_exp
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = NE;
                   $$->lineno = $1->lineno;
                 }
            | simple_exp LT simple_exp 
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = LT;
                   $$->lineno = $1->lineno;
                 }
            | simple_exp LE simple_exp
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = LE;
                   $$->lineno = $1->lineno;
                 }
            | simple_exp GT simple_exp
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = GT;
                   $$->lineno = $1->lineno;
                 }
            | simple_exp GE simple_exp
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = GE;
                   $$->lineno = $1->lineno;
                 }
            | simple_exp { $$ = $1; }
            ;

simple_exp  : simple_exp PLUS term 
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = PLUS;
                   $$->lineno = $1->lineno;
                 }
            | simple_exp MINUS term
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = MINUS;
                   $$->lineno = $1->lineno;
                 } 
            | term { $$ = $1; }
            ;

call        : id {$$ = newExpNode(CallK);
                  $$->attr.name = savedName; }
                LPAREN args RPAREN
                { $$ = $2;
                  $$->child[0] = $4;
                  $$->lineno = lineStackPop();
                }
            ;

term        : term TIMES factor 
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = TIMES;
                   $$->lineno = $1->lineno;
                 }
            | term OVER factor
                 { $$ = newExpNode(OpK);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                   $$->attr.op = OVER;
                   $$->lineno = $1->lineno;
                 }
            | factor { $$ = $1; }
            ;

factor      : LPAREN exp RPAREN
                 { $$ = $2; }
            | NUM
                 { $$ = newExpNode(ConstK);
                   $$->attr.val = atoi(tokenString);
                   $$->lineno = lineno;
                 }
            | var
            | call
                 { $$ = $1; }
            | error { $$ = NULL; }
            ;

args        : arg_list 
                { $$ = $1;}
            | { $$ = NULL; }
            ;

arg_list    : arg_list COMMA exp
                { YYSTYPE t = $1;
                  if (t != NULL)
                      { while (t->sibling != NULL)
                            t = t->sibling;
                      t->sibling = $3;
                      $$ = $1; }
                  else $$ = $3;
                }
            | exp
                { $$ = $1; }
            ;

/*
repeat_stmt : REPEAT stmt_seq UNTIL exp
                 { $$ = newStmtNode(RepeatK);
                   $$->child[0] = $2;
                   $$->child[1] = $4;
                 }
            ;
assign_stmt : id { $$ = newStmtNode(AssignK);
                   $$->attr.name = savedName; }
              ASSIGN exp SEMI
                 { 
                   $$ = $2;
                   $$->child[0] = $4;
                   $$->lineno = savedLineNo;
                 }
            ;
read_stmt   : READ ID
                 { $$ = newStmtNode(ReadK);
                   $$->attr.name =
                     copyString(tokenString);
                 }
            ;
write_stmt  : WRITE exp
                 { $$ = newStmtNode(WriteK);
                   $$->child[0] = $2;
                 }
            ; */

%%

int yyerror(char * message)
{ fprintf(listing,"Syntax error at line %d: %s\n",lineno,message);
  fprintf(listing,"Current token: ");
  printToken(yychar,tokenString);
  Error = TRUE;
  return 0;
}

/* yylex calls getToken to make Yacc/Bison output
 * compatible with ealier versions of the TINY scanner
 */
static int yylex(void)
{ TokenType token = getToken();
  if(token == ID)
  { lineStackPush(lineno);}
  return token;
}

TreeNode * parse(void)
{ yyparse();
  return savedTree;
}

void lineStackPush(int line)
{ lineStack[lineStackIndex++] = line; }

int lineStackPop()
{ return lineStack[--lineStackIndex]; }

