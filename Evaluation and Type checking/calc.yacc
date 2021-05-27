(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

%%
(* required declarations *)
%name Calc

%term
  ID of string | NUM of int | STR of string
| PLUS | TIMES | MINUS | NEGATE | RPAREN | LPAREN | EOF
| EQUALS | LET | IN | END | FI | LESSTHAN | GREATERTHAN | EQ | TERM | IF | THEN | ELSE | IMPLIES | AND | OR | XOR | NOT | CONST of bool | COLON | ARROW | DARROW | INT | BOOL | FN | FUN

%nonterm EXP of AST.exp | START of AST.exp list | program of AST.exp list | DECL of AST.decl | Type of AST.typ

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%right ARROW
%nonassoc DARROW

%right EQ  
%right IMPLIES IF THEN ELSE
%left AND OR XOR EQUALS
%right NOT
%left GREATERTHAN LESSTHAN
%left MINUS PLUS
%left TIMES 
%right NEGATE
%right LPAREN
%left RPAREN
(* %right *)
%start START

%verbose

%%

  START: program (program)

  program: EXP ([EXP]) | EXP TERM program (EXP::program)

  DECL: ID EQ EXP  (AST.ValDecl(ID, EXP))
      | FUN ID LPAREN ID COLON Type RPAREN COLON Type (AST.FunDecl(ID1,ID2,Type1,Type2))

  EXP: NUM (AST.NumExp(NUM))
  | ID (AST.VarExp(ID))
  | CONST (AST.BoolExp(CONST))
  | FN LPAREN ID COLON Type RPAREN COLON Type DARROW EXP (AST.FnExp(ID, Type1,Type2,EXP))
  | DECL DARROW EXP(AST.FunExp(DECL,EXP))
  | LPAREN EXP EXP RPAREN (AST.AppExp(EXP1,EXP2))
  | EXP PLUS EXP (AST.BinExp(AST.Add, EXP1,  EXP2))
  | EXP MINUS  EXP (AST.BinExp(AST.Sub,  EXP1,  EXP2))
  | EXP TIMES  EXP (AST.BinExp(AST.Mul,  EXP1, EXP2))
  | EXP GREATERTHAN  EXP (AST.BinExp(AST.GrTh,  EXP1, EXP2))
  | EXP LESSTHAN  EXP (AST.BinExp(AST.SmTh,  EXP1, EXP2))
  | EXP EQUALS EXP (AST.BinExp(AST.Eq ,  EXP1, EXP2))
  | EXP IMPLIES EXP (AST.BinExp(AST.Implies ,  EXP1, EXP2))
  | EXP OR EXP (AST.BinExp(AST.Or ,  EXP1, EXP2))
  | EXP AND EXP (AST.BinExp(AST.And ,  EXP1, EXP2))
  | EXP XOR EXP (AST.BinExp(AST.Xor ,  EXP1, EXP2))
  | NOT EXP (AST.UnanExp(AST.Not, EXP))
  | NEGATE EXP (AST.UnanExp(AST.Neg, EXP))
  | LET DECL IN EXP END (AST.LetExp(DECL, EXP))
  | LPAREN EXP RPAREN (EXP)
  | IF EXP THEN EXP ELSE EXP FI (AST.CondExp(EXP1,EXP2,EXP3))

Type:
    Type ARROW Type (AST.FnTy(Type1,Type2))
    | INT (AST.IntTy)
    | BOOL (AST.BoolTy)
    | LPAREN Type RPAREN (Type)