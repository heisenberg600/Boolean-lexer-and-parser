structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (l:int, _) => TextIO.output(TextIO.stdOut, "")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))


  val keywords =
  [
   ("end",  Tokens.END),
   ("in",  Tokens.IN),
   ("let",  Tokens.LET),
   ("PLUS",Tokens.PLUS),
   ("MINUS",Tokens.MINUS),
   ("TIMES",Tokens.TIMES),
   ("NEGATE",Tokens.NEGATE),
   ("EQUALS",Tokens.EQUALS),
   ("LESSTHAN",Tokens.LESSTHAN),
   ("GREATERTHAN",Tokens.GREATERTHAN),
   ("fi",Tokens.FI),
   ("if",Tokens.IF),
   ("then",Tokens.THEN),
   ("else",Tokens.ELSE),
   ("AND",Tokens.AND),
   ("OR",Tokens.OR),
   ("XOR",Tokens.XOR),
   ("IMPLIES",Tokens.IMPLIES),
   ("NOT",Tokens.NOT),
   ("fun",Tokens.FUN),
   ("fn",Tokens.FN),
   ("bool",Tokens.BOOL),
   ("int",Tokens.INT)
   ]

  fun findKeywords (str:string, pos1:pos, pos2:pos) =
  case List.find (fn (s, _) => s = str )  keywords of 
  SOME (_, tk) => tk(pos1, pos2) 
  | NONE => Tokens.ID (str, pos1, pos2)
  %%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));

alpha=[A-Za-z0-9];
digit=[0-9];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !pos, !pos));
"TRUE"  => (Tokens.CONST(true,!pos,!pos));
"FALSE"  => (Tokens.CONST(false,!pos,!pos));
"->"     => (Tokens.ARROW(!pos,!pos));
"=>"     => (Tokens.DARROW(!pos,!pos));
":"     => (Tokens.COLON(!pos,!pos));
{alpha}+ => (findKeywords(yytext,!pos,!pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos)); 
"="      => (Tokens.EQ(!pos,!pos)); 
";"      => (Tokens.TERM(!pos,!pos)); 
.      => (error (!pos,!pos);
             lex());
