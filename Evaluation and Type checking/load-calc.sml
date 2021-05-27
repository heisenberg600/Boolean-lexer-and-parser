structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);
structure CalcParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = CalcLrVals.ParserData
     	       structure Lex = CalcLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    CalcParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = CalcParser.Stream.get lexer
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer


fun LexerFrFile f =
    let val inStream = TextIO.openIn f
		val str = TextIO.inputAll inStream 
        val _ = TextIO.closeIn inStream
        val done = ref false
        val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in 
        lexer
    end

val parseIt = parse o LexerFrFile

open EVALUATOR
open Typing

fun typeChecking1(explist)=
		getProgramTy(explist,[])

fun evalExpList1(explist)=
		evalProgram(explist,[])


val typeChecking = typeChecking1 o parseIt
val evalExpList = evalExpList1 o parseIt


