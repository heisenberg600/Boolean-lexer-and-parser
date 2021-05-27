structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"

fun evalExp(e:exp, env:environment):value =
    case e of
	NumExp i            => IntVal i
	| BoolExp b            => BoolVal b
      | VarExp x            => envLookup (x, env) 				  
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | UnanExp (u, e1)  => evalUnanExp(u, e1,env)
      | CondExp (e1,e2,e3) => evalCond(e1,e2,e3,env)
      | LetExp(ValDecl(x, e1), e2)  =>
  	    let
	        val v1 = evalExp (e1, env)
	      in
	        evalExp(e2, envAdd(x,v1,env))
        end	
      | FnExp(x1,_,_,e1) => FnVal(x1,e1,ref env)
      | AppExp(e1,e2) =>
        (
          case evalExp(e1,env) of 
          FnVal (x1,body,enclosingScope) =>(
            let val v1 = evalExp(e2,env)
            in evalExp(body,envAdd(x1,v1,!enclosingScope)) end
          )
          | _ => raise brokenTypes
        )	
      | FunExp(FunDecl(fncname,x1,_,_),body) =>  let val initialEnv= ref [] 
                                                  val  enclosingScope =envAdd(fncname,FnVal(x1,body,initialEnv), env)
                                                  val ()=initialEnv := enclosingScope
                                                  in  FnVal(x1,body,initialEnv)
                                                  end
and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
case (b, evalExp(e1, env), evalExp(e2, env))  of
    (Add, IntVal i1, IntVal i2) => IntVal (i1+i2)
  |   (Sub, IntVal i1, IntVal i2) => IntVal (i1-i2)
  |   (Mul, IntVal i1, IntVal i2) => IntVal (i1*i2)
  |   (Eq, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
  |   (Eq, BoolVal i1, BoolVal i2)  => BoolVal (i1 = i2)
  |   (SmTh, IntVal i1, IntVal i2)  => BoolVal (i1 < i2)
  |   (GrTh, IntVal i1, IntVal i2)  => BoolVal (i1 > i2)
  |   (And, BoolVal i1, BoolVal i2)  => BoolVal (i1 andalso i2)
  |   (Or, BoolVal i1, BoolVal i2)  => BoolVal (i1 orelse i2)
  |   (Xor, BoolVal i1, BoolVal i2)  => BoolVal ((i1 orelse i2) andalso ( ( not i1 ) orelse ( not i2 )))
  |   (Implies, BoolVal i1, BoolVal i2)  => BoolVal ((not i1) orelse i2)
  |   _  => raise brokenTypes  
and   		
evalUnanExp(u:unanop, e1:exp, env:environment):value =
case (u, evalExp(e1, env))  of
    (Neg, IntVal i1) => IntVal (~1*i1)
  |   (Not, BoolVal i1) => BoolVal (not i1)
  |   _  => raise brokenTypes  			
and

evalCond(e1:exp,e2:exp,e3:exp, env:environment):value=

case (evalExp(e1, env))  of	
   (BoolVal true) => evalExp(e2,env)
  | (BoolVal false) => evalExp(e3,env)
  |   _  => raise brokenTypes



fun evalProgram(e:exp list , env: environment):value list =
    case e of
    [] => []
    | xs::ys => case xs of FunExp (FunDecl(name,parameter,t1,t2),e1) => evalExp(xs,env)::evalProgram(ys,envAdd(name, evalExp(xs,env),env))
            | _  =>evalExp(xs,env)::evalProgram(ys,env)
end
