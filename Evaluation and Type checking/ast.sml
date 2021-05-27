structure AST =
struct

type id = string

datatype binop = Add | Sub | Mul | Eq | GrTh | SmTh | And | Or | Xor | Implies

datatype unanop = Not | Neg

datatype typ = IntTy | BoolTy | FnTy of typ*typ


datatype decl = ValDecl of id * exp 
				| FunDecl of id*id*typ*typ

and exp = NumExp of int
    	| VarExp of id
	| BinExp of binop * exp * exp
	| UnanExp of unanop * exp
	| LetExp of decl * exp
        | BoolExp of bool
		| CondExp of exp*exp*exp
		| AppExp of exp*exp
		| FnExp of id*typ*typ*exp
		| FunExp of decl*exp


datatype value = IntVal of int
	       | BoolVal of bool
		   | FnVal of id*exp* ((id * value) list ref)
		   				
type environment = (id * value) list 
fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env


fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"							    
end


