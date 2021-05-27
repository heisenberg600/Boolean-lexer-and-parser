structure Typing =
struct

open AST

type typEnv=(id *typ) list 


fun idToString(s:id):string=
    s^""

fun  typEnvLookup (var: id, env: typEnv): typ =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => (print( "The function/variable "^idToString(var)^" isnt defined till now"); raise Fail "")


fun typeEnvAdd (var:id,t:typ,env:typEnv):typEnv =
    (var,t)::env


fun typeToString(t:typ):string= 
    case t of IntTy => "int"
    | BoolTy => "bool"
    | FnTy(t1,t2) => typeToString(t1)^"->"^typeToString(t2)



fun getType (e:exp,env:typEnv):typ =
    case e of 
        NumExp _ => IntTy
        | BoolExp _ => BoolTy
        | VarExp x => typEnvLookup(x,env)
        |AppExp (e1,e2) =>
         (case (getType(e1,env),getType(e2,env)) of (FnTy(t1,t2),t3) =>
                                    if (t1=t3) then t2 else (print( "Aplication argument type mismatch: expected: "^typeToString(t1)^" recieved: "^typeToString(t3)); raise Fail "")
            | (t,_) => (print("Function was expected but recieved "^typeToString(t)); raise Fail ""))
        | FnExp(x,t1,t2,e1) => (
                                let val compt=getType(e1,typeEnvAdd(x,t1,env)) in if t2<> compt then (print( "Mismatch in declared type and actual type: expected: "^typeToString(t2)^" recieved: "^typeToString(compt)); raise Fail "") else FnTy(t1,t2) end )
        |CondExp(e1,e2,e3) =>
            (
                let 
                    val a1=getType(e1,env)
                    val a2=getType(e2,env)
                    val a3=getType(e3,env)
                in 
                    if a1 <> BoolTy then  (print( "Conditon of If command is not of Boolty it is of "^typeToString(a1)); raise Fail "") 
                    else if a2<>a3 then (print( "Branches of if have different types "^typeToString(a2)^" and "^typeToString(a3)); raise Fail "") 
                    else a2
                end
            )
        |FunExp (FunDecl(name,parameter,t1,t2),e1)=>
        (
            let 
                val etype= getType(e1,typeEnvAdd(name,t2,typeEnvAdd(parameter,t1,env)))

            in 
                if etype <> t2 then (print( "Mismatch in declared type and actual type: expected: "^typeToString(t2)^" recieved:"^typeToString(etype)); raise Fail "") else FnTy(t1,t2)
            end
        )
        | UnanExp(a,e1) =>  ( case (a,getType(e1,env)) of (Not,BoolTy) => BoolTy | (Neg,IntTy) => IntTy| (_,t1) => (print( "This function is not defined on type"^typeToString(t1)); raise Fail ""))
        | BinExp(binop,e1,e2) =>(
                let 
                    val a1=getType(e1,env)
                    val a2=getType(e2,env)
                    val a3=binop
                in 
                    if a1 <> a2 then (print( "Binary operation are defined on  same operands types "^typeToString(a1)^" and "^typeToString(a2)^" not same types"); raise Fail "") 
                    else if ((a3=And orelse a3=Xor orelse a3=Implies) andalso a1=IntTy) then (print( "Cant perform boolean operations on intType"); raise Fail "")
                    else if ((a3=Add orelse a3=Sub orelse a3=Mul orelse a3=GrTh orelse a3=SmTh) andalso a1=BoolTy) then (print( "Cant perform int operations on boolType"); raise Fail "")
                    else if ( a3=Eq orelse a3=GrTh orelse a3=SmTh) then BoolTy
                    else a1 
                end
        )
        | LetExp(ValDecl(x1,e1),e2) => ( 
                let 
                    val a1=getType(e1,env)
                in 
                    getType(e2,typeEnvAdd(x1,a1,env))
                end
        )
        | _ => raise Fail "Undefined Expresssion"


fun getProgramTy(e:exp list , env: typEnv):typ list =
    case e of
    [] => []
    | xs::ys => case xs of  FunExp (FunDecl(name,parameter,t1,t2),e1) => getType(xs,env)::getProgramTy(ys,typeEnvAdd(name,FnTy(t1,t2),env))
                | _ => getType(xs,env)::getProgramTy(ys,env)

end