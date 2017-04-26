structure Semant :
     sig val transProg : Absyn.exp -> unit end =
struct
  structure A = Absyn

  (* dummy translate for chapter 5 *)
  structure Translate = struct type exp = unit end

  type expty = {exp: Translate.exp, ty: Types.ty}
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table

  fun checkInt ({ty=Types.INT, exp=_}, pos) = ()
  | checkInt ({ty=_,exp=_},pos) = ErrorMsg.error pos "integer required"

  fun transProg (exp:A.exp) : unit =
    let
      val {ty=_, exp=prog} = transExp (Env.base_venv, Env.base_tenv) exp
    in
      prog
    end
  and transExp(venv:venv,tenv:tenv) : A.exp -> expty =
    let fun trexp (A.OpExp{left, oper, right, pos}) =
      (checkInt (trexp left, pos);
       checkInt (trexp right, pos);
       {ty=Types.INT, exp=()})
       (*| trexp (A.VarExp var) = trvar(var)*)
       | trexp (A.IntExp _) = {ty=Types.INT, exp=()}
       | trexp (A.NilExp) = {ty= Types.NIL, exp=()}
       | trexp (A.StringExp (_,_)) = {ty=Types.STRING, exp=()}
       | trexp (A.SeqExp exprs) = trexprs exprs
       (*| trexp (A.AssignExp {var, exp, pos}) =
       | trexp (A.LetExp {decs, body, pos})
       | trexp (A.CallExp {func, args, pos})*)
       | trexp _ = {ty=Types.UNIT, exp=ErrorMsg.error 0 "Can't typecheck this yet"}
     and trexprs nil = {ty=Types.UNIT, exp=()}
        (*| trexprs [(exp, pos)] = trexpr exp*)
        | trexprs ((exp, pos)::exprs) = (trexp exp; trexprs exprs) 

     (*and actual_ty ty = case ty of
                              Types.Name(sym, refty) => actual_ty (valOf (Symbol.look (tenv, sym)))  
                               | final_ty => final_ty
     and trvar (A.SimpleVar(id,pos)) = (case Symbol.look(venv, id) of
                              SOME (Env.VarEntry{ty}) => {exp=(), ty=actual_ty ty}
        | NONE => (ErrorMsg.error pos ("undefined variable: " ^ Symbol.Name id); {exp=(), ty=Types.INT} ))  *)               
    in
      trexp
    end
  and transDec (venv,tenv,dec) =
    (* you should actually do something here *)
      {tenv=tenv,venv=venv}
end