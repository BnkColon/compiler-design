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
       | trexp (A.IntExp _) = {ty=Types.INT, exp=()}
       | trexp (A.StringExp (_,_)) = {ty=Types.STRING, exp=()}
       | trexp (A.NilExp) = {ty= Types.NIL, exp=()}
       | trexp (A.VarExp var) = trvar(var)
       | trexp (A.SeqExp exprs) = trexprs exprs
       | trexp (A.AssignExp {var, exp, pos}) = 
            let
              val {exp=left,  ty=expect} = trvar (var)
              val {exp=right, ty=actual} = trexp (exp)
            in
              if expect <> actual then
                (ErrorMsg.error pos "assignment mismatch"; {exp=(), ty=Types.UNIT})
              else
                {exp=(), ty=Types.UNIT}
            end
            
       (*| trexp (A.LetExp {decs, body, pos}) =
          let
            val (venv, tenv, decList, _) =
              foldl (fn (dec, (v, t, e, l)) => transDec(v, t, dec, break, e, l))
                (venv, tenv, [], level) decs
            val {exp=(), ty=bodyTy} = transExp (venv,tenv, break, level) body
          in
            {exp=(), ty=bodyTy}
          end*)
       | trexp _ = {ty=Types.UNIT, exp=ErrorMsg.error 0 "Can't typecheck this yet"}
     
     and trexprs nil = {ty=Types.UNIT, exp=()}
        (*| trexprs [(exp, pos)] = trexpr exp*)
        | trexprs ((exp, pos)::exprs) = (trexp exp; trexprs exprs)

     and actual_ty ty = case ty of
                      Types.NAME(sym, refty) => actual_ty (valOf (Symbol.look (tenv, sym)))  
                    | final_ty => final_ty
     and trvar (A.SimpleVar(id,pos)) = (case Symbol.look(venv, id) of
                      SOME (Env.VarEntry{ty}) => {exp=(), ty=actual_ty ty}
                    | _ => (ErrorMsg.error pos ("undefined variable: " ^ Symbol.name id); {exp=(), ty=Types.INT}))                 
    in
      trexp
    end
  and transDec (venv,tenv,dec) =
    (* you should actually do something here *)
      {tenv=tenv,venv=venv}
    (* fun trdec (A.VarDec{name, escape, typ, init, pos}) =
      | trdec (A.TypeDec(typedecs)) = *)

end