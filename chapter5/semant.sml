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

  (*fun typelookup tenv n pos= 
    let 
      val result=Symbol.look (tenv, n)
    in  
      (case result of
        SOME ty2 => ty2
      | NONE => (ErrorMsg.error pos ("type is not defined: " ^ Symbol.name n) ; Types.UNIT))
    end
  
  fun transTy (tenv, t)=
    let 
      fun recordtys(fields)= map (fn{name, escape, typ, pos}=>
            (case SOME(typelookup tenv typ pos) of 
               SOME t => (name, t)
             | _ => (name, Types.UNIT))) fields
      fun checkdups(h::l) = 
            (List.exists (fn {name, escape, typ, pos}=> 
                if (#name h)=name then
                  (ErrorMsg.error pos ("duplicate field: " ^ Symbol.name name);
                  true)
                else
                  false) l;
            checkdups(l))
        | checkdups(_) = ()
    in
      case t of
        A.NameTy (n, pos) => typelookup tenv n pos
      | A.RecordTy fields => (checkdups(fields);Types.RECORD (recordtys fields, ref()))
      | A.ArrayTy (n,pos) => Types.ARRAY(typelookup tenv n pos, ref())
    end*)

  fun transProg (exp:A.exp) : unit =
    let
      val {ty=_, exp=prog} = transExp (Env.base_venv, Env.base_tenv) exp
    in
      prog
    end
  and transExp (venv:venv,tenv:tenv) : A.exp -> expty =
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
            val (venv=venv', tenv=tenv') = transDecs(venv, tenv, decs)
          in
            transExp(venv', tenv') body
          end*)

       | trexp _ = {ty=Types.UNIT, exp=ErrorMsg.error 0 "Can't typecheck this yet"}
     
     and trexprs nil = {ty=Types.UNIT, exp=()}
       (* | trexprs [(exp, pos)] = trexpr exp*)
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
  and transDec (venv:venv, tenv:tenv, decs) =
     {tenv=tenv,venv=venv}
  (*  let
      fun trdec (A.VarDec{name, escape, typ=SOME(id,pos), init, pos}) =
        let
          val {exp,ty} = transExp(venv,tenv,init)
        in
          {tenv=tenv, venv=Symbol.enter(venv,name, Env.VarEntry{ty=ty})}
        end*)
      (*| trdec (A.TypeDec[{name,ty, pos}]) = {venv=venv, tenv=Symbol.enter(tenv,name,transTy(tenv,ty))}*)
      (*| trdec (venv,tenv,A.FunctionDec[(name,params,body,pos,result=SOME(rt, pos))]) =
        let
          val SOME(result_ty) = value
        in
          body
        end
    in
      trdec
    end    *)
end