open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval t  -> true
  | _ -> false

let rec eval1 t = match t with
    TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc(fi,t1) ->
      let t1' = eval1 t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 t1 in
      TmIsZero(fi, t1')
  | _ -> 
      raise NoRuleApplies
(*          
let rec eval t =
  try let t' = eval1 t
      in eval t'
  with NoRuleApplies -> t
*)

let rec evaln t = match t with
		TmFalse(_) -> 
			t
	|	TmTrue(_) ->
			t
	| TmZero(_) ->
			t				
	| TmIf(_, TmTrue(_), t2, t3) ->
    	evaln t2
  | TmIf(_, TmFalse(_), t2, t3) ->
    	evaln t3
  | TmIf(fi,t1,t2,t3) -> 
			(match evaln t1 with
						TmTrue(_) -> evaln t2
					| TmFalse(_) -> evaln t3
					|  _ -> raise NoRuleApplies    
			)
  | TmSucc(fi,t1) -> 
    	TmSucc(fi,evaln(t1))
	| TmPred(fi,t1) ->
		(match evaln t1 with
				TmZero(_) -> 
					TmZero(dummyinfo)
			| TmSucc(_,nv1) when (isnumericval nv1) ->
				nv1
			| _ ->
				raise NoRuleApplies	
		)
	| TmIsZero(fi,t1) ->
		(match evaln t1 with
				TmZero(_) ->
					TmTrue(dummyinfo)
			| TmSucc(_,nv1) when (isnumericval nv1) ->
					TmFalse(dummyinfo)
			| _ ->
				raise NoRuleApplies
		)
	| _ ->
			raise NoRuleApplies		


let eval t = 
	try evaln t
	with NoRuleApplies -> t