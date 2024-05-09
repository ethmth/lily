open Libparser
open Ast

module FuncId = struct

  let rec compare_typ t1 t2 =
    match t1, t2 with
    | Int, Int | Bool, Bool | Char, Char | Float, Float | Void, Void | Any, Any -> 0
    | Int, _ -> -1
    | _, Int -> 1
    | Bool, _ -> -1
    | _, Bool -> 1
    | Char, _ -> -1
    | _, Char -> 1
    | Float, _ -> -1
    | _, Float -> 1
    | Any, _ -> -1
    | _, Any -> 1
    | List(lt1), List(lt2) -> compare_typ lt1 lt2
    | List(_), _ -> -1
    | _, List(_) -> 1

  type t = {id: string; args: typ list}

  let rec compare_list l1 l2 = match l1, l2 with
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | h1 :: t1, h2 :: t2 ->
      match compare_typ h1 h2 with
      | 0 -> compare_list t1 t2
      | c -> c

  let compare (x: t) (y: t) =
    match String.compare x.id y.id with
    | 0 -> compare_list x.args y.args
    | c -> c
end