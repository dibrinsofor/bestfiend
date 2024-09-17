open! Base
open! Stdio
open Bf.Parser


let token_equal t1 t2 =
  match t1, t2 with
  | Left, Left | Right, Right | Plus, Plus | Minus, Minus
  | Dot, Dot | Comma, Comma | LBrack, LBrack | RBrack, RBrack -> true
  | _ -> false


let%test "tokens" =
  List.equal token_equal (tokenize "+++") [ Plus; Plus; Plus ]

  let%test "tokens2" =
  List.equal token_equal (tokenize ">++[<++]<.") [ Right; Plus; Plus; LBrack; Left; Plus; Plus; RBrack; Left; Dot ]


(* let%test "interp_hw" =
  String.equal (let program = "+++++++ [ > ++++++++++ < - ] > ++ . < +++ [ > ++++++++++ < - ] > - .
+++++++ . . +++ . < ++++++++ [ > ---------- < - ] > + . 
< +++++ [ > ++++++++++ < - ] > +++++ . < ++ [ > ++++++++++ < - ] > ++++ . 
+++ . ------ . -------- . < +++++++ [ > ---------- < - ] > +++ . 
< ++ [ > ---------- < - ] > --- ." in frontend program) "Hello WOrld!" *)