open! Base
open! Stdio
open Bf.Parser


let token_equal t1 t2 =
  match t1, t2 with
  | Left, Left | Right, Right | Plus, Plus | Minus, Minus
  | Dot, Dot | Comma, Comma | LBrack, LBrack | RBrack, RBrack -> true
  | _ -> false


let%test "tokens" =
  List.equal token_equal (parse_program "+++") [ Plus; Plus; Plus ]

let%test "tokens2" =
  List.equal token_equal (parse_program ">++[<++]<.") [ Right; Plus; Plus; LBrack; Left; Plus; Plus; RBrack; Left; Dot ]

let%test "tokensdot" =
  List.equal token_equal (parse_program "...") [ Dot; Dot; Dot ]

(* how do I test the content of stdout *)
let%test "loops" = 
  let prog = "+++[>+++++<-]>." in
  let profiler = {
    instr_count = Hashtbl.create (module String);
    simple_loops = Hashtbl.create (module TokenHashSet);
    complex_loops = Hashtbl.create (module TokenHashSet);
  } in
  begin
  interpret (parse_program prog) true profiler;
  Int.equal (Hashtbl.length profiler.simple_loops) 1;
  end

let%test "loops" = 
  let program = "+++[>+++++<-]>." in
  let result = frontend program () in 
  String.equal result "A"

let%test "interpret_hello_world" =
  let program = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." in
  let result = frontend program () in
  String.equal result "Hello World!"