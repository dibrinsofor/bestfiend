open Base
(* <, >, ., ,, [, ], +, -  *)
type token = 
  | Left | Right | Plus | Minus | Dot | Comma | LBrack | RBrack

exception InterpError of string

let tokenize input =
  let rec tokenize_aux i acc =
    if i >= String.length input then List.rev acc
    else
      let token = match input.[i] with
        | '<' -> Some Left
        | '>' -> Some Right
        | '+' -> Some Plus
        | '-' -> Some Minus
        | '.' -> Some Dot
        | ',' -> Some Comma
        | '[' -> Some LBrack
        | ']' -> Some RBrack
        | _ -> None
      in
      match token with
      | Some t -> tokenize_aux (i + 1) (t :: acc)
      | None -> tokenize_aux (i + 1) acc
  in
  tokenize_aux 0 []

let drop_from_index idx lst =
    List.filteri ~f:(fun i _ -> i >= idx) lst
  
let rec seek_closing_brack tokens depth = 
  match tokens with
  | [] -> raise (InterpError "expected closing bracket")
  | RBrack :: rest when depth = 0 -> rest
  | RBrack :: rest -> seek_closing_brack rest (depth - 1)
  | LBrack :: rest -> seek_closing_brack rest (depth + 1)
  | _ :: rest -> seek_closing_brack rest depth

let rec seek_opening_brack tokens o_t depth idx = 
  match tokens with 
  | [] -> raise (InterpError "expected opening bracket")
  | LBrack :: rest when depth = 0 -> 
    rest @ o_t
    (* drop_from_index idx o_t *)
  | LBrack :: rest -> seek_opening_brack rest o_t (depth - 1) (idx + 1)
  | RBrack :: rest -> seek_opening_brack rest o_t (depth + 1) (idx + 1)
  | t :: rest -> seek_opening_brack rest (t :: o_t) depth (idx + 1)

let interpret tokens =
  let outt = Buffer.create 500 in
  let tape = Array.create ~len:30000 0 in
  let buf = Buffer.create 500 in
  let rec execute acc tokens ptr =
    match tokens with
    | [] -> 
      Buffer.add_string buf (Printf.sprintf "Mafo, progam agwula");
      Stdio.Out_channel.output_buffer Stdio.stdout buf;
      Stdio.Out_channel.flush Stdio.stdout;
      Buffer.clear buf;
      ""
    | Left :: rest -> 
      let new_ptr = if ptr > 0 then ptr - 1 else ptr in
      Buffer.add_string buf (Printf.sprintf "Decr Ptr:: %i\n" new_ptr);
      Stdio.Out_channel.output_buffer Stdio.stdout buf;
      Stdio.Out_channel.flush Stdio.stdout;
      Buffer.clear buf;
      execute (Left :: acc) rest new_ptr
    | Right :: rest -> 
      let new_ptr = if ptr < 29999 then ptr + 1 else ptr in
      Buffer.add_string buf (Printf.sprintf "Adv Ptr:: %i\n" new_ptr);
      Stdio.Out_channel.output_buffer Stdio.stdout buf;
      Stdio.Out_channel.flush Stdio.stdout;
      Buffer.clear buf;
      execute (Right :: acc) rest new_ptr
    | Plus :: rest -> 
      tape.(ptr) <- Int.rem (tape.(ptr) + 1) 256;
      Buffer.add_string buf (Printf.sprintf "Val:: %i\n" tape.(ptr));
      Stdio.Out_channel.output_buffer Stdio.stdout buf;
      Stdio.Out_channel.flush Stdio.stdout;
      Buffer.clear buf;
      execute (Plus :: acc) rest ptr
    | Minus :: rest ->
      tape.(ptr) <- Int.rem (tape.(ptr) - 1 + 256) 256;
      Buffer.add_string buf (Printf.sprintf "Val:: %i\n" tape.(ptr));
      Stdio.Out_channel.output_buffer Stdio.stdout buf;
      Stdio.Out_channel.flush Stdio.stdout;
      Buffer.clear buf;
      execute (Minus :: acc) rest ptr
    | Dot :: rest ->
      let output_char = Char.of_int_exn tape.(ptr) in
      Buffer.add_string buf (Printf.sprintf "Out (as int):: %i\n" tape.(ptr));
      Buffer.add_string buf (Printf.sprintf "Out (as str):: %s\n" (String.of_char output_char));
      Stdio.Out_channel.output_buffer Stdio.stdout buf;
      Stdio.Out_channel.flush Stdio.stdout;
      Buffer.clear buf;
      
      Stdio.Out_channel.output_buffer Stdio.stdout outt;
      Stdio.Out_channel.flush Stdio.stdout;
      Buffer.clear outt;
      execute (Dot :: acc) rest ptr
    | Comma :: rest -> 
      Buffer.add_string buf (Printf.sprintf "Out:: %i\n" tape.(ptr));
      Stdio.Out_channel.output_buffer Stdio.stdout buf;
      Stdio.Out_channel.flush Stdio.stdout;
      Buffer.clear buf;
      let ch = Stdio.In_channel.input_char Stdio.stdin in
        tape.(ptr) <-  Option.value_map ch ~default:0 ~f:Char.to_int;
      execute (Comma :: acc) rest ptr
    | LBrack :: rest -> 
      Buffer.add_string buf (Printf.sprintf "Entering loop @ %i with %i\n" ptr tape.(ptr));
      Stdio.Out_channel.output_buffer Stdio.stdout buf;
      Stdio.Out_channel.flush Stdio.stdout;
      Buffer.clear buf;
      if tape.(ptr) = 0 then
        let goto = seek_closing_brack rest 0 in
        execute (LBrack :: acc) goto ptr
      else
        execute (LBrack :: acc) rest ptr
    | RBrack :: rest -> 
      Buffer.add_string buf (Printf.sprintf "Leaving loop @ %i with %i\n" ptr tape.(ptr));
      Stdio.Out_channel.output_buffer Stdio.stdout buf;
      Stdio.Out_channel.flush Stdio.stdout;
      Buffer.clear buf;
      if tape.(ptr) = 0 then
        execute (RBrack :: acc) rest ptr
      else
        let rev = List.rev acc in 
        let goto = seek_opening_brack rev [] 0 0 in
        execute (RBrack :: acc) (List.rev goto @ rest) ptr
  in
    Stdio.Out_channel.output_buffer Stdio.stdout outt;
    Stdio.Out_channel.flush Stdio.stdout;
    execute [] tokens 0

    
let frontend input =
  let tokens = tokenize input in
  interpret tokens
