(* <, >, ., ,, [, ], +, -  *)
type token = 
  | Left | Right | Plus | Minus | Dot | Comma | LBrack | RBrack

exception InterpError of string

(* Tokenizer function *)
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

let rec seek_closing_brack tokens depth = 
  match tokens with
  | [] -> raise (InterpError "expected closing bracket")
  | RBrack :: rest when depth = 0 -> rest
  | RBrack :: rest -> seek_closing_brack rest (depth - 1)
  | LBrack :: rest -> seek_closing_brack rest (depth + 1)
  | _ :: rest -> seek_closing_brack rest depth

let rec seek_opening_brack tokens depth idx = 
  match tokens with 
  | [] -> raise (InterpError "expected opening bracket")
  | LBrack :: rest when depth = 0 -> rest
  | LBrack :: rest -> seek_opening_brack rest (depth - 1) (idx + 1)
  | RBrack :: rest -> seek_opening_brack rest (depth + 1) (idx + 1)
  | _ :: rest -> seek_opening_brack rest depth (idx + 1)

(* BF interpreter core *)
let interpret tokens =
  let tape = Array.make 30000 0 in
  let output = Buffer.create 128 in
  let rec execute acc tokens ptr =
    match tokens with
    | [] -> Buffer.contents output
    | Left :: rest -> 
      let new_ptr = if ptr > 0 then ptr - 1 else ptr in
      execute (Left :: acc) rest new_ptr
    | Right :: rest -> 
      let new_ptr = if ptr < 29999 then ptr + 1 else ptr in
      execute (Right :: acc) rest new_ptr
    | Plus :: rest -> 
      tape.(ptr) <- (tape.(ptr) + 1) mod 256;
      execute (Plus :: acc) rest ptr
    | Minus :: rest ->
      tape.(ptr) <- (tape.(ptr) - 1 + 256) mod 256;
      execute (Minus :: acc) rest ptr
    | Dot :: rest -> begin
      Buffer.add_char output (Char.chr tape.(ptr));
      print_string (Buffer.contents output);
      execute (Dot :: acc) rest ptr
      end
    | Comma :: rest -> 
      let ch = input_char stdin in
      tape.(ptr) <- int_of_char ch;
      execute (Comma :: acc) rest ptr
    | LBrack :: rest when tape.(ptr) = 0 -> 
      let goto = seek_closing_brack rest 0 in
      execute (LBrack :: acc) goto ptr
    | LBrack :: rest -> 
      execute (LBrack :: acc) rest ptr
    | RBrack :: rest when tape.(ptr) = 0 -> 
      execute (RBrack :: acc) rest ptr
    | RBrack :: _rest -> 
      let goto = seek_opening_brack acc 0 0 in
      execute (RBrack :: acc) goto ptr
  in
  execute [] tokens 0


let frontend input =
  let tokens = tokenize input in
  interpret tokens
