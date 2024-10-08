open Printf
open Base
  
type token = 
  | Left | Right | Plus | Minus | Dot | Comma | LBrack | RBrack


let get_char_value () =
  match Stdio.In_channel.input_char Stdio.stdin with
  | Some c -> Char.to_int c
  | None -> 0

  let parse_program code =
  let rec parse acc = function
    | [] -> List.rev acc
    | '>' :: rest -> parse (Left :: acc) rest
    | '<' :: rest -> parse (Right :: acc) rest
    | '+' :: rest -> parse (Plus :: acc) rest
    | '-' :: rest -> parse (Minus :: acc) rest
    | '.' :: rest -> parse (Dot :: acc) rest
    | ',' :: rest -> parse (Comma :: acc) rest
    | '[' :: rest -> parse (LBrack :: acc) rest
    | ']' :: rest -> parse (RBrack :: acc) rest
    | _ :: rest -> parse acc rest
  in
  parse [] (String.to_list code)
  
  let interpret program =
    let memory = Array.create ~len:30000 0 in
    let pointer = ref 0 in
    
    let rec execute pc =
      if pc >= List.length program then ()
      else
        match List.nth program pc with
        | Some Left -> Int.incr pointer; execute (pc + 1)
        | Some Right -> Int.decr pointer; execute (pc + 1)
        | Some Plus-> memory.(!pointer) <- Int.rem (memory.(!pointer) + 1) 256; execute (pc + 1)
        | Some Minus -> memory.(!pointer) <- Int.rem (memory.(!pointer) - 1 + 256) 256; execute (pc + 1)
        | Some Dot -> printf "%c" (Char.of_int_exn memory.(!pointer)); execute (pc + 1)
        | Some Comma -> memory.(!pointer) <- get_char_value (); execute (pc + 1)
        | Some LBrack ->
            if memory.(!pointer) = 0 then
              let rec find_matching depth i =
                if i >= List.length program then failwith "Unmatched ["
                else match List.nth program i with
                  | Some RBrack when depth = 0 -> i
                  | Some LBrack -> find_matching (depth + 1) (i + 1)
                  | Some RBrack -> find_matching (depth - 1) (i + 1)
                  | _ -> find_matching depth (i + 1)
              in
              execute (find_matching 0 (pc + 1) + 1)
            else
              execute (pc + 1)
        | Some RBrack ->
            let rec find_matching depth i =
              if i < 0 then failwith "Unmatched ]"
              else match List.nth program i with
                | Some LBrack when depth = 0 -> i
                | Some RBrack -> find_matching (depth + 1) (i - 1)
                | Some LBrack -> find_matching (depth - 1) (i - 1)
                | _ -> find_matching depth (i - 1)
            in
            if memory.(!pointer) <> 0 then
              execute (find_matching 0 (pc - 1))
            else
              execute (pc + 1)
        | None -> failwith "Invalid program counter"
    in
    execute 0
  

let frontend input =
  let program = parse_program input in
  interpret program