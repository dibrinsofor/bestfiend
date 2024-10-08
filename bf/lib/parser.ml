open Printf
open Base
  
type token = 
  | Left | Right | Plus | Minus | Dot | Comma | LBrack | RBrack

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
  parse [] (List.of_seq (String.to_seq code))
  
  let interpret program =
    let memory = Array.make 30000 0 in
    let pointer = ref 0 in
    
    let rec execute pc =
      if pc >= List.length program then ()
      else
        match List.nth program pc with
        | Left -> incr pointer; execute (pc + 1)
        | Right -> decr pointer; execute (pc + 1)
        | Plus-> memory.(!pointer) <- (memory.(!pointer) + 1) mod 256; execute (pc + 1)
        | Minus -> memory.(!pointer) <- (memory.(!pointer) - 1 + 256) mod 256; execute (pc + 1)
        | Dot -> printf "%c" (Char.chr memory.(!pointer)); execute (pc + 1)
        | Comma -> memory.(!pointer) <- Char.code (input_char stdin); execute (pc + 1)
        | LBrack ->
            if memory.(!pointer) = 0 then
              let rec find_matching depth i =
                if i >= List.length program then failwith "Unmatched ["
                else match List.nth program i with
                  | RBrack when depth = 0 -> i
                  | LBrack -> find_matching (depth + 1) (i + 1)
                  | RBrack -> find_matching (depth - 1) (i + 1)
                  | _ -> find_matching depth (i + 1)
              in
              execute (find_matching 0 (pc + 1) + 1)
            else
              execute (pc + 1)
        | RBrack ->
            let rec find_matching depth i =
              if i < 0 then failwith "Unmatched ]"
              else match List.nth program i with
                | LBrack when depth = 0 -> i
                | RBrack -> find_matching (depth + 1) (i - 1)
                | LBrack -> find_matching (depth - 1) (i - 1)
                | _ -> find_matching depth (i - 1)
            in
            if memory.(!pointer) <> 0 then
              execute (find_matching 0 (pc - 1))
            else
              execute (pc + 1)
    in
    execute 0
  

let frontend input =
  let program = parse_program input in
  interpret program