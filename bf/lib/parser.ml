open Base
  
type token = 
  | Left | Right | Plus | Minus | Dot | Comma | LBrack | RBrack [@@deriving sexp, hash, compare]

let get_char_value () =
  match Stdio.In_channel.input_char Stdio.stdin with
  | Some c -> Char.to_int c
  | None -> 0

module TokenHashSet = struct
  type t = token list [@@deriving sexp, hash, compare]
end

type profiler = {
  instr_count: (string, int) Hashtbl.t;
  simple_loops: (TokenHashSet.t, unit) Hashtbl.t;
  complex_loops: (TokenHashSet.t, unit) Hashtbl.t;
}

let is_complex_loop tokens =
  List.exists tokens ~f:(function
    | Dot | Comma -> true
    | _ -> false)

let extract_loop pc program =
  let rec find_matching depth i =
    if i >= List.length program then failwith "Unmatched ["
    else match List.nth program i with
      | Some RBrack when depth = 0 -> i
      | Some LBrack -> find_matching (depth + 1) (i + 1)
      | Some RBrack -> find_matching (depth - 1) (i + 1)
      | _ -> find_matching depth (i + 1)
  in
  let loop_end = find_matching 0 (pc + 1) in
  (List.sub program ~pos:(pc + 1) ~len:(loop_end - pc - 1), loop_end)

let get_str_tok (instr) = 
  match instr with
  | Left -> ">"
  | Right -> "<"
  | Plus -> "+"
  | Minus -> "-"
  | Dot -> "."
  | Comma -> ","
  | LBrack -> "["
  | RBrack -> "]"

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


let interpret program profile profiler =
  let memory = Array.create ~len:30000 0 in
  let pointer = ref 0 in
  let output = Buffer.create 100 in


  let rec execute pc =
    if pc >= List.length program then Buffer.contents output
    else
      match List.nth program pc with
      | None -> failwith "Invalid program counter"
      | Some instr ->
          if profile then
            Hashtbl.update profiler.instr_count (get_str_tok instr) ~f:(function
              | None -> 1
              | Some v -> v + 1);
          match instr with
          | Left -> Int.incr pointer; execute (pc + 1)
          | Right -> Int.decr pointer; execute (pc + 1)
          | Plus -> memory.(!pointer) <- Int.rem (memory.(!pointer) + 1) 256; execute (pc + 1)
          | Minus -> memory.(!pointer) <- Int.rem (memory.(!pointer) - 1 + 256) 256; execute (pc + 1)
          | Dot -> 
              Buffer.add_char output (Char.of_int_exn memory.(!pointer));
              execute (pc + 1)
          | Comma -> memory.(!pointer) <- get_char_value (); execute (pc + 1)
          | LBrack ->
              let (loop_body, loop_end) = extract_loop pc program in
              if is_complex_loop loop_body then
                Hashtbl.set profiler.complex_loops ~key:loop_body ~data:()
              else
                Hashtbl.set profiler.simple_loops ~key:loop_body ~data:();
              if memory.(!pointer) = 0 then
                execute (loop_end + 1)
              else
                execute (pc + 1)
          | RBrack ->
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
  in
  execute 0