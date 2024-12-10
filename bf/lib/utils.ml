open !Parser

(* todo: can sequential writes be removed or eliminated? *)
type bfir = 
  | Plus of { count: int }
  | Minus of { count: int }
  | Left of { count: int }
  | Right of { count: int }
  | Dot 
  | Comma
  | Nop
  | Scan of {dir: bfir}
  | Loop of { body: bfir list; nested: bool; cmplx: bool; mem_scan: bool}

(* (* type Interpreter struct {
	source  []byte
	memory  []byte
	pointer int
	pc      int
	jumpMap map[int]int
} *)
 *)

module BFIR = struct
  type t = (token * int) list

  let rec get_loop_body_2 program depth =
    match program with
    | [] -> None
    | tok :: rest ->
      match tok with
      | LBrack -> 
          let next_depth = depth + 1 in
          (match get_loop_body_2 rest next_depth with
          | Some body -> Some (tok :: body)
          | None -> None)
      | RBrack when depth = 1 ->
          Some [tok]
      | RBrack when depth > 1 ->
          (match get_loop_body_2 rest (depth - 1) with
          | Some body -> Some (tok :: body)
          | None -> None)
      | _ -> 
          (match get_loop_body_2 rest depth with
          | Some body -> Some (tok :: body)
          | None -> None)

  let ptr_movement body =
    List.fold_left (fun acc instr ->
      match instr with
      | Left { count } -> acc - count
      | Right { count } -> acc + count
      | _ -> acc
    ) 0 body
  
  let p0_delta body =
    List.fold_left (fun acc instr ->
      match instr with
      | Plus { count } -> acc + count
      | Minus { count } -> acc - count
      | _ -> acc
    ) 0 body
  
  let is_no_op p0 ptr_mov =
    p0 = 0 && ptr_mov = 0

  let is_propagation ptr_mov nestedorcmplx = 
    match ptr_mov with
    | 0 when not nestedorcmplx -> true
    | _ -> false

  let is_trivial_loop body =
    let ptr_mv = ptr_movement body in
    let p0 = p0_delta body in
    p0 = 0 && ptr_mv = 0 &&
    List.for_all (function
      | Minus { count = 1 } -> true
      | _ -> false) body
  
  let rec replace_simple_loop instr =
    match instr with
    | Loop { body; nested; cmplx; mem_scan } when not nested && not cmplx ->
        if is_trivial_loop body then
          Nop
        else
          Loop { body = List.map replace_simple_loop body; nested; cmplx; mem_scan }
    | Loop { body; nested; cmplx; mem_scan } ->
        Loop { body = List.map replace_simple_loop body; nested; cmplx; mem_scan }
    | _ -> instr

  let rec opt_simple program =
    let optimized = List.map replace_simple_loop program in
    if optimized = program then program
    else opt_simple optimized

  let rec drop n lst =
    match n, lst with
    | 0, _ -> lst
    | _, [] -> []
    | n, _ :: xs -> drop (n - 1) xs

  let rec get_loop_body program depth acc = 
    match program with 
    | [] when depth <> 0 ->
      failwith "Expected matching right bracket"
    | [] -> None
    | tok :: rest ->
      match tok with 
      | LBrack -> 
        get_loop_body rest (depth + 1) (tok :: acc)
      | RBrack when depth = 0 ->
        Some (List.rev (tok :: acc))
      | RBrack ->
        get_loop_body rest (depth - 1) (tok :: acc)
      | _ -> 
        get_loop_body rest depth (tok :: acc)


  let rec bfir_to_string bfir =
    let bfir_to_str = function
      | Plus { count } -> Printf.sprintf "Plus(%d)" count
      | Minus { count } -> Printf.sprintf "Minus(%d)" count
      | Left { count } -> Printf.sprintf "Left(%d)" count
      | Right { count } -> Printf.sprintf "Right(%d)" count
      | Dot -> "Dot"
      | Comma -> "Comma"
      | Loop { body; nested; cmplx; mem_scan } ->
          Printf.sprintf "Loop(nested=%b, cmplx=%b, mem_scan=%b, body=[%s])"
            nested
            cmplx
            mem_scan
            (bfir_to_string body)
      | Nop -> "Nop"
      | Scan{dir} -> 
          Printf.sprintf "Scan(shift_by=%s)" 
            (bfir_to_string [dir])
    in
    match bfir with
    | [] -> ""
    | [head] -> bfir_to_str head 
    | head :: tail ->
        let head_str = bfir_to_str head in
        let tail_str = bfir_to_string tail in
        head_str ^ ", " ^ tail_str

  let rec get_freq program token acc =
    match program with 
    | fst :: rst when fst = token ->
      get_freq rst token (acc + 1)
    | _ -> acc

  let extract_body x =
    match x with
    | Some x -> x
    | None -> failwith "Expected Loop Body"

  let get_new_label lst = 
    match lst with
    | [] -> "label", []
    | head :: tail -> head, tail
  
  let rec is_complex_loop loop =
    match loop with 
    | [] -> false
    | tok :: toks -> 
      match tok with
      | Dot | Comma -> true
      | _ -> is_complex_loop toks
      
  let rec is_nested loop =
    match loop with 
    | [] -> false
    | tok :: toks -> 
      match tok with
      | Loop _ -> true
      | _ -> is_nested toks
    
  let count_loops program =
    let rec loop_counter program acc =
      match program with
      | [] -> acc
      | fst :: rst ->
          match fst with
          | Loop { body; _} -> 
              loop_counter rst (acc + 1 + loop_counter body 0)
          | _ -> loop_counter rst acc
    in
    loop_counter program 0

    let is_mem_scan body =
      let rec helper acc = function
        | [] -> (abs acc) mod 2 = 0
        | Nop :: rst -> helper acc rst
        | Left { count } :: rst -> helper (acc + count) rst
        | Right { count } :: rst -> helper (acc - count) rst
        | _ -> false
      in
      helper 0 body      

    (* [Left(4); Right(16)] -> (Right(12)) *)
    let calc_dis (bfir_list: bfir list) =
      let rec calculate_displacement acc = function
        | [] -> acc
        | Right { count } :: rest -> 
            calculate_displacement (acc + count) rest
        | Left { count } :: rest -> 
            calculate_displacement (acc - count) rest
        | _ :: rest -> 
            calculate_displacement acc rest
      in
      calculate_displacement 0 bfir_list

    let get_dis_instr count =
      match count with
      | count when count > 0 -> Right { count = abs count }
      | count when count < 0 -> Left { count = abs count }
      | _ -> Nop

    let replace_scan = function
    | Loop data -> 
      if not data.mem_scan then
        Loop data
      else (*gra, we'vw got a scan *)
        let count = calc_dis data.body in
        let new_instr = get_dis_instr count in
          Scan {dir=new_instr}
    | other -> other
      
    let opt_mem_scan program =
      let optimized = List.map replace_scan program in
      if optimized = program then program
      else opt_simple optimized

    let rec opt_simple program =
      let optimized = List.map replace_simple_loop program in
      if optimized = program then program
      else opt_simple optimized

    let rec apply_both_opts bfir =
      let simple_opt = opt_simple bfir in
      let mem_scan_opt = opt_mem_scan simple_opt in
      if mem_scan_opt = bfir then
        bfir 
      else
        apply_both_opts mem_scan_opt

    let rec gen_ir (program: token list) (acc: bfir list) (prev: token option) =
      let create_ir (token: token) (loop: token list) =
        match token with
        | Plus ->
          let freq = get_freq program token 0 in
          Plus { count = freq }
        | Minus -> 
          let freq = get_freq program token 0 in
          Minus { count = freq }
        | Left -> 
          let freq = get_freq program token 0 in
          Left { count = freq }
        | Right ->         
          let freq = get_freq program token 0 in
          Right { count = freq }
        | Dot -> Dot
        | Comma -> Comma
        | LBrack -> 
          let body = gen_ir loop [] None in
          Loop { 
            body = body; 
            nested = is_nested body; 
            cmplx = is_complex_loop body;
            mem_scan = is_mem_scan body;
          }
        | _ -> Nop
      in 
      match program with
      | [] -> List.rev acc
      | tok :: toks -> (*., [.]*)
        match prev with  (*+*)
        | Some seen when seen = tok ->
            (match seen with
            | Dot | Comma -> 
              gen_ir toks (create_ir tok [] :: acc) prev
            | _ ->
              gen_ir toks acc prev)
        | Some _ | None -> 
          match tok with
          | LBrack -> 
            let loop_body = 
              match get_loop_body_2 toks 1 with
              | Some body -> body
              | None -> failwith "Expected Loop Body"
            in
            let loop_length = List.length loop_body in
            let rem = drop loop_length toks in
            gen_ir rem (create_ir tok loop_body :: acc) (Some tok)
          | _ -> 
            gen_ir toks (create_ir tok [] :: acc) (Some tok)

  let optimize _program = ()

end