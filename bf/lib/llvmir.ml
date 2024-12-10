open !Base
open !Utils
open Ollvm.Ez.Instr

module AST = Ollvm_ast
module M = Ollvm.Ez.Module
module T = Ollvm.Ez.Type
module P = Ollvm.Printer
module B = Ollvm.Ez.Block
module V = Ollvm.Ez.Value

type architecture = Intel | ARM | WASM

let get_arch arch = 
  match arch with 
  | ARM -> ("arm64", "apple", "macosx15.0.0")
  | Intel -> ("x86_64", "pc", "linux-gnu")
  | WASM -> ("wasm32", "", "")

let generate_llvm_ir filename program = 

  (* let _ = Stdio.print_endline "inside llvm mod\n" in *)
  let _chan = Stdio.Out_channel.create (filename ^ ".ll") in
  let arch = get_arch ARM in
  let _ = Stdio.print_endline (BFIR.bfir_to_string program) in

  let rec _gen_labels count (acc: string list) = 
    if count <> 0 then
    _gen_labels (count -1) ((Printf.sprintf "loop_%d" count) :: acc) 
    else 
    List.rev acc
  in 

  let m = M.init
    filename
    arch
    "e-m:e-i64:64-f80:128-n8:16:32:64-S128" in (* data layout *)
    
  let i8 = AST.TYPE_I 8 in
  let i64 = AST.TYPE_I 64 in

  let zero = AST.VALUE_Integer 0 in

  (* Declare external functions *)
  let m, getchar = M.global m i8 "getchar" in
  let m = M.declaration m (B.declare getchar []) in
  let m, putchar = M.global m T.i32 "putchar" in
  let m = M.declaration m (B.declare putchar [i8]) in
  let m, memset = M.global m T.void "memset" in
  let m = M.declaration m (B.declare memset [T.pointer i8; i8; i64]) in

  (* Initialize memory *)
  let data_ptr = {
    AST.g_ident = ID_Global "data_ptr";
    AST.g_typ = TYPE_Pointer (TYPE_I 64);  (* Pointer to 64-bit integer *)
    AST.g_constant = false;
    AST.g_value = Some (AST.VALUE_Null);  (* Initial value as null *)
    AST.g_linkage = Some LINKAGE_External;
    AST.g_visibility = Some VISIBILITY_Default;
    AST.g_dll_storage = None;
    AST.g_thread_local = None;
    AST.g_unnamed_addr = false;
    AST.g_addrspace = None;
    AST.g_externally_initialized = false;
    AST.g_section = None;
    AST.g_align = Some 8;  (* 8-byte alignment for 64-bit pointer *)
  } in

  (* let m, cell_memory = M.global m (T.pointer array_type) "cell_memory" in *)
  let mem_size = 30000 in
  let array_type = T.array mem_size i8 in
  let cell_memory = {
    AST.g_ident = ID_Global "cell_memory";
    AST.g_typ = TYPE_Array (mem_size, i8);
    AST.g_constant = false;
    AST.g_value = Some (VALUE_Zero_initializer);  (* Initialized with zero values *)
    AST.g_linkage = Some LINKAGE_External;
    AST.g_visibility = Some VISIBILITY_Default;
    AST.g_dll_storage = None;
    AST.g_thread_local = None;
    AST.g_unnamed_addr = false;
    AST.g_addrspace = None;
    AST.g_externally_initialized = false;
    AST.g_section = None;
    AST.g_align = Some 8;  (* 8-byte alignment for 64-bit integers *)
  } in
    
  let m = {
    m with m_module = { m.m_module with m_globals = m.m_module.m_globals @ [
      ("data_ptr", data_ptr);
      ("cell_memory", cell_memory)
      ]
    }
  } in

  let cells = AST.VALUE_Ident (AST.ID_Global "cell_memory") in
  let ptr = AST.VALUE_Ident (AST.ID_Global "data_ptr") in

  let zero_byte = i8, zero in
  let _ = call (memset) [
    array_type, cells; (* destination *)
    zero_byte;   (* value to set *)
    i64, AST.VALUE_Integer mem_size
  ] in

  (* Entry point *)
  let m, bf = M.global m T.i32 "bf" in
  let _, label = M.local m T.label "_entry" in

  let instrs = 
    let rec gen_llvm instr acc =
      match instr with
      | [] -> List.rev acc
      | Left { count } :: rst -> 
        (* let (_, ptr) = data_ptr in *)
        let m, (_, current_val) = M.local m i64 "current_ptr" in

        let load_instr = AST.INSTR_Load (false, (T.pointer i64, ptr), None) in
        let _, (_, sub_result) = M.local m i64 "ptr_sub_result" in

        let sub_instr = AST.INSTR_IBinop (
          Sub (false, false),
          i64,
          current_val,
          AST.VALUE_Integer count
        ) in

        let store_instr = AST.INSTR_Store (
          false,
          (i64, sub_result),
          (T.pointer i64, AST.ID_Local "data_ptr"),
          None
        ) in
    
        let left_b = [load_instr; sub_instr; store_instr] in
        gen_llvm rst (List.rev left_b @ acc)

      | Right { count } :: rst -> 

        let m, (_, current_val) = M.local m i64 "current_ptr" in
        let load_instr = AST.INSTR_Load (false, (T.pointer i64, ptr), None) in
        let _, (_, add_result) = M.local m i64 "ptr_add_result" in

        let add_instr = AST.INSTR_IBinop (
          Add (false, false),
          i64,
          current_val,
          AST.VALUE_Integer count
        ) in

        let store_instr = AST.INSTR_Store (
          false,
          (i64, add_result),
          (T.pointer i64, AST.ID_Local "data_ptr"),
          None
        ) in
      
        let right_b = [load_instr; add_instr; store_instr] in
        gen_llvm rst (List.rev right_b @ acc)

      | Plus { count } :: rst -> 

          let m, (_, current_ptr) = M.local m i8 "current_ptr" in
          let curr = AST.INSTR_Load (false, (T.pointer i64, ptr), None) in
          let curr_ptr_instr = AST.INSTR_Assign (AST.ID_Local "current_ptr", curr) in
          
          let m, (_, cell_ptr) = M.local m (T.pointer i8) "cell_ptr" in
          
          let get_ptr = AST.INSTR_GetElementPtr (
              (array_type, cells),
              [(T.i32, AST.VALUE_Integer 0); (i64, current_ptr)]
          ) in
          let get_ptr_instr = AST.INSTR_Assign (AST.ID_Local "cell_ptr", get_ptr) in
          
          let m, (_, loaded_val) = M.local m i8 "loaded_val" in
          let load = AST.INSTR_Load (false, (T.pointer i8, cell_ptr), None) in
          let load_instr = AST.INSTR_Assign (AST.ID_Local "loaded_val", load) in
          
          let _, (_, add_result) = M.local m i8 "add_result" in
          let add = AST.INSTR_IBinop (
            Add (false, false),
            i8,
            loaded_val,
            AST.VALUE_Integer count
          ) in
          let add_instr = AST.INSTR_Assign (AST.ID_Local "add_result", add) in
          
          let store_instr = AST.INSTR_Store (
              false,
              (i8, add_result),
              (T.pointer i8, AST.ID_Local "cell_ptr"),
              None
          ) in
      
          let plus_b = [curr_ptr_instr; get_ptr_instr; load_instr; add_instr; store_instr] in 
          gen_llvm rst (List.rev plus_b @ acc)
      
      | Minus { count } :: rst -> 
          
          let m, (_, current_ptr) = M.local m (T.pointer i8) "cell_ptr" in
          
          let get_ptr_instr = AST.INSTR_GetElementPtr (
              (array_type, cells),
              [(T.i32, AST.VALUE_Integer 0); (i64, ptr)]
          ) in
          
          let m, (_, loaded_val) = M.local m i8 "loaded_val" in
          let load_instr = AST.INSTR_Load (false, (T.pointer i8, current_ptr), None) in
          
          let _, (_, sub_result) = M.local m i8 "sub_result" in
          let sub_instr = AST.INSTR_IBinop (
              Sub (false, false),
              i8,
              loaded_val,
              AST.VALUE_Integer count
          ) in
          
          let store_instr = AST.INSTR_Store (
              false,
              (i8, sub_result),
              (T.pointer i8, AST.ID_Local "current_ptr"),
              None
          ) in
      
          let minus_b = [get_ptr_instr; load_instr; sub_instr; store_instr] in 
          gen_llvm rst (List.rev minus_b @ acc)   

      | Dot :: rst -> 
        let m, (_, cell_ptr) = M.local m (T.pointer i8) "cell_ptr" in

        let get_ptr_instr = AST.INSTR_GetElementPtr (
          (array_type, cells),
          [(T.i32, AST.VALUE_Integer 0); (T.i32, AST.VALUE_Integer 0)]
        ) in
        let _, (_, loaded_val) = M.local m i8 "cell_value" in

        let load_instr = AST.INSTR_Load (false, (T.pointer i8, cell_ptr), None) in

        let putchar_call = AST.INSTR_Call (
          (T.i32, AST.ID_Global "putchar"),
          [(i8, loaded_val)]
        ) in
      
        let dot_b = [get_ptr_instr; load_instr; putchar_call] in
        gen_llvm rst (List.rev dot_b @ acc)

      | Comma :: rst -> 
        let m, (_, _cell_ptr) = M.local m (T.pointer i8) "cell_ptr" in

        let get_ptr_instr = AST.INSTR_GetElementPtr (
          (array_type, cells),
          [(T.i32, AST.VALUE_Integer 0); (T.i32, AST.VALUE_Integer 0)]
        ) in

        let _, (_, input_val) = M.local m i8 "input_value" in
        let getchar_call = AST.INSTR_Call (
          (i8, AST.ID_Global "getchar"),
          []
        ) in

        let store_instr = AST.INSTR_Store (
          false,
          (i8, input_val),
          (T.pointer i8, AST.ID_Local "cell_ptr"),
          None
        ) in
      
        let comma_b = [get_ptr_instr; getchar_call; store_instr] in
        gen_llvm rst (List.rev comma_b @ acc)

      | Loop _body :: rst -> gen_llvm rst (acc)
      | _ :: rst -> gen_llvm rst (acc) in
    gen_llvm program [] in 

  let block = B.block label instrs in
  let bf_instrs = B.define bf [] [block] in
  let m = M.definition m bf_instrs in
  (* let _ = Stdio.print_endline "inside llvm mod, done\n" in *)
  P.modul (P.empty_env ()) Stdlib.Format.std_formatter m.m_module



(* open Llvm

let context = global_context ()
let the_module = create_module context "BFCompiler"
let builder = builder context
let i8_type = i8_type context
let i32_type = i32_type context

(* Allocate memory *)
let bf_memory = define_global "tape" (const_array i8_type (Array.make 30000 (const_int i8_type 0))) the_module

(* Declare external functions *)
let putchar = declare_function "putchar" (function_type i32_type [| i32_type |]) the_module
let getchar = declare_function "getchar" (function_type i32_type [||]) the_module

(* Generate code for BF instructions *)
let rec codegen_instr tape_ptr instr =
  match instr with
  | Move n ->
      let ptr = build_load tape_ptr "ptr" builder in
      let new_ptr = build_add ptr (const_int i32_type n) "ptr_add" builder in
      ignore (build_store new_ptr tape_ptr builder)
  | Add n ->
      let ptr = build_load tape_ptr "ptr" builder in
      let cell_ptr = build_gep bf_memory [| ptr |] "cell_ptr" builder in
      let cell = build_load cell_ptr "cell" builder in
      let new_val = build_add cell (const_int i8_type n) "cell_add" builder in
      ignore (build_store new_val cell_ptr builder)
  | Output ->
      let ptr = build_load tape_ptr "ptr" builder in
      let cell_ptr = build_gep bf_memory [| ptr |] "cell_ptr" builder in
      let cell = build_load cell_ptr "cell" builder in
      let cell_as_int = build_zext cell i32_type "zext" builder in
      ignore (build_call putchar [| cell_as_int |] "" builder)
  | Input ->
      let input = build_call getchar [||] "getchar" builder in
      let input_as_byte = build_trunc input i8_type "trunc" builder in
      let ptr = build_load tape_ptr "ptr" builder in
      let cell_ptr = build_gep bf_memory [| ptr |] "cell_ptr" builder in
      ignore (build_store input_as_byte cell_ptr builder)
  | Loop body ->
      let loop_cond_bb = append_block context "loop.cond" (block_parent (insertion_block builder)) in
      let loop_body_bb = append_block context "loop.body" (block_parent (insertion_block builder)) in
      let loop_end_bb = append_block context "loop.end" (block_parent (insertion_block builder)) in

      ignore (build_br loop_cond_bb builder);

      (* Loop condition *)
      position_at_end loop_cond_bb builder;
      let ptr = build_load tape_ptr "ptr" builder in
      let cell_ptr = build_gep bf_memory [| ptr |] "cell_ptr" builder in
      let cell = build_load cell_ptr "cell" builder in
      let cond = build_icmp Icmp.Eq cell (const_int i8_type 0) "loop_cond" builder in
      ignore (build_cond_br cond loop_end_bb loop_body_bb builder);

      (* Loop body *)
      position_at_end loop_body_bb builder;
      List.iter (codegen_instr tape_ptr) body;
      ignore (build_br loop_cond_bb builder);

      (* Loop end *)
      position_at_end loop_end_bb builder


let compile_bf instructions =
  let main_fn_type = function_type i32_type [||] in
  let main_fn = define_function "main" main_fn_type the_module in
  let bb = append_block context "entry" main_fn in
  position_at_end bb builder;

  let tape_ptr = build_alloca i32_type "tape_ptr" builder in
  ignore (build_store (const_int i32_type 0) tape_ptr builder);

  List.iter (codegen_instr tape_ptr) instructions;

  ignore (build_ret (const_int i32_type 0) builder);

  the_module


let () =
  let code = "+++[>+++<-]>." in
  let instructions = fst (parse_bf (List.of_seq (String.to_seq code))) in
  let llvm_module = compile_bf instructions in
  print_module "bf_output.ll" llvm_module;
  print_endline "Generated LLVM IR written to bf_output.ll" *)
