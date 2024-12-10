open !Utils
open !Llvmir
open !Parser
open BFIR
open Base

type arch = Intel | ARM | WASM

let get_file_ext filename arch =
    match arch with
    | Intel -> filename ^ ".asm"
    | ARM -> filename ^ ".asm"
    | WASM -> filename ^ ".wat"

let generate_asm filename program arch  = 
    let filename_asm = get_file_ext filename arch in
    let chan = Stdio.Out_channel.create filename_asm in
    let output_asm asm = Out_channel.output_string chan (Printf.sprintf "%s\n" asm) in

    let _ = Stdio.print_endline (BFIR.bfir_to_string program) in
    
    let generate_label prefix =
        let counter = ref 0 in
        fun () ->
            Int.incr counter;
            Printf.sprintf "%s_%d" prefix !counter
    in
    
    let loop_label = generate_label "loop" in
    let loop_stack = Stack.create () in

    let emit_loop_end_cmd arch loop_stack = 
        let label = Stack.pop_exn loop_stack in
        match arch with
        | ARM -> 
            output_asm "    ldrb w0, [x20]";
            output_asm (Printf.sprintf "    cbnz w0, %s" label);
            output_asm (Printf.sprintf "%s_end:" label);
        | _ -> ()
    in

    let rec _output_asm_range i max =
        if i <= max then
          (output_asm (Printf.sprintf " str xzr, [x1, #%d]" (i * 16));
           _output_asm_range (i + 1) max)
    in 

    let anal_scan (bfir: bfir) = 
        match bfir with
        | Right {count} -> (count, "#1")
        | Left {count} -> (count, "#-1")
        | _ -> failwith "Invalid Scan Loop" 
    in

    let get_vector_width count =
        match count with
        | 2 -> 8
        | 4 -> 8
        | 8 -> 16
        | 16 -> 32
        | _ -> failwith "Unsupported width"
    in

    (* (match vector_select with
    | 8 ->
        output_asm "    ldr q0, [x20]";        (* Load 8-bit vector *)
        output_asm "    cmeq v1.8b, v0.8b, #0"; (* Compare 8-bit vector to zero *)
        output_asm "    not v1.8b, v1.8b";     (* Invert mask *)
        output_asm "    umov w0, v1.b[0]";     (* Extract first non-zero index *)
        output_asm (Printf.sprintf "    add x20, x20, x0")
    | 16 ->
        output_asm "    ldr q0, [x20]";        (* Load 16-bit vector *)
        output_asm "    cmeq v1.16b, v0.16b, #0";
        output_asm "    not v1.16b, v1.16b";
        output_asm "    umov w0, v1.b[0]";
        output_asm (Printf.sprintf "    add x20, x20, x0")
    | 32 ->
        output_asm "    ldr q0, [x20]";        (* Load 32-bit vector *)
        output_asm "    ldr q1, [x20, #16]";   (* Load next 32 bits *)
        output_asm "    cmeq v2.16b, v0.16b, #0";
        output_asm "    cmeq v3.16b, v1.16b, #0";
        output_asm "    not v2.16b, v2.16b";
        output_asm "    not v3.16b, v3.16b";
        output_asm "    umov w0, v2.b[0]";     (* First try first vector *)
        output_asm "    cbnz w0, scan_found";
        output_asm "    umov w0, v3.b[0]";     (* If not found, try second vector *)
        output_asm "    add x0, x0, #16";      (* Adjust index for second vector *)
        output_asm "scan_found:";
        output_asm (Printf.sprintf "    add x20, x20, x0") *)

    let emit_scan_instr width count =
        match width with
        | 8 -> 
            output_asm "    ldr q0, [x20]";
            output_asm "    cmeq v1.8b, v0.8b, #0";
            output_asm "    not v1.8b, v1.8b";
            output_asm "    umov w0, v1.b[0]";
            output_asm (Printf.sprintf "    add x20, x20, #%d" count)
            (* output_asm (Printf.sprintf "    add x20, x20, x0") *)
        | 16 -> 
            (* output_asm "    ldr q0, [x20]"; 
            output_asm "    cmeq v1.16b, v0.16b, #0"; 
            output_asm "    mov w1, v1.d[0]"; 
            output_asm "    rbit w1, w1"; 
            output_asm "    clz w0, w1";  *)
            output_asm "    ldr q0, [x20]";
            output_asm "    cmeq v1.16b, v0.16b, #0";
            output_asm "    not v1.16b, v1.16b";
            output_asm "    umov w0, v1.b[0]";
            output_asm (Printf.sprintf "    add x20, x20, #%d" count)
        | 32 ->
            output_asm "    ldr q0, [x20]";        (* Load 32-bit vector *)
            output_asm "    ldr q1, [x20, #16]";   (* Load next 32 bits *)
            output_asm "    cmeq v2.16b, v0.16b, #0";
            output_asm "    cmeq v3.16b, v1.16b, #0";
            output_asm "    not v2.16b, v2.16b";
            output_asm "    not v3.16b, v3.16b";
            output_asm "    umov w0, v2.b[0]";
            output_asm "    cbnz w0, scan_found";
            output_asm "    umov w0, v3.b[0]";
            output_asm "    add x0, x0, #16";
            output_asm "scan_found:";
            output_asm (Printf.sprintf "    add x20, x20, #%d" count)
        | _ -> failwith "Unsupported width"
    in
    
    (match arch with
    | ARM ->
        output_asm ".global _main";
        output_asm ".align 2";
        output_asm ".text";
        output_asm ".extern _malloc";
        output_asm ".extern _free";
        output_asm ".extern _memset";
        output_asm ".extern _putchar";
        output_asm ".extern _getchar";
        output_asm "_main:";
        output_asm "    stp x29, x30, [sp, #-16]!";
        output_asm "    mov x29, sp";
        output_asm "    mov x0, #30000"; (*Allocate memory for cells (30,000 bytes)*)
        output_asm "    bl _malloc";
        output_asm "    mov x20, x0"; (*x20 will be our cell pointer*)
        output_asm "    cbz x20, _exit_error"; (* Check if malloc failed *)
        output_asm "    str x20, [sp, #-16]!"; (* Store malloc'd pointer on stack *)
        output_asm "    mov x2, #30000";
        output_asm "    mov x1, #0"; (* Initialize memory to zero *)
        output_asm "    bl _memset";
    | _ -> ()
    );
    
    let rec emit_command (command: bfir) =
        match command with 
        | Left { count } -> 
        (match arch with
            | ARM -> output_asm (Printf.sprintf "    sub x20, x20, #%d" count)
            | _ -> ()
        )
        | Right { count } -> 
        (match arch with
            | ARM -> output_asm (Printf.sprintf "    add x20, x20, #%d" count)
            | _ -> ()
        )
        | Plus { count } -> 
        (match arch with
            | ARM ->
                output_asm "    ldrb w0, [x20]";
                output_asm (Printf.sprintf "    add w0, w0, #%d" count);
                output_asm "    and w0, w0, #255";
                output_asm "    strb w0, [x20]"
            | _ -> ()
        )
        | Minus { count } -> 
        (match arch with
            | ARM ->
                output_asm "    ldrb w0, [x20]";
                output_asm (Printf.sprintf "    sub w0, w0, #%d" count);
                output_asm "    and w0, w0, #255";
                output_asm "    strb w0, [x20]"
            | _ -> ()
        )
        | Dot ->
        (match arch with
            | ARM ->
                output_asm "    ldrb w0, [x20]";
                output_asm "    bl _putchar"
            | _ -> ()
        )
        | Comma ->
        (match arch with
            | ARM ->
                output_asm "    bl _getchar";
                output_asm "    and w0, w0, #255";
                output_asm "    strb w0, [x20]"
            | _ -> ()
        )
        | Nop -> ()
        | Scan disp -> 
            let (count, _) = anal_scan disp.dir in
            let width = get_vector_width count in
            (match arch with
            | ARM ->
                emit_scan_instr width count
            | _ -> ()
            )
        | Loop { body; _ } -> (* implicit L brack*)
            let label = loop_label () in
            Stack.push loop_stack label;
            (match arch with
                | ARM -> 
                    output_asm (Printf.sprintf "%s:" label);
                    output_asm "    ldrb w0, [x20]";
                    output_asm (Printf.sprintf "    cbz w0, %s_end" label);

                    List.iter body ~f:emit_command;

                    emit_loop_end_cmd ARM loop_stack;
                | _ -> ()
        )
    in
    
    List.iter program ~f:emit_command;
        
    (match arch with
    | ARM ->
        output_asm "    ldr x0, [sp], #16"; (* Load malloc'd pointer from stack *)
        output_asm "    bl _free"; (* Free the allocated memory *)
        output_asm "    mov w0, #0"; (* Set return value to 0 (success) *)
        output_asm "    ldp x29, x30, [sp], #16";
        output_asm "    ret"; (* Return from main *)

        (* Keep the error exit code *)
        output_asm "_exit_error:";
        output_asm "    mov x0, #1"; (* error code 1 *)
        output_asm "    bl _exit"
    | _ -> ()
    );
    
    Out_channel.close chan


module ParserMap = struct
    module T = struct
        type t = Parser.token
        let compare = Parser.compare_token
        let sexp_of_t = Parser.sexp_of_token
    end
    include T
    include Comparable.Make(T)
    end
        
(* todo: store this info in a module *)
let generate filename input opt ?(_profile = false)() =
  let arch = ARM in
  let program = parse_program input in
  let bfir = BFIR.gen_ir program [] None in
  let result = 
    match opt with 
    | 1 -> 
        let _ = Stdio.print_endline (bfir_to_string bfir) in 
        let optimized = opt_simple bfir in
        let _ = Stdio.print_endline (bfir_to_string optimized) in 
        generate_asm filename optimized arch
    | 2 -> 
        let _ = Stdio.print_endline (bfir_to_string bfir) in 
        let optimized = opt_mem_scan bfir in
        let _ = Stdio.print_endline (bfir_to_string optimized) in 
        generate_asm filename (optimized) arch
    | 3 -> 
        let _ = Stdio.print_endline (bfir_to_string bfir) in 
        let optimized = apply_both_opts bfir in
        let _ = Stdio.print_endline (bfir_to_string optimized) in 
        generate_asm filename (optimized) arch
    | _ -> generate_asm filename bfir arch
  in 
  result;
  Stdio.print_endline "gen prog exit";