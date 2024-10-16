open !Parser
open Base

type architecture = Intel | ARM | WASM

let get_file_ext filename arch =
    match arch with
    | Intel -> filename ^ ".asm"
    | ARM -> filename ^ ".asm"
    | WASM -> filename ^ ".wat"

let generate_asm filename program arch _profile _profiler = 
    let filename_asm = get_file_ext filename arch in
    let chan = Stdio.Out_channel.create filename_asm in
    let output_asm asm = Out_channel.output_string chan (Printf.sprintf "%s\n" asm) in
    
    let generate_label prefix =
        let counter = ref 0 in
        fun () ->
        Int.incr counter;
        Printf.sprintf "%s_%d" prefix !counter
    in
    
    let loop_label = generate_label "loop" in
    let loop_stack = Stack.create () in

    let rec output_asm_range i max =
        if i <= max then
          (output_asm (Printf.sprintf " str xzr, [x1, #%d]" (i * 16));
           output_asm_range (i + 1) max)
    in 
    
    (match arch with
    | Intel ->
        output_asm "section .text";
        output_asm "global _start";
        output_asm "_start:";
        output_asm "    mov rsi, memory ; Initialize pointer"
    | ARM ->
        output_asm ".text";
        output_asm ".global _main";
        output_asm ".align 4";
        output_asm "_main:";
        output_asm "    sub sp, sp, #0x100"; (*allocate 256 bits of stack space*)
        output_asm "    mov x1, sp"; (*x1 will be our cell pointer*)

        (* initialize our 16 cells (16 bits each) to zero *)
        output_asm_range 0 15;
    | WASM -> 
        output_asm "(module";
        output_asm "  (import \"env\" \"memory\" (memory 1))";
        output_asm "  (import \"env\" \"putchar\" (func $putchar (param i32)))";
        output_asm "  (import \"env\" \"getchar\" (func $getchar (result i32)))";
        output_asm "  (func $run (export \"run\")";
        output_asm "    (local $ptr i32)";
        output_asm "    i32.const 0";
        output_asm "    local.set $ptr"
    );
    
    let emit_command command =
        match command with 
        | Parser.Left -> 
        (match arch with
            | Intel -> output_asm "    dec rsi ; Move pointer left"
            | ARM -> 
                output_asm "    add x1, x1, #-16";
            | WASM ->
                output_asm "    local.get $ptr";
                output_asm "    i32.const 1";
                output_asm "    i32.sub";
                output_asm "    local.set $ptr"
        )
        | Parser.Right -> 
        (match arch with
            | Intel -> output_asm "    inc rsi ; Move pointer right"
            | ARM -> 
                output_asm "    add x1, x1, #16"
            | WASM ->
                output_asm "    local.get $ptr";
                output_asm "    i32.const 1";
                output_asm "    i32.add";
                output_asm "    local.set $ptr"
        )
        | Parser.Plus -> 
        (match arch with
        | Intel ->
            output_asm "    inc byte [rsi] ; Increment value";
            output_asm "    and byte [rsi], 255 ; Ensure value is in 0-255 range"
        | ARM ->
            output_asm "    ldr x7, [x1]";
            output_asm "    add x7, x7, #1";
            output_asm "    str x7, [x1]"
        | WASM ->
            output_asm "    local.get $ptr";
            output_asm "    local.get $ptr";
            output_asm "    i32.load8_u";
            output_asm "    i32.const 1";
            output_asm "    i32.add";
            output_asm "    i32.const 255";
            output_asm "    i32.and";
            output_asm "    i32.store8"
        )
        | Parser.Minus -> 
        (match arch with
        | Intel ->
            output_asm "    dec byte [rsi] ; Decrement value";
            output_asm "    and byte [rsi], 255 ; Ensure value is in 0-255 range"
        | ARM ->
            output_asm "    ldr x7, [x1]";
            output_asm "    add x7, x7, #-1";
            output_asm "    str x7, [x1]"
        | WASM ->
            output_asm "    local.get $ptr";
            output_asm "    local.get $ptr";
            output_asm "    i32.load8_u";
            output_asm "    i32.const 1";
            output_asm "    i32.sub";
            output_asm "    i32.const 255";
            output_asm "    i32.and";
            output_asm "    i32.store8"
        )
        | Parser.Dot ->
        (match arch with
            | Intel ->
                output_asm "    mov rax, 1 ; sys_write";
                output_asm "    mov rdi, 1 ; stdout";
                output_asm "    mov rdx, 1 ; length";
                output_asm "    syscall"
            | ARM ->
                output_asm "    mov x6, x1";
                output_asm "    mov x0, #1";
                output_asm "    mov x2, #1";
                output_asm "    mov x16, #4"; (*syscall write*)
                output_asm "    svc #0x80"; (*print*)
                output_asm "    mov x1, x6"
            | WASM ->
                output_asm "    local.get $ptr";
                output_asm "    i32.load8_u";
                output_asm "    call $putchar"
        )
        | Parser.Comma ->
        (match arch with
            | Intel ->
                output_asm "    mov rax, 0 ; sys_read";
                output_asm "    mov rdi, 0 ; stdin";
                output_asm "    mov rdx, 1 ; length";
                output_asm "    syscall";
            | ARM ->
                output_asm "    mov x6, x1";
                output_asm "    mov x0, #0"; 
                output_asm "    mov x2, #1";
                output_asm "    mov x16, #3"; (*syscall read*)
                output_asm "    svc #0x80";
                output_asm "    mov x1, x6"
            | WASM ->
                output_asm "    call $getchar";
                output_asm "    local.get $ptr";
                output_asm "    i32.const 255";
                output_asm "    i32.and";
                output_asm "    i32.store8"
        )
        | Parser.LBrack ->
            let label = loop_label () in
            Stack.push loop_stack label;
            (match arch with
            | Intel ->
                output_asm (Printf.sprintf "%s:" label);
                output_asm "    cmp byte [rsi], 0 ; Check if current value is zero";
                output_asm (Printf.sprintf "    je %s_end ; Jump to end if zero" label)
            | ARM ->
                output_asm (Printf.sprintf "%s:" label);
                output_asm "    ldr x7, [x1]";
                output_asm (Printf.sprintf "    cbz x7, %s_end" label)
            | WASM ->
                output_asm (Printf.sprintf "    (block $%s_end" label);
                output_asm "      (loop $loop";
                output_asm "        local.get $ptr";
                output_asm "        i32.load8_u";
                output_asm "        i32.eqz";
                output_asm (Printf.sprintf "        br_if $%s_end" label)
            )
        | Parser.RBrack ->
            let label = Stack.pop_exn loop_stack in
            (match arch with
            | Intel -> 
                output_asm "    cmp byte [rsi], 0 ; Check if current value is zero";
                output_asm (Printf.sprintf "    jne %s ; Jump back to start of loop if non-zero" "")
            | ARM -> 
                output_asm (Printf.sprintf "%s_end:" label);
                output_asm "    ldr x7, [x1]";
                output_asm (Printf.sprintf "    cbnz x7, %s" label)
            | WASM ->
                output_asm "        br $loop";
                output_asm "      )";
                output_asm "    )"
            )
    in
    
    List.iter program ~f:emit_command;
    
    (match arch with
    | Intel ->
        output_asm "    mov rax, 60 ; sys_exit";
        output_asm "    xor rdi, rdi ; exit code 0";
        output_asm "    syscall";
        output_asm "section .bss";
        output_asm "memory: resb 30000"
    | ARM ->
        output_asm "    add sp, sp, #0x100";  (* Restore stack ptr *)
        output_asm "    mov w0, #0";
        output_asm "    ret";
    | WASM ->
        output_asm "  )";
        output_asm ")"
    );
    
    Out_channel.close chan


let generate filename input ?(profile = false)() =
  let profiler = {
    instr_count = Hashtbl.create (module String);
    simple_loops = Hashtbl.create (module TokenHashSet);
    complex_loops = Hashtbl.create (module TokenHashSet);
  } in
  let arch = ARM in
  let program = parse_program input in
  let result = generate_asm filename program arch profile profiler in 
  result;
  Stdio.print_endline "gen prog exit";