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
  let print_asm asm = Out_channel.output_string chan (Printf.sprintf "%s\n" asm) in

  let generate_label prefix =
    let counter = ref 0 in
    fun () ->
      Int.incr counter;
      Printf.sprintf "%s_%d" prefix !counter
  in

  let loop_label = generate_label "loop" in

  (match arch with
  | Intel ->
      print_asm "section .text";
      print_asm "global _start";
      print_asm "_start:";
      print_asm "    mov rsi, memory ; Initialize pointer"
  | ARM ->
      print_asm ".text";
      print_asm ".global _start";
      print_asm "_start:";
      print_asm "    adrp x0, memory";
      print_asm "    add x0, x0, :lo12:memory ; Initialize pointer"
  | WASM -> 
      print_asm "(module";
      print_asm "  (import \"env\" \"memory\" (memory 1))";
      print_asm "  (import \"env\" \"putchar\" (func $putchar (param i32)))";
      print_asm "  (import \"env\" \"getchar\" (func $getchar (result i32)))";
      print_asm "  (func $run (export \"run\")";
      print_asm "    (local $ptr i32)";
      print_asm "    i32.const 0";
      print_asm "    local.set $ptr"
  );

  let rec emit_command command =
    match command with 
    | Parser.Left -> 
      (match arch with
        | Intel -> print_asm "    dec rsi ; Move pointer left"
        | ARM -> print_asm "    sub x0, x0, #1 ; Move pointer left"
        | WASM ->
            print_asm "    local.get $ptr";
            print_asm "    i32.const 1";
            print_asm "    i32.sub";
            print_asm "    local.set $ptr"
        )
    | Parser.Right -> 
      (match arch with
        | Intel -> print_asm "    inc rsi ; Move pointer right"
        | ARM -> print_asm "    add x0, x0, #1 ; Move pointer right"
        | WASM ->
            print_asm "    local.get $ptr";
            print_asm "    i32.const 1";
            print_asm "    i32.add";
            print_asm "    local.set $ptr"
        )
    | Parser.Plus -> 
      (match arch with
      | Intel ->
          print_asm "    inc byte [rsi] ; Increment value";
          print_asm "    and byte [rsi], 255 ; Ensure value is in 0-255 range"
      | ARM ->
          print_asm "    ldrb w1, [x0]";
          print_asm "    add w1, w1, #1";
          print_asm "    and w1, w1, #255";
          print_asm "    strb w1, [x0]"
      | WASM ->
          print_asm "    local.get $ptr";
          print_asm "    local.get $ptr";
          print_asm "    i32.load8_u";
          print_asm "    i32.const 1";
          print_asm "    i32.add";
          print_asm "    i32.const 255";
          print_asm "    i32.and";
          print_asm "    i32.store8"
      )
    | Parser.Minus -> 
      (match arch with
      | Intel ->
          print_asm "    dec byte [rsi] ; Decrement value";
          print_asm "    and byte [rsi], 255 ; Ensure value is in 0-255 range"
      | ARM ->
          print_asm "    ldrb w1, [x0]";
          print_asm "    sub w1, w1, #1";
          print_asm "    and w1, w1, #255";
          print_asm "    strb w1, [x0]"
      | WASM ->
          print_asm "    local.get $ptr";
          print_asm "    local.get $ptr";
          print_asm "    i32.load8_u";
          print_asm "    i32.const 1";
          print_asm "    i32.sub";
          print_asm "    i32.const 255";
          print_asm "    i32.and";
          print_asm "    i32.store8"
      )
    | Parser.Dot ->
      (match arch with
        | Intel ->
            print_asm "    mov rax, 1 ; sys_write";
            print_asm "    mov rdi, 1 ; stdout";
            print_asm "    mov rdx, 1 ; length";
            print_asm "    syscall"
        | ARM ->
            print_asm "    mov x1, x0 ; buffer";
            print_asm "    mov x0, #1 ; stdout";
            print_asm "    mov x2, #1 ; length";
            print_asm "    mov x8, #64 ; sys_write";
            print_asm "    svc #0"
        | WASM ->
            print_asm "    local.get $ptr";
            print_asm "    i32.load8_u";
            print_asm "    call $putchar"
        )
    | Parser.Comma ->
      (match arch with
        | Intel ->
            print_asm "    mov rax, 0 ; sys_read";
            print_asm "    mov rdi, 0 ; stdin";
            print_asm "    mov rdx, 1 ; length";
            print_asm "    syscall";
        | ARM ->
            print_asm "    mov x1, x0 ; buffer";
            print_asm "    mov x0, #0 ; stdin";
            print_asm "    mov x2, #1 ; length";
            print_asm "    mov x8, #63 ; sys_read";
            print_asm "    svc #0"
        | WASM ->
            print_asm "    call $getchar";
            print_asm "    local.get $ptr";
            print_asm "    i32.const 255";
            print_asm "    i32.and";
            print_asm "    i32.store8"
        )
    | Parser.LBrack ->
      let label = loop_label () in
        (match arch with
        | Intel ->
            print_asm (Printf.sprintf "%s:" label);
            print_asm "    cmp byte [rsi], 0 ; Check if current value is zero";
            print_asm (Printf.sprintf "    je %s_end ; Jump to end if zero" label)
        | ARM ->
            print_asm (Printf.sprintf "%s:" label);
            print_asm "    ldrb w1, [x0]";
            print_asm "    cmp w1, #0";
            print_asm (Printf.sprintf "    b.eq %s_end" label)
        | WASM ->
            print_asm (Printf.sprintf "    (block $%s_end" label);
            print_asm "      (loop $loop";
            print_asm "        local.get $ptr";
            print_asm "        i32.load8_u";
            print_asm "        i32.eqz";
            print_asm (Printf.sprintf "        br_if $%s_end" label)
        );
        List.iter program ~f:emit_command;
        (match arch with
        | Intel ->
            print_asm (Printf.sprintf "    jmp %s ; Jump back to start of loop" label);
            print_asm (Printf.sprintf "%s_end:" label)
        | ARM ->
            print_asm (Printf.sprintf "    b %s" label);
            print_asm (Printf.sprintf "%s_end:" label)
        | WASM ->
            print_asm "        br $loop";
            print_asm "      )";
            print_asm "    )"
        )
    | Parser.RBrack -> ()
  in

  List.iter program ~f:emit_command;

  (match arch with
  | Intel ->
      print_asm "    mov rax, 60 ; sys_exit";
      print_asm "    xor rdi, rdi ; exit code 0";
      print_asm "    syscall";
      print_asm "section .bss";
      print_asm "memory: resb 30000"
  | ARM ->
      print_asm "    mov x0, #0 ; exit code 0";
      print_asm "    mov x8, #93 ; sys_exit";
      print_asm "    svc #0";
      print_asm ".bss";
      print_asm "memory: .space 30000"
  | WASM ->
      print_asm "  )";
      print_asm ")"
  );

  Out_channel.close chan


let generate filename input ?(profile = false)() =
  let profiler = {
    instr_count = Hashtbl.create (module String);
    simple_loops = Hashtbl.create (module TokenHashSet);
    complex_loops = Hashtbl.create (module TokenHashSet);
  } in
  let arch = WASM in
  let program = parse_program input in
  let result = generate_asm filename program arch profile profiler in 
  result;