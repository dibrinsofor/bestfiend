open !Bf.Parser
open !Bf.Gen
open Bf.Frontend
open Bf.Llvmir
open !Bf.Utils
open Core

let process_input input profile = 
  frontend input ~profile:profile () 

let read_input_from_file filename =
  In_channel.read_all filename

let get_opt opt =
  if opt > 3 then
    3
  else 
    opt

let read_input_from_stdin () =
  let buffer = Buffer.create 1024 in
  let rec read_loop () =
    match In_channel.input_line In_channel.stdin with
    | None -> ()
    | Some line -> 
        Buffer.add_string buffer (line ^ "\n"); 
        read_loop ()
  in
  read_loop ();
  let content = Buffer.contents buffer in
  content

  
let run_bf =
  Command.basic
    ~summary:"Interp your bf code"
    (let open Command.Let_syntax in
      let%map_open
        filename = flag "src" (optional string) ~doc:"read bf program from FILE"
      and
        profile = flag "profile" (Command.Flag.optional_with_default false Command.Param.bool) ~doc:"Profile loops in your bf program"
      and
        llvm = flag "ll" (Command.Flag.optional_with_default false Command.Param.bool) ~doc:"Compile to LLVM"
      and 
        opt = flag "o" (Command.Flag.optional_with_default 2 Command.Param.int) ~doc:"Optimize loops"
        (* collect number up to 3 and run optimizations that many times *)
      and 
        interp = flag "i" (Command.Flag.optional_with_default false Command.Param.bool) ~doc:"Interpret your bf program"
      in
      fun () ->
        let input =
          match filename with
          | Some file -> read_input_from_file file
          | None -> read_input_from_stdin () in

        if interp then
          process_input input profile |> print_endline
        else if llvm then 
          match filename with 
          | Some file -> 
            let program = parse_program input in
            (* let _ = Stdio.print_endline (Printf.sprintf "Generating llvm with src: %s\n" (input)) in *)
            let bfir = BFIR.gen_ir program [] None in
            (* let _ = Stdio.print_endline (Printf.sprintf "Generating llvm with ir: %s\n" (bfir_to_string bfir)) in *)
            generate_llvm_ir file bfir;
          | None -> failwith "Expected BF Program" |> print_endline;
        else
          match filename with 
          | Some f_name -> generate f_name input (get_opt opt) () ;
          | None -> failwith "Expected BF Program" |> print_endline;
          
        
        (Stdio.print_endline "gen prog exit"))

let () = Command_unix.run run_bf

