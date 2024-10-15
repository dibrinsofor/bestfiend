open Bf.Parser
open Bf.Gen
open Bf.Frontend
open Core

let process_input input profile = 
  frontend input ~profile:profile () 

let read_input_from_file filename =
  In_channel.read_all filename

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
        profile = flag "--profile" (Command.Flag.optional_with_default false Command.Param.bool) ~doc:"Profile loops in your bf program"
      and 
        interp = flag "--i" (Command.Flag.optional_with_default false Command.Param.bool) ~doc:"Interpret your bf program"
      in
      fun () ->
        let input =
          match filename with
          | Some file -> read_input_from_file file
          | None -> read_input_from_stdin () in

        if interp then
          process_input input profile |> print_endline
        else
          match filename with 
          | Some f_name -> generate f_name input ~profile:profile ();
          | None -> failwith "Expected BF Program" |> print_endline)

let () = Command_unix.run run_bf
