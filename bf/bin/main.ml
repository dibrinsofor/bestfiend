open! Base
open! Stdio
open! Bf.Parser
open Core
open! Core_unix

(* exception FrontendError of string *)

let process_input input =
  print_endline ("Processing input: " ^ input);
  let result = frontend input in
  print_endline ("Result: " ^ result);
  result

let read_input_from_file filename =
  In_channel.read_all filename

let read_input_from_stdin () =
  print_endline "Reading from stdin...";
  let buffer = Buffer.create 1024 in
  let rec read_loop () =
    match In_channel.input_line In_channel.stdin with
    | None -> print_endline "Finished reading stdin"
    | Some line -> 
        print_endline ("Read line: " ^ line);
        Buffer.add_string buffer (line ^ "\n"); 
        read_loop ()
  in
  read_loop ();
  let content = Buffer.contents buffer in
  print_endline ("Total input: " ^ content);
  content

  
let command =
  Command.basic
    ~summary:"Interp your best friend"
    (let open Command.Let_syntax in
      let%map_open
        filename = flag "file" (optional string) ~doc:"FILE input file"
      in
      fun () ->
        match filename with
        | Some file -> 
          print_endline "out:";
          read_input_from_file file |> process_input |> print_endline
        | None -> 
          print_endline "out:";
          read_input_from_stdin () |> process_input |> print_endline)
  
  let () = Command_unix.run command
