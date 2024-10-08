open! Base
open! Stdio
open! Bf.Parser
open Core
open! Core_unix

(* exception FrontendError of string *)

let process_input input = 
  frontend input

let read_input_from_file filename =
  In_channel.read_all filename

let read_input_from_stdin () =
  let buffer = Buffer.create 1024 in
  let rec read_loop () =
    match In_channel.input_line In_channel.stdin with
    | None -> print_endline ""
    | Some line -> 
        Buffer.add_string buffer (line ^ "\n"); 
        read_loop ()
  in
  read_loop ();
  let content = Buffer.contents buffer in
  content

  
let command =
  Command.basic
    ~summary:"Interp your bf code"
    (let open Command.Let_syntax in
      let%map_open
        filename = flag "file" (optional string) ~doc:"FILE input file"
      in
      fun () ->
        match filename with
        | Some file -> 
          read_input_from_file file |> process_input
        | None -> 
          read_input_from_stdin () |> process_input)
  
  let () = Command_unix.run command
