open Parser
open Base

let frontend input ?(profile = false) () =
  let profiler = { 
    instr_count = Hashtbl.create (module String);
    simple_loops = Hashtbl.create (module TokenHashSet);
    complex_loops = Hashtbl.create (module TokenHashSet);
  } in
  let program = parse_program input in
  let result = interpret program profile profiler in
  result;