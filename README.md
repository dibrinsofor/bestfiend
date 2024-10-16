### reqs
- dune
- ocaml
- Core and Base modules. (`opam install core base`)

### compiler runs by default.
- build: `cd bf && dune build` 
- run: `dune exec -- bf --help`

### run generated assembly with python script
- run: `py test_gen.py` 
> [!IMPORTANT]  
> Only tested the ARM assembly code.

#### [dni ++ not improving ++ probs doesnt work] python interpreter with profiler
run: `cd pybf && python bf.py --s [bf_string]`
