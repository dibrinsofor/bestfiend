import subprocess
import argparse
import os
import sys
from typing import Any, Optional, List

def run_command(command: List[str]) -> Optional[str]:
    try:
        result = subprocess.run(command, check=True, text=True, timeout=20, capture_output=True)
        return result.stdout
    except subprocess.CalledProcessError as e:
        print(f"Error executing command: {' '.join(command)}")
        print(f"Error message: {e.stderr}")
        sys.exit(e.returncode)
    except subprocess.TimeoutExpired:
        print(f"Command Timed Out: {' '.join(command)}")


def compile_and_run_arm(asm_file: str) -> None:
    base_name = os.path.splitext(asm_file)[0]
    base_name = os.path.splitext(base_name)[0] 
    
    run_command(["gcc", "-v", "-g", asm_file, "-o", base_name, "-pedantic", "-lc", "-target", "arm64-apple-darwin24.0.0"])
    # run_command(["as", "-o", f"{base_name}.o", asm_file])
    # run_command(["ld", "-o", base_name, f"{base_name}.o", "-lSystem", "-syslibroot", "`xcrun -sdk macosx --show-sdk-path`", "-e", "_main", "-arch", "arm64"])
    print(f"Linking all done, running exec: ./{base_name}")
    
    out = run_command([f"./{base_name}"])
    print("bf: \n", out)

def gen_asm(bf_file: str) -> str:
    run_command(["dune", "build"])
    run_command(["dune", "exec", "--", "bf", "-src", bf_file])
    
    return bf_file + ".asm"

def run_benchmark() -> None:
    filepath = "../../brainfuck-benchmark/benches/"

    benchmarks = os.listdir(filepath)
    asm_bench = [gen_asm(filepath + x) for x in benchmarks if x.endswith(".b")]

    for x in asm_bench:
        try:
            compile_and_run_arm(x)
        except Exception:
            print(f"unable to run: {x}")

def run() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("-b", action="store_true")
    parser.add_argument("-c", "--compile-exec-asm")
    
    if len(sys.argv) == 1:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args()

    if args.b:
        run_benchmark()
    elif args.compile_exec_asm:
        if not os.path.exists(args.compile_exec_asm):
            print(f"File not found: {args.compile_exec_asm}")
            sys.exit(1)
    
        compile_and_run_arm(args.compile_exec_asm)



if __name__ == "__main__":
    run()

    
    