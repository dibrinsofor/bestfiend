import subprocess
import os
import sys
from typing import Optional, List

def run_command(command: List[str]) -> Optional[str]:
    try:
        result = subprocess.run(command, check=True, text=True, capture_output=True)
        return result.stdout
    except subprocess.CalledProcessError as e:
        print(f"Error executing command: {' '.join(command)}")
        print(f"Error message: {e.stderr}")
        sys.exit(1)

def compile_and_run_arm(asm_file: str) -> None:
    base_name = os.path.splitext(asm_file)[0]
    base_name = os.path.splitext(base_name)[0] 
    
    run_command(["gcc", "-g", "-v", asm_file, "-o", base_name])
    print(f"Linking all done, running exec: ./{base_name}")
    
    out = run_command([f"./{base_name}"])
    print("bf: \n", out)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python arm_tester.py <assembly_file.asm>")
        sys.exit(1)
    
    asm_file = sys.argv[1]
    if not os.path.exists(asm_file):
        print(f"File not found: {asm_file}")
        sys.exit(1)
    
    compile_and_run_arm(asm_file)