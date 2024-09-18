import argparse
import sys
from typing import Optional, Self, List

class BFInterp:
    input_index: int = 0
    ptr: int = 0
    code_pointer: int = 0
    output = ""
    input_stream: str = ""
    commands = '<>[].,+-'
    
    def __init__(self: Self, code: str, mem_size: int = 30000) -> None:
        self.tape = [0] * mem_size
        self.code = ''.join(c for c in code if c in self.commands)

    def run(self: Self) -> str:
        while self.code_pointer < len(self.code):
            self.interp(self.code[self.code_pointer])
            self.code_pointer += 1
        return self.output

    def interp(self: Self, instr: str) -> None:
        if instr == '>':
            self.ptr += 1
        elif instr == '<':
            self.ptr -= 1
        elif instr == '+':
            self.tape[self.ptr] = (self.tape[self.ptr] + 1) % 256
        elif instr == '-':
            self.tape[self.ptr] = (self.tape[self.ptr] - 1) % 256
        elif instr == '.':
            self.output += chr(self.tape[self.ptr])
        elif instr == ',':
            if self.input_index < len(self.input_stream):
                self.tape[self.ptr] = ord(self.input_stream[self.input_index])
                self.input_index += 1
            else:
                self.tape[self.ptr] = 0
        elif instr == '[':
            if self.tape[self.ptr] == 0:
                bracket_count = 1
                while bracket_count > 0:
                    self.code_pointer += 1
                    if self.code[self.code_pointer] == '[':
                        bracket_count += 1
                    elif self.code[self.code_pointer] == ']':
                        bracket_count -= 1
        elif instr == ']':
            if self.tape[self.ptr] != 0:
                bracket_count = 1
                while bracket_count > 0:
                    self.code_pointer -= 1
                    if self.code[self.code_pointer] == ']':
                        bracket_count += 1
                    elif self.code[self.code_pointer] == '[':
                        bracket_count -= 1

def run_bf(code: str) -> str:
    interpreter = BFInterp(code)
    return interpreter.run()

def read_file(file_path: str) -> Optional[str]:
    try:
        with open(file_path, 'r') as file:
            return file.read()
    except IOError as e:
        print(f"Error reading file: {e}", file=sys.stderr)
        return None

def main() -> None:
    parser = argparse.ArgumentParser(description="Bf interp")
    parser.add_argument('src', nargs='?', help='Read bf src')
    parser.add_argument('--s', type=str, help='Read bf str')
    args = parser.parse_args()

    if args.src:
        content = read_file(args.input)
        if content:

            print("bf:\n", run_bf(content))
    else:
        print("bf:\n", run_bf(args.s))

if __name__ == "__main__":
    main()