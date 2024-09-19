import argparse
import sys
from typing import Optional, Self, List, Any, Dict

class Profiler:
    instr_count: Dict[str, int]

    def __init__(self: Self, i_c: Dict[str, int], smpl: List[Any], cmplx: List[Any]) -> None:
        self.instr_count = i_c
        self.simple_loops = smpl
        self.complex_loops = cmplx

    def __repr__(self) -> str:
        msg = f'''Instr count:\n===========\n{self.instr_count}'''
        msg2 = f'Simple Loops:\n===========\n' + (''.join(self.simple_loops) if len(self.simple_loops) > 0 else "None found.")
        msg3 = f'Complex Loops:\n===========\n' + (''.join(self.complex_loops) if len(self.complex_loops) > 0 else "None found.")
        
        return msg + '\n\n' + msg2 + '\n\n' + msg3
    
class Loop:
    def __init__(self: Self, posn: tuple[int, int], instr: str, has_io: bool) -> None:
        self.posn = posn
        self.instr = instr
        self.has_io = has_io 

class BFInterp:
    input_index: int = 0
    ptr: int = 0
    code_pointer: int = 0
    output = ""
    input_stream: str = ""
    commands = '<>[].,+-'
    profile: bool = False
    instr_profile: Dict[str, int] = {char: 0 for char in commands}
    loops: List[Loop] = []
    prof_res: Profiler
    
    def __init__(self: Self, code: str, profile: bool, mem_size: int = 30000) -> None:
        self.tape = [0] * mem_size
        self.code = ''.join(c for c in code if c in self.commands)
        self.profile = profile

    def sanitize_profile(self: Self) -> Profiler:
        print("here too", self.loops)
        inner = [x for x in self.loops for y in self.loops if x.posn[0] >= y.posn[0] and x.posn[1] <= y.posn[0]]

        smpl = [x.instr for x in inner if x.has_io is False]
        cmplx = [x.instr for x in inner if x.has_io is True]

        self.prof_res = Profiler(self.instr_profile, smpl, cmplx)
        return self.prof_res
    
    def run(self: Self) -> str:
        while self.code_pointer < len(self.code):
            self.interp(self.code[self.code_pointer])
            self.code_pointer += 1

        if self.profile:
            self.sanitize_profile()
            print(self.prof_res)
        return self.output
    

    def is_simple(self: Self) -> None:
        pass

    def interp(self: Self, instr: str) -> None:
        if self.profile:
            self.instr_profile[instr] += 1

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
        elif instr == '[' and self.profile:
            print("here")
            if self.tape[self.ptr] == 0:
                depth = 1
                has_io = False
                loop_seq: str = ""
                while depth > 0:
                    loop_seq += self.code[self.code_pointer]

                    self.code_pointer += 1
                    if self.code[self.code_pointer] == '[':
                        depth += 1
                    elif self.code[self.code_pointer] == ']':
                        depth -= 1

                    if self.code[self.code_pointer] in '.,':
                        has_io = True
                ## why would this work woth append and not insert??
                self.loops.append(Loop((self.ptr, self.code_pointer), loop_seq, has_io))
        elif instr == '[':
            if self.tape[self.ptr] == 0:
                depth = 1
                while depth > 0:
                    self.code_pointer += 1
                    if self.code[self.code_pointer] == '[':
                        depth += 1
                    elif self.code[self.code_pointer] == ']':
                        depth -= 1
        elif instr == ']':
            if self.tape[self.ptr] != 0:
                depth = 1
                while depth > 0:
                    self.code_pointer -= 1
                    if self.code[self.code_pointer] == ']':
                        depth += 1
                    elif self.code[self.code_pointer] == '[':
                        depth -= 1

def run_bf(code: str, profile: bool) -> str:
    interpreter = BFInterp(code, profile)
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
    parser.add_argument('--p', action='store_true', help='run profiler on script')
    parser.add_argument('--s', type=str, help='Read bf str')
    args = parser.parse_args()

    if args.src:
        content = read_file(args.input)
        if content:

            print("bf:\n", run_bf(content, args.p))
    else:
        print("bf:\n", run_bf(args.s, args.p))


if __name__ == "__main__":
    main()