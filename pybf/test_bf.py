from .bf import run_bf, BFInterp

def test_hello_world() -> None:
    res = run_bf("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>ajdjdkjd+.>.",
           False)
    
    assert res == "Hello World!\n"

def test_profile() -> None:
    interp = BFInterp("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>ajdjdkjd+.>.",
           True)
    
    interp.run()
    
    assert interp.prof_res.instr_count["+"] == 254 
    assert len(interp.prof_res.simple_loops) == 0
    assert len(interp.prof_res.complex_loops) == 0

def test_profile_cmplx() -> None:
    interp = BFInterp('''>,+++[+++>,]+++<+++[+++<]>[.>]''',
           True)
    
    interp.run()
    
    assert interp.prof_res.instr_count['.'] == 15
    assert len(interp.prof_res.simple_loops) == 0
    assert len(interp.prof_res.complex_loops) == 0

def test_profile_smpl() -> None:
    pass