let source = Olex.File.read_whole_file "/home/wajdi/Desktop/olex/test_files/calc.lox"
let lexer = Olex.Lexer.scan source
let parser = Olex.Parser.parse lexer

let _ =  Olex.Interpreter.evaluate parser Olex.Interpreter.env


