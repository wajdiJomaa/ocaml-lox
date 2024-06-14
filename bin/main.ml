let source = Olex.File.read_whole_file "/home/wajdi/Desktop/olex/test_files/calc"
let lexer = Olex.Lexer.scan source
let parser = Olex.Parser.parse lexer

let () = Olex.Interpreter.print_result (Olex.Interpreter.evaluate parser) 


