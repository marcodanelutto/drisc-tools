(* File main.ml *)
let filename = Sys.argv.(1);;

let _ =
  try
    let lexbuf = Lexing.from_channel (open_in filename) in
    while true do
      let result = Parser.main Lexer.token lexbuf in
        Prettyprinter.pp_program result
    done
  with _ (* Lexer.Eof*) -> 
    exit 0

