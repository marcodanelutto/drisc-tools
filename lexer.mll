{
open Parser
exception Eof
exception Lexerr
}

rule token = parse 
    [' ' '\t'] { token lexbuf }
|   ['\n']     { EOL }
|   [':']      { COLON }
|   [',']      { COMMA }
|   ['#']      { IMMEDIATE }
|   ['0'-'9']* as num { INT(int_of_string num) }
|   "R"        { REG }
|   "//"       { COMMENT }
|   "memloc"   { MEMVAL }
|   "data"     { DATA }
|   "regval"   { REGVAL }
|   "loc"      { LOCVAL }
|   "start"    { START }
|   "define"   { DEFINE }
|   "add"      { ADD }
|   "ADD"      { ADD }
|   "addi"     { ADDI }
|   "ADDI"     { ADDI }
|   "and"      { AND }
|   "AND"      { AND }
|   "andi"     { ANDI }
|   "ANDI"     { ANDI }
|   "sub"      { SUB }
|   "SUB"      { SUB }
|   "subi"     { SUBI }
|   "SUBI"     { SUBI }
|   "mul"      { MUL }
|   "MUL"      { MUL }
|   "muli"     { MULI }
|   "MULI"     { MULI }
|   "div"      { DIV }
|   "DIV"      { DIV }
|   "mod"      { MOD }
|   "MOD"      { MOD }
|   "modi"     { MODI }
|   "MODI"     { MODI }
|   "divi"     { DIVI }
|   "DIVI"     { DIVI }
|   "goto"     { GOTO }
|   "GOTO"     { GOTO }
|   "load"     { LOAD }
|   "LOAD"     { LOAD }
|   "loadi"    { LOADI }
|   "LOADI"    { LOADI }
|   "ld"       { LOAD }
|   "LD"       { LOAD }
|   "ldi"      { LOADI }
|   "LDI"      { LOADI }
|   "store"    { STORE }
|   "STORE"    { STORE }
|   "storei"   { STOREI }
|   "STOREI"   { STOREI }
|   "st"       { STORE }
|   "ST"       { STORE }
|   "sti"      { STOREI }
|   "STI"      { STOREI }
|   "call"     { CALL }
|   "CALL"     { CALL }
|   "if=0"     { IFEQ0 }
|   "IF=0"     { IFEQ0 }
|   "if="      { IFEQ }
|   "IF="      { IFEQ }
|   "if!=0"    { IFNEQ0 }
|   "IF!=0"    { IFNEQ0 }
|   "if!="     { IFNEQ }
|   "IF!="     { IFNEQ }
|   "if<"      { IFL }
|   "IF<"      { IFL }
|   "if<="     { IFLE }
|   "if>="     { IFGE }
|   "if<0"     { IFL0 }
|   "IF<0"     { IFL0 }
|   "if>"      { IFG }
|   "IF>"      { IFG }
|   "if>0"     { IFG0 }
|   "IF>0"     { IFG0 }
|   "clear"    { CLEAR }
|   "CLEAR"    { CLEAR }
|   "decr"     { DECR }
|   "DECR"     { DECR }
|   "incr"     { INCR }
|   "INCR"     { INCR }
|   "move"     { MOV }
|   "MOVE"     { MOV }
|   "mov"      { MOV }
|   "MOV"      { MOV }
|   "end"      { END }
|   "END"      { END }
|   ['a'-'z''0'-'9']* as lxm { LABEL(lxm) }
|   eof        { EOF }
