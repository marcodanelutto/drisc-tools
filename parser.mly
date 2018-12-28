%{
open Asm
open Prettyprinter
let parse_error s = Printf.printf "DRisc parser: "; print_endline s
let line = ref 0 
let print_error () = 
  Printf.printf "parse error on line %d\n"
	((Parsing.symbol_start_pos()).Lexing.pos_lnum)
%}

%token <int> INT
%token <string> LABEL
%token REG
%token COLON
%token COMMA COMMENT
%token MEMVAL REGVAL LOCVAL START DEFINE
%token IMMEDIATE
%token EOL EOF
%token ADD ADDI AND ANDI SUB SUBI MUL MULI DIV DIVI MOD MODI
%token LOAD LOADI STORE STOREI
%token GOTO 
%token CALL 
%token IFEQ IFEQ0 IFNEQ IFNEQ0
%token IFG0 IFG IFL0 IFL IFGE0 IFGE IFLE0 IFLE
%token CLEAR INCR DECR MOV
%token END

%start main

%type <Asm.instruction list> main
%type <Asm.instruction list> program
%type <Asm.instruction> anyinstr
%type <Asm.asm> instr
%type <Asm.reg> reg
%type <Asm.const> imm
%%

main: 
     program { $1 }
;

program: 
     END                   { [Instr(End)]               }
  |  LABEL COLON END EOL   { [LabInstr(LabLab($1),End)] }
  |  anyinstr program      {  $1::$2                    }
;

anyinstr: 
     LABEL COLON instr EOL   { LabInstr(LabLab($1),$3)  }
  |  instr EOL               { Instr($1)                }
  |  MEMVAL INT INT EOL      { Instr(Memloc($2,$3))     } 
  |  REGVAL INT INT EOL      { Instr(Regval($2,$3))     } 
  |  LOCVAL INT EOL          { Instr(Locval($2))        }
  |  START INT EOL           { Instr(Start($2))         }
  |  error EOL               { Instr(Error)             }
;

instr: 
     SUB reg COMMA reg COMMA reg    { Sub($2, $4, $6) }
|    SUBI reg COMMA imm COMMA reg   { Subi($2, $4, $6) } 
|    ADD reg COMMA reg COMMA reg    { Add($2, $4, $6) } 
|    ADDI reg COMMA imm COMMA reg   { Addi($2, $4, $6) } 
|    AND reg COMMA reg COMMA reg    { And($2, $4, $6) } 
|    ANDI reg COMMA imm COMMA reg   { Andi($2, $4, $6) } 
|    MUL reg COMMA reg COMMA reg    { Mul($2, $4, $6) }
|    MULI reg COMMA imm COMMA reg   { Muli($2, $4, $6) }
|    DIV reg COMMA reg COMMA reg    { Div($2, $4, $6) }
|    DIVI reg COMMA imm COMMA reg   { Divi($2, $4, $6) }
|    MOD reg COMMA reg COMMA reg    { Mod($2, $4, $6) }
|    MODI reg COMMA imm COMMA reg   { Modi($2, $4, $6) }
|    GOTO reg                       { Gotor($2) }
|    GOTO LABEL                     { Gotol(LabLab($2)) }
|    CALL reg COMMA reg             { Call($2,$4) }
|    STORE reg COMMA reg COMMA reg  { Store($2,$4,$6)}
|    STOREI reg COMMA imm COMMA reg { Storei($2,$4,$6)}
|    LOAD reg COMMA reg COMMA reg   { Load($2,$4,$6)}
|    LOADI reg COMMA imm COMMA reg  { Loadi($2,$4,$6)}
|    IFEQ reg COMMA reg COMMA LABEL { Ifeq($2,$4,LabLab($6)) }
|    IFNEQ reg COMMA reg COMMA LABEL{ Ifneq($2,$4,LabLab($6)) }
|    IFEQ0 reg COMMA LABEL          { Ifeq0($2,LabLab($4)) }
|    IFNEQ0 reg COMMA LABEL         { Ifneq0($2,LabLab($4)) }
|    IFL reg COMMA reg COMMA LABEL  { Ifl($2,$4,LabLab($6))}
|    IFLE reg COMMA reg COMMA LABEL { Ifle($2,$4,LabLab($6))}
|    IFGE reg COMMA reg COMMA LABEL { Ifge($2,$4,LabLab($6))}
|    IFL0 reg COMMA LABEL           { Ifl0($2,LabLab($4))}
|    IFG reg COMMA reg COMMA LABEL  { Ifg($2,$4,LabLab($6))}
|    IFG0 reg COMMA LABEL           { Ifg0($2,LabLab($4))}
|    CLEAR reg                      { Add(Reg(0), Reg(0), $2) }
|    INCR reg                       { Addi($2, Const(1), $2) }
|    DECR reg                       { Subi($2, Const(1), $2) }
|    MOV  reg COMMA reg             { Add($2, Reg(0), $4) }
;

reg: 
    REG INT   { Reg($2) }
; 

imm: 
     IMMEDIATE INT  { Const($2) }; 
%%
