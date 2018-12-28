(** some support to the analysis of D-RISC code on pipeline processors
    This includes the possibility to find dependencies in linear 
    D-RISC code and to execute D-RISC instructions with a given 
    configuration of registers and memory (with no cache).
    @author Marco Danelutto 
    @year 2011
*)

open Printf;;

(** the type modelling registers *)
type reg = Reg of int;;

(** the type modelling labels: either strings or offsets *)
type label = NoLab | LabOff of int | LabLab of string;;

(** the type modelling the constants: e.g. #i -> Const(i) *)
type const = Const of int;;

(** the environment used to compute an instruction *)
(*                         pc    ,    mem assoc   ,    reg array    *)
type driscenv = Env of (int ref) * (int*int) list ref * (int ref array) ;;


(** the assembler opcodes *)
type asm = 
   Add of reg*reg*reg
|  And of reg*reg*reg
|  Sub of reg*reg*reg
|  Mul of reg*reg*reg
|  Div of reg*reg*reg
|  Mod of reg*reg*reg
|  Addi of reg*const*reg
|  Andi of reg*const*reg
|  Subi of reg*const*reg
|  Muli of reg*const*reg
|  Divi of reg*const*reg
|  Modi of reg*const*reg
|  Incr of reg
|  Decr of reg
|  Load of reg*reg*reg
|  Loadi of reg*const*reg
|  Store of reg*reg*reg
|  Storei of reg*const*reg
|  Call of reg*reg
|  Gotor of reg
|  Gotol of label
|  Ifl0 of reg*label
|  Ifl of reg*reg*label
|  Ifle0 of reg*label
|  Ifle of reg*reg*label
|  Ifge0 of reg*label
|  Ifge of reg*reg*label
|  Ifg0 of reg*label
|  Ifg of reg*reg*label
|  Ifeq0 of reg*label
|  Ifeq of reg*reg*label
|  Ifneq0 of reg*label
|  Ifneq of reg*reg*label
(* this are actually directives to the assembler program *)
|  Memloc of int*int
|  Regval of int*int
|  Locval of int
|  Data of int * ((int) list)
|  Start  of int
|  Error
|  End
;;

(** the assembler instruction: may have a label *)
type instruction = 
   Instr of asm 
|  LabInstr of label*asm;;

(** the type of "relocated" instructions *)
type relocinstruction = 
   RelI of label * asm * int;;
