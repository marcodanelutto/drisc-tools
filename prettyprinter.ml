open Asm;;
open Printf;;

(** pretty print a register *) 
let pp_reg = function
  Reg(x) -> printf " R_%d " x ;;
 
(** pretty print a list of registers *)
let rec pp_regs = function 
  [] -> ()
| r::rr -> (pp_reg r);(pp_regs rr);;
 
(** pretty print the register set
    Used in pretty print of the 
    environment of execution of a program *)
let pp_reg_set r = 
  let n = Array.length r in 
  for i=0 to (n-1) do
    printf " R%d=%d " i !(r.(i))
  done; 
  printf "\n"
;;
 
(** pretty print a memory state. Used in pretty print of the 
    environment of execution of a program *)
let rec pp_mem m = 
  match m with
    [] -> printf "\n\n"
  | (l,v)::rm -> printf "Mem[%d]=%d\n" l v; pp_mem rm;;

let pp_mem_r r = 
  let n = Array.length r in 
  for i=0 to (n-1) do
    if(i mod 5 == 0) 
    then printf " M%d=%d " i !(r.(i))
    else printf " %d " !(r.(i));
    if(i mod 19 == 0 && not (i = 0)) 
    then printf "\n"
  done; 
  printf "\n"
;;
 
(** pretty print a label *)
let pp_lab = function 
  LabLab(s) -> printf " %s:\t" s
| NoLab -> printf "\t" 
| LabOff(o) -> printf "offset(%d) " o;;
 
(** pretty print a constant *)
let pp_const = function 
  Const c -> printf " #%d " c;;
 
(** pretty print a D-RISC instruction 
    This should still be completed. 
    In case you see a NOFORMATAVAILABLE you should add a clause
    to handle the missing instruction print *)
let pp_asm = function 
  Add(a,b,c) -> printf "ADD "; pp_reg(a); pp_reg(b); pp_reg(c)
| And(a,b,c) -> printf "AND "; pp_reg(a); pp_reg(b); pp_reg(c)
| Addi(a,b,c) -> printf "ADDI "; pp_reg(a); pp_const(b); pp_reg(c)
| Andi(a,b,c) -> printf "ANDI "; pp_reg(a); pp_const(b); pp_reg(c)
| Sub(a,b,c) -> printf "SUB "; pp_reg(a); pp_reg(b); pp_reg(c)
| Subi(a,b,c) -> printf "SUBI "; pp_reg(a); pp_const(b); pp_reg(c)
| Muli(a,b,c) -> printf "MULI "; pp_reg(a); pp_const(b); pp_reg(c)
| Divi(a,b,c) -> printf "DIVI "; pp_reg(a); pp_const(b); pp_reg(c)
| Modi(a,b,c) -> printf "MODI "; pp_reg(a); pp_const(b); pp_reg(c)
| Mul(a,b,c) -> printf "MUL "; pp_reg(a); pp_reg(b); pp_reg(c)
| Div(a,b,c) -> printf "DIV "; pp_reg(a); pp_reg(b); pp_reg(c)
| Incr(a) -> printf "INC"; pp_reg(a)
| Ifle(r1,r2,l) -> printf "IF<= "; pp_reg(r1); pp_reg(r2); pp_lab(l)
| Ifge(r1,r2,l) -> printf "IF>= "; pp_reg(r1); pp_reg(r2); pp_lab(l)
| Ifneq(r1,r2,l) -> printf "IF!= "; pp_reg(r1); pp_reg(r2); pp_lab(l)
| Ifeq(r1,r2,l) -> printf "IF= "; pp_reg(r1); pp_reg(r2); pp_lab(l)
| Ifeq0(r2,l) -> printf "IF=0 ";  pp_reg(r2); pp_lab(l)
| Ifneq0(r2,l) -> printf "IF!=0 ";  pp_reg(r2); pp_lab(l)
| Ifl(r1,r2,l) -> printf "IF< "; pp_reg(r1); pp_reg(r2); pp_lab(l)
| Ifl0(r2,l) -> printf "IF<0 "; pp_reg(r2); pp_lab(l)
| Ifg(r1,r2,l) -> printf "IF> "; pp_reg(r1); pp_reg(r2); pp_lab(l)
| Ifg0(r2,l) -> printf "IF>0 "; pp_reg(r2); pp_lab(l)
| Load(a,b,c) -> printf "LOAD "; pp_reg(a); pp_reg(b); pp_reg(c)
| Loadi(a,b,c) -> printf "LOAD "; pp_reg(a); pp_const(b); pp_reg(c)
| Store(a,b,c) -> printf "STORE "; pp_reg(a); pp_reg(b); pp_reg(c)
| Storei(a,b,c) -> printf "STOREI "; pp_reg(a); pp_const(b); pp_reg(c)
| Gotor(r) -> printf "GOTO "; pp_reg(r)
| Gotol(l) -> printf "GOTO "; pp_lab(l)
| Call(a,b) -> printf "CALL "; pp_reg(a); pp_reg(b)
| Error -> printf "Syntax error on this (hidden) line "
| End -> printf "END "
| Memloc(l,v) -> printf "Mem[%d]=%d" l v;
| Regval(r,v) -> printf "R[%d]=%d" r v;
| Locval(v)   -> printf "%d:\t" v
| Start(v)    -> printf "START at %d" v
| _ -> printf "NOFORMATAVAILABLE"
;;
 
(** pretty print an instruction, possibly with a label *)
let pp_instr add = function 
  Instr(i) -> printf "%d.\t" add; pp_asm(i); printf "\n"
| LabInstr(l,i) -> printf "%d. " add; 	
		   pp_lab(l);  pp_asm(i); printf "\n"
;;

let pp_reloci (RelI(l,a,p)) =                   
    Printf.printf "%d\t" p; pp_lab(l); pp_asm(a);Printf.printf "\n";;

let rec pp_relocp p = 
  match p with
    [] -> Printf.printf "\n"
|   i::ri -> pp_reloci i; pp_relocp ri
;;
 
(** pretty print a whole program, starting with at a given address *)
let rec pp_prog add = function 
  [] -> printf "\n"
| i::ri -> (pp_instr add i); (pp_prog (add+1) ri)
;;
 
(** pretty print a program, assuming it is allocated from address 0 *)
let pp_program p = (pp_prog 1 p);;
 
(** pretty printers for the interpreter: 
    these are used at each step in the execution of the program
    and/or at the beginning of the program *)

(** aux fun for memory pp **)
let compare (x,y) (a,b) = if(x<a) then (-1) else if(x=a) then 0 else 1;;

(** dump memory **)
let dump_mem m =
  match m with
    [] -> ()
  | _ ->
     printf "Memory dump (data)\n" ;
     List.hd (List.map (fun (l,v) -> Printf.printf "Mem[%d]=%d\t" l v)
                (List.sort (compare) (m)));
     printf "\n" ;;

(** dump registers, the compact way **)
let dump_areg x = printf "%5d " !x;;

let dump_reg r = 
  let lim = 10 in 
  let rows = 64 / lim in
  printf "Register dump (%d per line)\n\t" lim;
  for i=0 to lim-1 do 
    printf "%5d " i
  done; printf "\n\t-----------------------------------------------------------\n";
  for i=0 to rows-1 do
    Printf.printf "R%d-%d\t" (i*lim) (i*lim+lim-1);
    for j=0 to lim-1 do
      let k = i*lim + j in 
        dump_areg r.(k)
    done;
    Printf.printf "\n"
  done;;

(** pretty print the environment *)
let dump (Env(pc, m, r ))= 
    printf "-------------------------------------------------------------------\n";
    printf "PC:%d\n" !pc;
    printf "-------------------------------------------------------------------\n";
    dump_reg r; 
    printf "-------------------------------------------------------------------\n";
    dump_mem !m; ();
    printf "-------------------------------------------------------------------\n"
;;
