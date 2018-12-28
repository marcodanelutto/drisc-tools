open Printf
open Asm

exception EndOfProgram;;

(** execute one instruction within an environment 
    @param i the instruction to be executed  **)
let step i (Env(pc,m,r)) = 
  
  match i with 
|   Add(Reg(a),Reg(b),Reg(c)) -> r.(c) := !(r.(a)) + !(r.(b));pc:= !pc+1
|   And(Reg(a),Reg(b),Reg(c)) -> r.(c) := !(r.(a)) land !(r.(b));pc:= !pc+1
|   Sub(Reg(a),Reg(b),Reg(c)) -> r.(c) := !(r.(a)) - !(r.(b));pc:= !pc+1
|   Mul(Reg(a),Reg(b),Reg(c)) -> r.(c) := !(r.(a)) * !(r.(b));pc:= !pc+1
|   Div(Reg(a),Reg(b),Reg(c)) -> r.(c) := !(r.(a)) / !(r.(b));pc:= !pc+1
|   Mod(Reg(a),Reg(b),Reg(c)) -> r.(c) := !(r.(a)) mod !(r.(b));pc:= !pc+1
|   Addi(Reg(a),Const(b),Reg(c)) -> r.(c) := !(r.(a)) + b;pc:= !pc+1
|   Muli(Reg(a),Const(b),Reg(c)) -> r.(c) := !(r.(a)) * b;pc:= !pc+1
|   Divi(Reg(a),Const(b),Reg(c)) -> r.(c) := !(r.(a)) / b;pc:= !pc+1
|   Andi(Reg(a),Const(b),Reg(c)) -> r.(c) := !(r.(a)) land b;pc:= !pc+1
|   Modi(Reg(a),Const(b),Reg(c)) -> r.(c) := !(r.(a)) mod b;pc:= !pc+1
|   Subi(Reg(a),Const(b),Reg(c)) -> r.(c) := !(r.(a)) - b;pc:= !pc+1
|   Incr(Reg(a)) -> r.(a) := !(r.(a))+1; pc:= !pc +1
|   Decr(Reg(a)) -> r.(a) := !(r.(a))-1; pc:= !pc +1
|   Load(Reg(a),Reg(b),Reg(c)) -> 
	let ind = !(r.(a)) + !(r.(b)) in 
	  r.(c) := (List.assoc ind !m); pc := !pc + 1
|   Loadi(Reg(a),Const(b),Reg(c)) -> 
	let ind = !(r.(a)) + b in 
	  r.(c) := (List.assoc ind !m); pc := !pc + 1
|   Store(Reg(a),Reg(b),Reg(c)) -> 
        let ind = !(r.(a)) + !(r.(b)) in
        let mm = List.remove_assoc ind !m in 
          m := (ind, !(r.(c)))::mm ;  pc := !pc + 1
|   Storei(Reg(a),Const(b),Reg(c)) -> 
        let ind = !(r.(a)) + b in
        let mm = List.remove_assoc ind !m in 
          m:= (ind, !(r.(c)))::mm; pc := !pc + 1
|   Call(Reg(f), Reg(ret)) -> 
	r.(ret):= !pc + 1;
 	pc := !(r.(f))
|   Gotor(Reg(l)) ->  pc := !(r.(l))
|   Gotol(LabOff(ll)) ->  
           pc := !pc + ll
|   Ifle(Reg(r1),Reg(r2),LabOff(l)) ->
       if(!(r.(r1)) <= !(r.(r2))) 
       then pc := !pc + l
       else pc := !pc + 1
|   Ifl0(Reg(r1),LabOff(l)) ->
       if(!(r.(r1)) <= 0)
       then pc := !pc + l
       else pc := !pc + 1
|   Ifl(Reg(r1),Reg(r2),LabOff(l)) ->
       if(!(r.(r1)) < !(r.(r2))) 
       then pc := !pc + l 
       else pc := !pc + 1
|   Ifge(Reg(r1),Reg(r2),LabOff(l)) ->
       if(!(r.(r1)) >= !(r.(r2))) 
       then pc := !pc + l 
       else pc := !pc + 1
|   Ifg(Reg(r1),Reg(r2),LabOff(l)) ->
       if(!(r.(r1)) > !(r.(r2))) 
       then pc := !pc + l 
       else pc := !pc + 1
|   Ifeq(Reg(r1),Reg(r2),LabOff(l)) ->
       if(!(r.(r1)) = !(r.(r2))) 
       then pc := !pc + l 
       else pc := !pc + 1
|   Ifeq0(Reg(r1),LabOff(l)) ->
       if(!(r.(r1)) =  0)
       then pc := !pc + l 
       else pc := !pc + 1
|   Ifneq(Reg(r1),Reg(r2),LabOff(l)) ->
       if(not(!(r.(r1)) = !(r.(r2)))) 
       then pc := !pc + l 
       else pc := !pc + 1
|   End -> raise EndOfProgram
| _ -> printf "UNIMPLEMENTED:"; Prettyprinter.pp_asm i
;;
