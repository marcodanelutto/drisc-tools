open Asm
open Printf

(** returns the domain of an instruction *)
let domain = function 
   Add(a,b,c) -> [a;b]
|  Sub(a,b,c) -> [a;b]
|  And(a,b,c) -> [a;b]
|  Mul(a,b,c) -> [a;b]
|  Div(a,b,c) -> [a;b]
|  Mod(a,b,c) -> [a;b]
|  Addi(a,b,c) -> [a]
|  Andi(a,b,c) -> [a]
|  Subi(a,b,c) -> [a]
|  Muli(a,b,c) -> [a]
|  Divi(a,b,c) -> [a]
|  Modi(a,b,c) -> [a]
|  Incr(a) -> [a]
|  Decr(a) -> [a]
|  Load(a,b,c) -> [a;b]
|  Store(a,b,c) -> [a;b;c]
|  Loadi(a,b,c) -> [a]
|  Storei(a,b,c) -> [a;c]
|  Call(a,b) -> [a]
|  Gotor(a) -> [a]
|  Gotol(a) -> []
|  Ifle(a,b,c) -> [a;b]
|  Ifle0(b,c) -> [b]
|  Ifl(a,b,c) -> [a;b]
|  Ifl0(b,c) -> [b]
|  Ifge(a,b,c) -> [a;b]
|  Ifge0(b,c) -> [b]
|  Ifg(a,b,c) -> [a;b]
|  Ifg0(b,c) -> [b]
|  Ifeq(a,b,c) -> [a;b]
|  Ifeq0(b,c) -> [b]
|  Ifneq(a,b,c) -> [a;b]
|  Ifneq0(b,c) -> [b]
|  End -> []
|  Memloc(l,v) -> []
|  Start(v) -> []
|  Locval(v) -> []
|  Regval(r,v) -> []
|  _ -> (Printf.printf "domain: unmatched instruction\n"); []
;;
	
(** returns the range of an instruction *)
let codomain = function 
   Add(a,b,c) -> [c]
|  Sub(a,b,c) -> [c]
|  And(a,b,c) -> [c]
|  Mul(a,b,c) -> [c]
|  Div(a,b,c) -> [c]
|  Mod(a,b,c) -> [c]
|  Addi(a,b,c) -> [c]
|  Andi(a,b,c) -> [c]
|  Subi(a,b,c) -> [c]
|  Muli(a,b,c) -> [c]
|  Divi(a,b,c) -> [c]
|  Modi(a,b,c) -> [c]
|  Incr(a) -> [a]
|  Decr(a) -> [a]
|  Load(a,b,c) -> [c]
|  Store(a,b,c) -> []
|  Loadi(a,b,c) -> [c]
|  Storei(a,b,c) -> []
|  Call(a,b) -> []
|  Gotor(a) -> []
|  Gotol(a) -> []
|  Ifle(a,b,c) -> []
|  Ifle0(b,c) -> []
|  Ifl(a,b,c) -> []
|  Ifl0(b,c) -> []
|  Ifge(a,b,c) -> []
|  Ifge0(b,c) -> []
|  Ifg(a,b,c) -> []
|  Ifg0(b,c) -> []
|  Ifeq(a,b,c) -> []
|  Ifeq0(b,c) -> []
|  Ifneq(a,b,c) -> []
|  Ifneq0(b,c) -> []
|  Memloc(l,v) -> []
|  Regval(r,v) -> []
|  Start(v) -> []
|  Locval(v) -> []
|  End -> []
|  _ -> (Printf.printf "domain: unmatched instruction\n"); []
;;

(** function computing the intersection of two lists. 
    This is used to compute Bernstein conditions *)
let intersect l1 l2 = 
  let a1 = Array.of_list l1 in
  let a2 = Array.of_list l2 in
  let res = ref [] in 
  let n1 = Array.length a1 in 
  let n2 = Array.length a2 in 
  for i=0 to (n1-1) do 
    for j=0 to (n2-1) do 
      if(a1.(i) = a2.(j))
      then res := a2.(j) :: !res 
    done
  done; 
  !res
;;

(** checks if an instruction is "executed" on the IU *)
let iu_instruction = function
    Ifle(a,b,c) -> true
  | Ifl(a,b,c)  -> true
  | Ifge(a,b,c) -> true
  | Ifg(a,b,c)  -> true
  | Ifeq(a,b,c)  -> true
  | Ifneq(a,b,c) -> true
  | Load(a,b,c)    -> true
  | Loadi(a,b,c)   -> true
  | Store(a,b,c)    -> true
  | Storei(a,b,c)   -> true
  | _ -> false
;;

(** data dependency: 
	instructions inducing the data dependencies
	interested register(s) 
	"distance"
	"N"
*) 
type datadep = 
  NoDataDep 
| DataDep of int * asm * int * asm * reg list * int * int ;;

(** removes labels from an instruction, if present, 
    and returns the assembler instruction only *)
let delab = function
  LabInstr(l,i) -> i 
| Instr(i) -> i;;

(** check whether there is a data dependency among instructions
    @param a1 the address of the first instruction
    @param a2 the address of the second instruction
    @param li1 the first instruction 
    @param li2 the second instruction 
    @param dist the distance between instructions 
    @param n the N parameter
*)
let data_dep_i a1 a2 li1 li2 dist n = 
  let i1 = delab li1 in 
  let i2 = delab li2 in
  let wrs = codomain(i1) in
  let rds = domain(i2) in 
  let regset = (intersect rds wrs) in 
  if(iu_instruction i2 && not(regset = []))
    then DataDep(a1,i1,a2,i2,(intersect rds wrs),dist,n)
    else NoDataDep;;

(** checks whether there is a load in the sequence 
    leading to the dependency 
    @param i1 the starting point of the sequence
    @param i2 the ending point of the sequence 
    @param prog the program with the sequence *)
let loadsinsequence prog i1 i2 = 
  let aprog = Array.of_list prog in 
  let bign  = ref false in 
  for i=i1 to (i2-1) do
    let asmi = match aprog.(i) with 
	    	Instr(ai) -> ai
	       |LabInstr(l,ai) -> ai in
    bign := match asmi with 
		Load(a,b,c) -> true
	    |   Loadi(a,b,c) -> true
	    |   _ -> !bign
  done; 
  !bign
;; 

(** finds all data dependencies in a program 
    @param prog the program *)
let data_deps prog = 
  let aprog = Array.of_list prog in
  let n     = Array.length aprog in 
  let res   = ref [] in
  let start = ref 0 in 
  for i=0 to (n-2) do
    for j=(i+1) to (n-1) do
      let i1 = aprog.(i) in 
      let i2 = aprog.(j) in 
      let dd = (data_dep_i i j i1 i2 (j-i) 0) in (* N=0 TODO *)
        match dd with 
          NoDataDep -> ()
        | DataDep(i,i1,j,i2,regs,dist,n) -> 
	    let hasloads = loadsinsequence prog !start (j-1) in
   	    let bign = if(hasloads) then 2 else 1 in
            let dd = DataDep(i,i1,j,i2,regs,dist,bign) in

	    start := j;
	    res := (List.append (!res) [dd])
    done
  done;
  !res
;;

(** bernstein conditions check *)
let bernstein i1 i2 = 
  let d1 = domain i1 in 
  let d2 = domain i2 in 
  let c1 = codomain i1 in 
  let c2 = codomain i2 in
  let d1c2 = intersect d1 c2 in 
  let d2c1 = intersect d2 c1 in 
  if(d1c2 = [] && d2c1 = []) 
  then true
  else false
;;


(** transforms a list of instructions (with labels) into  
    a list of assembler instruction (with no labels) *)
let rec prog_to_asm p = 
  List.map delab p;;
