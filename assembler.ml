open Asm
open Prettyprinter


(** this is used to assign locations to instructions. 
    Processes loc N + ASM instructions only *)
let rec reloc pgm pc =
  match pgm with 
     [] -> []
|    i::ri ->
       (match i with  
         Instr(Locval newpc) -> (reloc ri newpc)
       | Instr(Memloc _ ) -> (reloc ri pc)
       | Instr(Regval _ ) -> (reloc ri pc)
       | Instr(Start _  ) -> (reloc ri pc) 
       | Instr ii -> (RelI (NoLab, ii, pc))::(reloc ri (pc+1))
       | LabInstr (ll,ii) -> (RelI (ll,ii,pc))::(reloc ri (pc+1)));;

(** aux fun to add values to mem **)
let rec addata addr lv mem =
  match lv with
    [] -> mem
  | v::rv -> (addata (addr+1) rv ((addr,v)::mem));;

(** function scanning initial memory assignments:
    returns an associative list  *)
let rec msetup pgm mem = 
  match pgm with 
    [] -> mem
  | Instr(Memloc (l,v))::rpgm -> msetup rpgm ((l,v)::mem)
  | Instr(Data (addr, lv))::rpgm -> msetup rpgm (addata addr lv mem)
| _::rpgm -> msetup rpgm mem
;;

(** function scanning initial register assignments: 
    returns an associative list *)
(* register file *)
let rec rfsetup pgm regs = 
  match pgm with 
    [] -> ()
  | Instr(Regval (l,v))::rpgm -> (regs.(l):=v; rfsetup rpgm regs)
  | _::rpgm -> (rfsetup rpgm regs)
;;

let rec rsetup pgm reg = 
  match pgm with 
    [] -> reg
| Regval (l,v)::rpgm -> rsetup rpgm ((l,v)::reg)
| _::rpgm -> rsetup rpgm reg
;;
  
(** function scanning labels declares in the program
    returns an associative list *)
let rec lsetup pgm labs = 
  match pgm with 
    [] -> labs
|   RelI(NoLab,a,p)::rpgm -> (lsetup rpgm labs)
|   RelI(LabLab(s),a,p)::rpgm -> (lsetup rpgm ((s,p)::labs))
|   _::rpgm -> (lsetup rpgm labs)
;;  

(** looks for pseudo instruction defining the start address *)
let rec ssetup pgm = 
  match pgm with 
    [] -> 0
| Instr(Start(v))::rpgm -> v
| _ :: rpgm -> ssetup rpgm
;;

(** exception error istruction not found *)
exception InstructionNotFound;;

(* get instruction at location loc in pgm *)
let rec get_instr_at loc pgm = 
  match pgm with
    [] -> raise InstructionNotFound
|  RelI(_,i,ll)::rpgm -> if(ll=loc) then i else (get_instr_at loc rpgm)
;;

(** convert labels to offsets in a program *)
let rec labconvert pgm labs = 
  match pgm with
    [] -> []
|   RelI(l,Ifge(ra, rb ,LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Ifge(ra, rb,LabOff(off)),pc)::(labconvert rpgm labs)
|   RelI(l,Ifge0(r,LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Ifge0(r,LabOff(off)),pc)::(labconvert rpgm labs)
|   RelI(l,Ifg(ra,rb,LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Ifg(ra,rb,LabOff(off)),pc)::(labconvert rpgm labs)
|   RelI(l,Ifg0(r,LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Ifg0(r,LabOff(off)),pc)::(labconvert rpgm labs)
|   RelI(l,Ifle(ra,rb,LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Ifle(ra,rb,LabOff(off)),pc)::(labconvert rpgm labs)
|   RelI(l,Ifle0(r,LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Ifle0(r,LabOff(off)),pc)::(labconvert rpgm labs)
|   RelI(l,Ifl(ra,rb,LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Ifl(ra,rb,LabOff(off)),pc)::(labconvert rpgm labs)
|   RelI(l,Ifl0(r,LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Ifl0(r,LabOff(off)),pc)::(labconvert rpgm labs)
|   RelI(l,Ifeq(ra,rb,LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Ifeq(ra,rb,LabOff(off)),pc)::(labconvert rpgm labs)
|   RelI(l,Ifeq0(r,LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Ifeq0(r,LabOff(off)),pc)::(labconvert rpgm labs)
|   RelI(l,Ifneq(ra,rb,LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Ifneq(ra,rb,LabOff(off)),pc)::(labconvert rpgm labs)
|   RelI(l,Ifneq0(r,LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Ifneq0(r,LabOff(off)),pc)::(labconvert rpgm labs)
|   RelI(l,Gotol(LabLab(s)),pc)::rpgm -> 
	let lv = List.assoc s labs in 
	let off = lv - pc in 
        RelI(l,Gotol(LabOff(off)),pc)::(labconvert rpgm labs)
| i::rpgm -> i::(labconvert rpgm labs)
;;

 
let filename = Sys.argv.(1);;
let steps    = 
  try 
    Sys.argv.(2)
  with 
    Invalid_argument(_) -> "1000"
;;

(* File main.ml *)
let _ =
  try
    let lexbuf = Lexing.from_channel (open_in filename) in
    while true do
    (* first step: parse the source *)
    let result = Parser.main Lexer.token lexbuf in
    Printf.printf "=================== Scanned program source:\n";
    pp_program result; 
    (* second step: assign pc to instructions *) 
    let relocp = reloc result 0 in 
    Printf.printf "=================== Relocated program: \n";
    pp_relocp relocp; 
    (* third step: convert and solve labels *)
    let labs = lsetup relocp [] in 
    let newpgm = labconvert relocp labs in 
    Printf.printf "=================== Final program (labels compiled)\n";
    pp_relocp newpgm; 
    (* get initial state *)
    let regs = Array.init 64 (fun x -> ref 0) in
    let mems = msetup result [] in
    let start = ssetup result in 
    Printf.printf "=================== Program starting at %d \n\n\n" start ;
    let env  = Env(ref start, ref mems, regs) in
      rfsetup result regs; dump env; 
      Printf.printf "Code dump\n";
      Prettyprinter.pp_relocp newpgm;
      match env with 
        Env(pc,m,r) -> 
          for i=0 to (int_of_string steps) do
            let istr = get_instr_at !pc newpgm in 
            Printf.printf "\n\nExecuting instruction ==> %d: " !pc ; pp_asm istr ; 
		Printf.printf "\n";
	    Interp.step istr env; dump env
          done;
    done
   with 
   | Lexer.Eof -> exit 0 
   | Parsing.Parse_error -> Printf.printf "Parse Error \n\n\n" 
