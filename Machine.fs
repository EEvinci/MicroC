(* File MicroC/Machine.fs 

   Instructions and code emission for a stack-based
   abstract machine * sestoft@itu.dk 2009-09-23

   Implementations of the machine are found in file MicroC/Machine.java 
   and MicroC/machine.c.
   
   Must precede Comp.fs and Contcomp.fs in the VS Solution Explorer.
 *)

module Machine

type label = string
// 汇编指令
type instr =
  | Label of label                     (* symbolic label; pseudo-instruc. *)
  | FLabel of int * label                     (* symbolic label; pseudo-instruc. *)
  | CSTI of int                        (* constant                        *)
  | CSTC of int32
  | CSTS of int32 list
  | OFFSET of int                        (* constant     偏移地址  x86     *) 
  | GVAR of int                        (* global var     全局变量  x86     *) 
  | ADD                                (* addition                        *)
  | SUB                                (* subtraction                     *)
  | MUL                                (* multiplication                  *)
  | DIV                                (* division                        *)
  | MOD                                (* modulus                         *)
  | EQ                                 (* equality: s[sp-1] == s[sp]      *)
  | LT                                 (* less than: s[sp-1] < s[sp]      *)
  | NOT                                (* logical negation:  s[sp] != 0   *)
  | DUP                                (* duplicate stack top             *)
  | SWAP                               (* swap s[sp-1] and s[sp]          *)
  | LDI                                (* get s[s[sp]]                    *)
  | STI                                (* set s[s[sp-1]]                  *)
  | GETBP                              (* get bp                          *)
  | GETSP                              (* get sp                          *)
  | INCSP of int                       (* increase stack top by m         *)
  | GOTO of label                      (* go to label                     *)
  | IFZERO of label                    (* go to label if s[sp] == 0       *)
  | IFNZRO of label                    (* go to label if s[sp] != 0       *)
  | CALL of int * label                (* move m args up 1, push pc, jump *)
  | TCALL of int * int * label         (* move m args down n, jump        *)
  | RET of int                         (* pop m and return to s[sp]       *)
  | PRINTI                             (* print s[sp] as integer          *)
  | PRINTC                             (* print s[sp] as character        *)
  | LDARGS of int                             (* load command line args on stack *)
  | STOP                               (* halt the abstract machine       *)

(* Generate new distinct labels *)

// 返回两个函数 resetLabels , newLabel
let (resetLabels, newLabel) = 
    let lastlab = ref -1
    ((fun () -> lastlab := 0), (fun () -> (lastlab := 1 + !lastlab; "L" + (!lastlab).ToString())))

(* Simple environment operations *)

type 'data env = (string * 'data) list

let rec lookup env x = 
    match env with 
    | []         -> failwith (x + " not found")
    | (y, v)::yr -> if x=y then v else lookup yr x

(* An instruction list is emitted in two phases:
   * pass 1 builds an environment labenv mapping labels to addresses 
   * pass 2 emits the code to file, using the environment labenv to 
     resolve labels
 *)

(* These numeric instruction codes must agree with Machine.java: *)



//机器码

//[<Literal>] 属性可以让 
//该变量在模式匹配时候被匹配,否则匹配时只能用数值.不能用变量名



[<Literal>]
let CODECSTI   = 0 


[<Literal>]
let CODEADD    = 1 


[<Literal>]
let CODESUB    = 2 


[<Literal>]
let CODEMUL    = 3 

[<Literal>]
let CODEDIV    = 4 

[<Literal>]
let CODEMOD    = 5 

[<Literal>]
let CODEEQ     = 6 

[<Literal>]
let CODELT     = 7 

[<Literal>]
let CODENOT    = 8 

[<Literal>]
let CODEDUP    = 9 

[<Literal>]
let CODESWAP   = 10 

[<Literal>]
let CODELDI    = 11 


[<Literal>]
let CODESTI    = 12 


[<Literal>]
let CODEGETBP  = 13 


[<Literal>]
let CODEGETSP  = 14 


[<Literal>]
let CODEINCSP  = 15 


[<Literal>]
let CODEGOTO   = 16


[<Literal>]
let CODEIFZERO = 17

[<Literal>]
let CODEIFNZRO = 18 



[<Literal>]
let CODECALL   = 19


[<Literal>]
let CODETCALL  = 20

[<Literal>]
let CODERET    = 21


[<Literal>]
let CODEPRINTI = 22 

[<Literal>]
let CODEPRINTC = 23


[<Literal>]
let CODELDARGS = 24

[<Literal>]
let CODESTOP   = 25;

[<Literal>]
let CODECSTC    = 27;

[<Literal>]
let CODECSTS    = 28;

