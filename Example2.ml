open List 
let rec gcd i j = 
  if i <> 0 
  then if j > i
    then gcd i (j- i)
    else gcd (i - j) j
  else j ;; 
  
let rat n d = 
  let g = gcd n d in
  let p = n / g in 
  let c = d / g in 
    (p,c);;
let ratAdd a b =
  let top = fst a * snd b + snd a * fst b in 
  let bot = snd a * snd b in 
  rat top bot;;
  
let ratMul a b = 
  let top = fst a * fst b in 
  let bot = snd a * snd b in 
  rat top bot;; 
let ratDiv a b = 
  let top =  fst a *snd b in 
  let bot = snd a * fst b in 
  rat top bot;; 
let ratGt a b = 
  let left = fst a * snd b in 
  let right = snd a * fst b in 
  if left > right 
    then true 
  else false  ;; 
let rec eulering c s t = 
  let one = rat 1 1 in 
  let num = rat 1 100000 in 
  if ratGt t num 
    then let s' = ratAdd t s in
    let t' = ratDiv t c in 
    let c' = ratAdd c one in 
   
    eulering c' s' t'
  else s   
let euler ()=
  eulering (1,1) (0,1) (1,1);;
  


(* RAT PRINT. Print a pair (N, D) as the fraction N / D. You don't have to know
   how this works. *)

let ratPrint (n, d) =
  Printf.printf "%i / %i\n" n d ;;

(* BOOL PRINT. Print a BOOL B. You don't have to know how this works either. *)

let boolPrint b =
  Printf.printf "%b\n" b ;;

(* Test the rational arithmetic functions. *)

ratPrint (rat 1 2) ;;                                       (* 2 pts: 1 / 2 *)

ratPrint (rat 10 20) ;;                                     (* 2 pts: 1 / 2 *)

ratPrint (ratAdd (rat 1 2) (rat 1 2)) ;;                    (* 2 pts: 1 / 1 *)

ratPrint (ratAdd (rat 1 3) (rat 1 2)) ;;                    (* 2 pts: 5 / 6 *)

ratPrint (ratMul (rat 1 2) (rat 10 1)) ;;                   (* 2 pts: 5 / 1 *)

ratPrint (ratMul (rat 2 3) (rat 4 5)) ;;                    (* 2 pts: 8 / 15 *)

ratPrint (ratDiv (rat 1 2) (rat 10 2)) ;;                   (* 2 pts: 1 / 10 *)

ratPrint (ratDiv (rat 1 2) (rat 1 3)) ;;                    (* 2 pts: 3 / 2 *)

boolPrint (ratGt (rat 1 2) (rat 1 3)) ;;                    (* 2 pts: true *)

boolPrint (ratGt (rat 1 3) (rat 1 2)) ;;                    (* 2 pts: false *)

(* The big finish. Compute E. *)

ratPrint (euler ()) ;;                             (* 20 pts: 109601 / 40320 *)

(*# #use "Example2.ml";;
val gcd : int -> int -> int = <fun>
val rat : int -> int -> int * int = <fun>
val ratAdd : int * int -> int * int -> int * int = <fun>
val ratMul : int * int -> int * int -> int * int = <fun>
val ratDiv : int * int -> int * int -> int * int = <fun>
val ratGt : int * int -> int * int -> bool = <fun>
val eulering : int * int -> int * int -> int * int -> int * int = <fun>
val euler : unit -> int * int = <fun>
# #use "tests2.ml";;
val ratPrint : int * int -> unit = <fun>
val boolPrint : bool -> unit = <fun>
1 / 2
- : unit = ()
1 / 2
- : unit = ()
1 / 1
- : unit = ()
5 / 6
- : unit = ()
5 / 1
- : unit = ()
8 / 15
- : unit = ()
1 / 10
- : unit = ()
3 / 2
- : unit = ()
true
- : unit = ()
false
- : unit = ()
109601 / 40320
- : unit = ()
# *)


