(*

  This file contains definitions for the OCaml type THING, and the OCaml module
  SCANNER, both of which are needed to write the module PARSER for Project 2.

*)

(* THING. Types of the usual Lisp objects. *)

type
  thing =
    Closure of thing * thing * environment |
    Cons of thing * thing |
    Nil |
    Number of int |
    Primitive of (thing -> environment -> thing) |
    Symbol of string
and
  environment = (string * thing) list ;;

(* SCANNER. Lexical scanner for Lisp from Lab 9. It also ignores comments. *)

module Scanner =
struct

(* TOKEN. A token for an expression in a subset of Lisp. *)

  type token =
    CloseParenToken |
    EndToken |
    NumberToken of int |
    OpenParenToken |
    SymbolToken of string ;;

(* MAKE SCANNER. Return a version of the scanner function NEXT TOKEN that reads
   TOKENs from a file whose pathname is the string PATH. INPUT is a channel
   connected to the file. CH holds the most recently read CHAR from INPUT. *)

  let makeScanner path =
    let input = open_in path
    in let ch = ref ' '
       in

(* NEXT CHAR. Advance CH to the next CHAR from INPUT. If there is no next CHAR,
   then set CH to '\000'. We use this CHAR to represent the end of a file. We'd
   like to give this CHAR a name, but then we couldn't MATCH on that name. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file ->
           ch := '\000'
  in

(* NEXT CLOSE PAREN TOKEN. Read a CLOSE PAREN TOKEN. *)

  let nextCloseParenToken () =
    nextChar () ;
    CloseParenToken
  in

(* NEXT COMMENT. Skip a comment. It starts with a ';' and ends with a newline
   '\n' or an end of file '\000'. We skip the '\n', but not the '\000'. *)

  let rec nextComment () =
    match ! ch
    with '\000' ->
           () |
         '\n' ->
           nextChar () |
         _ ->
           nextChar () ;
           nextComment ()
  in

(* NEXT END TOKEN. Read an END TOKEN. We don't skip a CHAR because there are no
   more CHARs to skip. *)

  let nextEndToken () =
    EndToken
  in

(* NEXT NUMBER TOKEN. Read a NUMBER TOKEN that starts with PREFIX. *)

  let nextNumberToken prefix =
    let rec nextNumbering chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             NumberToken (int_of_string chars) |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextNumbering (chars ^ otherChars)
    in nextNumbering prefix
  in

(* NEXT OPEN PAREN TOKEN. Read an OPEN PAREN TOKEN. *)

  let nextOpenParenToken () =
    nextChar () ;
    OpenParenToken
  in

(* NEXT SYMBOL TOKEN. Read a SYMBOL TOKEN that starts with PREFIX. *)

  let nextSymbolToken prefix =
    let rec nextSymboling chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             SymbolToken chars |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextSymboling (chars ^ otherChars)
    in nextSymboling prefix
  in

(* NEXT NUMBER OR SYMBOL TOKEN. We've just read a '-', but we don't know yet if
   it starts a NUMBER TOKEN or a SYMBOL token. Skip the '-'. If we see a digit,
   then it starts a NUMBER TOKEN, otherwise it starts a SYMBOL TOKEN. *)

  let nextNumberOrSymbolToken () =
    nextChar () ;
    match ! ch
    with '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "-" |
         _ ->
           nextSymbolToken "-"
  in

(* NEXT TOKEN. Look at CH to tell what TOKEN is coming next. Dispatch to the
   function that will read that TOKEN and return it. *)

  let rec nextToken () =
    match ! ch
    with '\000' ->
           nextEndToken () |
         ' ' | '\n' ->
           nextChar () ;
           nextToken () |
         '(' ->
           nextOpenParenToken () |
         ')' ->
           nextCloseParenToken () |
         ';' ->
           nextComment () ;
           nextToken () |
         '-' ->
           nextNumberOrSymbolToken () |
         '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "" |
         _ ->
           nextSymbolToken ""

(* Lost? This is MAKE SCANNER's body. Initialize CH by reading the NEXT CHAR,
   and return (but do not call!) NEXT TOKEN. *)

  in nextChar () ;
     nextToken ;;

end ;;
module type Parsers = (* the signature including makeParser and the exception can'tParse *) 
sig 
    val makeParser: string->unit->thing
    exception Can'tParse of string 

end ;; 

module Parser:Parsers = (* the module parser with type parsers that contains the definition of can'tParse along with the functions makeParser, nextThing, and nextThings *)
  struct 
    exception Can'tParse of string ;;  
    
    let makeParser path = (* looked at makeScanner as a blueprint for makeParser nextToken is the next token in the Scanner. Token is the current Token which is set to an EndToken to begin with  *)
      let nextToken = Scanner.makeScanner path 
      in let token = ref Scanner.EndToken
        in
    let rec  nextThing()= (* dispatcher that will read the Token and tell us what to do with it *)
      match !token 
      with Scanner.CloseParenToken -> raise(Can'tParse "closed parenthesis can't parse")|(*what to do with close parenthesis, raises a can'tParse with a string telling us why*)
           Scanner.EndToken -> raise (Can'tParse "End Token can't parse")|(*what to do with an EndToken raises a Can'tParse with a string telling us why*)
           Scanner.NumberToken number -> token:=nextToken();Number number | (*what to do with a NumberToken. first sets the token to the next token then returns the Number*)
           Scanner.OpenParenToken -> token :=nextToken(); nextThings()|(*what to do with a OpenParenToken. first sets the token to the next token then calls nextThings to break up the Parenthesis *) 
           Scanner.SymbolToken "nil" -> token:=nextToken(); Nil |(*what to do with a nil. first sets token to the next token then returns Nil*)
           Scanner.SymbolToken symbol -> token:=nextToken(); Symbol symbol (* what to do with a SymbolToken. first changes token to the next token then returns the symbol*)
            
    and nextThings()=(*helper function to help nextThing parse Lisp list. must use and becausue NextThings and nextThing both call each other and ocaml is strict define before use.  *)
      match !token (*match on token*)
      with Scanner.CloseParenToken -> token:=nextToken() ;Nil|(* if the token is a ClosedParenToken first token is set to next token and then we return Nil*)
           Scanner.EndToken->raise (Can'tParse "Open parenthesis without a closed parenthesis")|(* if we get an EndToken before we get a CloseParenToken we have a problem. we need matching parenthesis so this raises a Can'tParse with a string telling us the problem.  *)
           _-> let left = nextThing() in (*takes the left value and sends it though the nextThing disbatcher to see what it is. returns the appropriate thing and puts it at the front*)
                  let right = nextThings() in (* takes everything else and sends it recursively back to NextThings. this continues until we get a CloseParenToken or EndToken *)
                    Cons(left,right)(* puts the thing we get from the left and the thing we get from the right together. recursively builds a Lisp list of cons put together using both nextThing and nextThings.*) 
    in token:= nextToken();nextThing; end;;
       
     
(*
results from running parser.ml and the test file called testsP2.ml 
# #use "parser.ml";;
type thing =
    Closure of thing * thing * environment
  | Cons of thing * thing
  | Nil
  | Number of int
  | Primitive of (thing -> environment -> thing)
  | Symbol of string
and environment = (string * thing) list
module Scanner :
  sig
    type token =
        CloseParenToken
      | EndToken
      | NumberToken of int
      | OpenParenToken
      | SymbolToken of string
    val makeScanner : string -> unit -> token
  end
module type Parsers =
  sig
    val makeParser : string -> unit -> thing
    exception Can'tParse of string
  end
module Parser : Parsers
# #use "testsP2.ml";;
val nextThing : unit -> thing = <fun>
- : thing = Nil
- : thing = Number 7734
- : thing = Symbol "lobyms"
- : thing = Cons (Symbol "a", Nil)
- : thing = Cons (Symbol "a", Cons (Symbol "b", Nil))
- : thing = Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil)))
- : thing =
Cons (Cons (Symbol "a", Nil), Cons (Symbol "b", Cons (Symbol "c", Nil)))
- : thing =
Cons (Cons (Symbol "a", Cons (Symbol "b", Nil)), Cons (Symbol "c", Nil))
- : thing =
Cons (Symbol "a", Cons (Cons (Symbol "b", Cons (Symbol "c", Nil)), Nil))
- : thing =
Cons (Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil))), Nil)
- : thing =
Cons (Symbol "define",
 Cons (Symbol "!",
  Cons
   (Cons (Symbol "lambda",
     Cons (Cons (Symbol "n", Nil),
      Cons
       (Cons (Symbol "if",
         Cons (Cons (Symbol "=", Cons (Symbol "n", Cons (Number 0, Nil))),
          Cons (Number 1,
           Cons
            (Cons (Symbol "*",
              Cons (Symbol "n",
               Cons
                (Cons (Symbol "!",
                  Cons
                   (Cons (Symbol "-",
                     Cons (Symbol "n", Cons (Number 1, Nil))),
                   Nil)),
                Nil))),
            Nil)))),
       Nil))),
   Nil)))
Exception: Parser.Can'tParse "End Token can't parse".

*)
