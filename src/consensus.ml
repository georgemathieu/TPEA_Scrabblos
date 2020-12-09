open Word
open Constants
open Letter

(* English rules *)
let lettre_score l : int =
  match l.letter with
  | 'a' -> 1
  | 'b' -> 3
  | 'c' -> 3
  | 'd' -> 2
  | 'e' -> 1
  | 'f' -> 4
  | 'g' -> 2
  | 'h' -> 4
  | 'i' -> 1
  | 'j' -> 8
  | 'k' -> 5
  | 'l' -> 1
  | 'm' -> 3
  | 'n' -> 1
  | 'o' -> 1
  | 'p' -> 3
  | 'q' -> 10
  | 'r' -> 1
  | 's' -> 1
  | 't' -> 1
  | 'u' -> 1
  | 'v' -> 4
  | 'w' -> 4
  | 'x' -> 8
  | 'y' -> 4
  | 'z' -> 10
  | _ -> 0

let rec aux (wordContent : letter list) res : int =
  match wordContent with
  | [] -> 0
  | (x::xs) -> aux xs (res + (lettre_score x))

let word_score (word : word) : int =
  aux word.word 0
  

let fitness st word =
(* score àchaque mot de la chaine*)
  (* ignoring unused variables - to be removed *)
  ignore st ;
  ignore word ;
  ignore word_score ;
  (* end ignoring unused variables - to be removed *)
  (* TODO *)
  assert false

(* TODO *)

let rec headAux (level:int) (words : word list) (meilleurScore : int) (meilleurMot : word option) : word option = 
  match words with
  | [] -> meilleurMot
  | (x::xs) -> let newScore = word_score x in
               if (level = x.level) && ((word_score x) > meilleurScore) 
                          then headAux level xs newScore (Some x)
                          else headAux level xs meilleurScore meilleurMot


let head ?level (st : Store.word_store) : word option  =
  match level with
  | None -> None
  | Some l -> if (l = 0) then Some genesis_word else headAux l (List.of_seq (Hashtbl.to_seq_values (st.words_table))) 0 None
  

  