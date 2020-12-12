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
  | [] -> res
  | (x::xs) -> aux xs (res + (lettre_score x))

let word_score (word : word) : int =
  aux word.word 0
  

let fitness st word =
  (* ignoring unused variables - to be removed *)
  ignore st ;
  ignore word ;
  ignore word_score ;
  (* end ignoring unused variables - to be removed *)
  (* TODO *)
  assert false

(* TODO *)

let rec headAux (level:int) (words : word list) (meilleurScore : int) (meilleurMot : word option) : word option = 
  (* Log.log_info "CURRENT BEST SCORE%i@." meilleurScore; *)
  match words with
  | [] -> meilleurMot
  | (x::xs) -> let newScore = word_score x in
                (* Log.log_info "NEW SCORE %i@." newScore;
                Log.log_info "NEW WORD%a@." Word.pp x; *)
               if (level = x.level) && ((word_score x) > meilleurScore) 
                          then headAux level xs newScore (Some x)
                          else headAux level xs meilleurScore meilleurMot


let rec get_latest_period (words : word list) (latestPeriod : int) : int =
  match words with
  | [] -> latestPeriod
  | x::xs -> if x.level > latestPeriod then get_latest_period xs x.level else get_latest_period xs latestPeriod


let head ?level (st : Store.word_store) : word option  =
  match (List.of_seq (Hashtbl.to_seq_values (st.words_table))), level with
  | _,None -> None
  | [], _ -> Log.log_info "EMPTY WORD STORE@." ; Some genesis_word
  | liste, Some l -> ignore l; (*Log.log_info "GETTING BEST WORD@." ;*) headAux (get_latest_period liste 0) liste 0 (Some genesis_word)
  

  