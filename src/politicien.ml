(* open Messages *)
open Word
open Crypto
open Letter

let newline = 13 |> Char.chr

(* print a list of letter, used for debug *)
let rec print_letter_list (l : letter list) =
  match l with
  | [] -> print_string " "
  | x::xs -> print_char x.letter ; print_letter_list xs

(* casts a string to a char list *)
let string_to_char_list (str : string) : char list = str |> String.to_seq |> List.of_seq

(* the list of dictionnary used *)
let dictionnaryList = [Client_utils.list_of_dict "dict/dict_100000_1_10.txt" ; Client_utils.list_of_dict "dict/dict_100000_5_15.txt";
                       Client_utils.list_of_dict "dict/dict_100000_25_75.txt" ; Client_utils.list_of_dict "dict/dict_100000_50_200.txt";
                       Client_utils.list_of_dict "dict/dict_test.txt"]

(* get a dictionnary from the list *)
let get_dictionnary (i : int) : string list = List.nth dictionnaryList i

(* Vérifie que la lettre courante est dans le letter store *)
let rec check_letter_in_store (c : char) (lStore : letter list) : letter option =
  (* Log.log_info "IN CHECK LETTER IN STORE @." ; *)
  match lStore with
  | [] -> (* Log.log_info "EMPTY LETTER STORE @." ; *) None
  | x::xs -> if (x.letter = c) then Some x else check_letter_in_store c xs

(* Vérifie que le mot courant a toutes ses lettres dans le letter store *)
let rec check_dict_word_valid (w : char list) (lStore : letter list) (word : letter list) : letter list option =
  (* Log.log_info "IN CHECK DICT WORD VALID @." ;
  print_letter_list word; *)
  match w with
  | [] -> Some word
  | x::xs -> (*Log.log_info "CURRENT CHAR : %i@." (Char.code x) ;*)  if (x = newline) then Some word else 
              match (check_letter_in_store x lStore) with
              | None -> None
              | Some z -> check_dict_word_valid xs (List.filter (fun (a:letter) -> if a = z then false else true) lStore) (word@[z])
              

(* Recupere un mot dans le dictionnaire en utilisant le contenu du letter store *)
let rec get_word_from_dict_aux (dico : string list) (lStore : letter list) : letter list option =
  (* Log.log_info "IN GET WORD FROM DICT AUX @." ; *)
  match dico with
  | [] -> None
  | x::xs -> match check_dict_word_valid (string_to_char_list x) lStore [] with
              | None -> (* Log.log_info "WORD NOT VALID @." ;*) get_word_from_dict_aux xs lStore
              | Some l -> (*Log.log_info "FOUND WORD @." ;*) Some l

(* Choisis le dictionnaire à utiliser pour construire un mot à partir du letter store *)
let get_word_from_dict (lStore : letter list) : letter list option =
  let l = List.length lStore in
  if (1 < l) && (l < 5) then get_word_from_dict_aux (get_dictionnary 0) lStore
  else None

type politician = { sk : Crypto.sk; pk : Crypto.pk } [@@deriving yojson, show]

type state = {
  politician : politician;
  word_store : Store.word_store;
  letter_store : Store.letter_store;
  next_words : word list;
}

let make ~(politicien : politician) ~(word : letter list) ~(head : hash) ~(level : int)  =
  (* Create message *)
  let msg = Word.pre_bigstring ~word ~level ~head ~pk:politicien.pk in
  (* Build signature *)
  let signature = Crypto.sign ~sk:politicien.sk ~msg in
  Log.log_info "IN MAKE MAKE" ;
  {word ; Word.level ; head ; politician=politicien.pk ; signature}


let make_word_on_hash level letters politician head_hash : word =
  let head = head_hash in
  (*Log.log_info "IN MAKE WORD HASH@." ;*)
  make ~word:letters ~level ~politicien:politician ~head

let make_word_on_blockletters level letters politician head : word =
  let head_hash = Crypto.hash head in
  (* Log.log_info "IN MAKE WORD BLOCK@." ;*)
  make_word_on_hash level letters politician head_hash

(* fonction auxiliaire pour check_letters_one_per_author *)
let rec check_letters_one_per_author_aux (lStore : letter list) (author : Id.author_id) : letter list = 
  match lStore with
  | [] -> []
  | x::xs -> if (x.author = author) then check_letters_one_per_author_aux xs author else x::(check_letters_one_per_author_aux xs author)

(* Supprime les lettres qui appartiennent au meme auteur*)
(* A modifier pour roue libre *)
let rec check_letters_one_per_author (lStore : letter list) : letter list =
  match lStore with
  | [] -> []
  | x::xs -> x::(check_letters_one_per_author (check_letters_one_per_author_aux xs x.author))

(* Supprime les lettres non utilisees d'un ancien niveau *)
let rec deletes_letters_from_old_level (lStore : letter list) (level : int) : letter list =
  match lStore with
  | [] -> []
  | x::xs -> if x.level < level then deletes_letters_from_old_level xs level
                                else x::(deletes_letters_from_old_level xs level)

(* Supprime les lettres qui n'ont pas la meme reference au mot predecesseur *)
let rec check_letters_same_hash (word : word) (lStore : letter list) : letter list =
  match lStore with
  | [] -> lStore
  | x::xs -> if ((Crypto.hash (Word.to_bigstring word)) = x.head) then x::(check_letters_same_hash word xs)
                                     else check_letters_same_hash word xs

let send_new_word st level =
  (* generate a word above the blockchain head, with the adequate letters *)
  (* then send it to the server *)
  Option.iter
    (fun (head:word) ->
      let store_letters = (List.of_seq (Hashtbl.to_seq_values st.letter_store.letters_table))  in
      Log.log_info "STORE CONTENT IN SEND NEW WORD : @.";
      print_letter_list store_letters ;
      Log.log_info "@.";

      (*Créer le mot *)
      let same_level_letters = deletes_letters_from_old_level store_letters level in
      Log.log_info "STORE CONTENT AFTER SAME LEVEL CLEANING : @.";
      print_letter_list same_level_letters ;
      Log.log_info "@.";

      let same_hash_letters = check_letters_same_hash head same_level_letters in
      Log.log_info "STORE CONTENT AFTER SAME HASH CLEANING : @.";
      print_letter_list same_hash_letters ;
      Log.log_info "@.";

      let final_letter_store = check_letters_one_per_author same_hash_letters in
      Log.log_info "STORE CONTENT AFTER ONE PER AUTHOR CLEANING : @.";
      print_letter_list final_letter_store ;
      Log.log_info "@.";

      match get_word_from_dict final_letter_store with
        | Some word_from_dico -> let word = make_word_on_blockletters level word_from_dico st.politician (Word.to_bigstring head) in
                    Log.log_info "SENDING WORD AT LEVEL %i@." level ;
                    Store.add_word st.word_store word ;
                    let message = Messages.Inject_word word in
                    Client_utils.send_some message
        | None -> ()
    )
    (Consensus.head ~level:(level - 1) st.word_store)
  
  

let run ?(max_iter = 0) () =
  (* Generate public/secret keys *)
  let (pk, sk) = Crypto.genkeys () in

  (* Get initial wordpool *)
  let getpool = Messages.Get_full_wordpool in
    Client_utils.send_some getpool ;
  let wordpool =
    match Client_utils.receive () with
    | Messages.Full_wordpool wordpool -> wordpool
    | _ -> assert false
  in

  (* Generate initial blocktree *)
  let wStore = Store.init_words () in
    Store.add_words wStore wordpool.words ;

  (* Get initial letterpool *)
  let getlpool = Messages.Get_full_letterpool in
    Client_utils.send_some getlpool ;
  let lpool =
    match Client_utils.receive () with
      | Messages.Full_letterpool letterpool -> letterpool
      | _ -> assert false
  in

  (* Generate initial letterpool *)
  let lStore = Store.init_letters () in
    Store.add_letters lStore lpool.letters ;

  (* Create and send first word *)
  let pol = {sk ; pk} in
  let state = {politician=pol ; word_store=wStore ; letter_store=lStore ; next_words=[]} in
  send_new_word state wordpool.current_period ;
  
  (* start listening to server messages *)
  Client_utils.send_some Messages.Listen ;
  (*  main loop *)
  let level = ref wordpool.current_period in
  let rec loop max_iter =
    if max_iter = 0 then ()
    else (
      ( match Client_utils.receive () with
      | Messages.Inject_word w ->
          Log.log_info "RECEIVED INJECT WORD@." ;
          Store.add_word wStore w ;
          (*Option.iter
            (fun head ->
              if head = w then (
                Log.log_info "Head updated to incoming word %a@." Word.pp w ;
                send_new_word state !level )
              else Log.log_info "incoming word %a not a new head@." Word.pp w)
            (Consensus.head ~level:(!level - 1) wStore) *)
      | Messages.Next_turn p -> 
          Log.log_info "RECEIVED NEXT TURN@." ;
          level := p; 
      | Messages.Inject_letter l ->
          Log.log_info "RECEIVED INJECT LETTER@." ;
          Store.add_letter lStore l ;
          Option.iter
            (fun head ->
              (* (to avoid unused variable head) *)
              (* Log.log_info "Current head  :  %a@." Word.pp head ; *)
              ignore head;
              send_new_word state !level)
            (Consensus.head ~level:(!level - 1) wStore)
      | _ -> () ) ; (* To avoid non exhaustive pattern matching*)
      loop (max_iter - 1) )
  in
  loop max_iter

let _ =
  let main =
    Random.self_init () ;
    let () = Client_utils.connect () in
    run ~max_iter:(-1) ()
  in
  main
