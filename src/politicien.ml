(* open Messages *)
open Word
open Crypto
open Letter

let rec print_letter_list (l : letter list) =
  match l with
  | [] -> print_string " "
  | x::xs -> print_char x.letter ; print_letter_list xs

let dictionnaryList = [Client_utils.list_of_dict "dict/dict_100000_1_10.txt" ; Client_utils.list_of_dict "dict/dict_100000_5_15.txt";
                       Client_utils.list_of_dict "dict/dict_100000_25_75.txt" ; Client_utils.list_of_dict "dict/dict_100000_50_200.txt"]

let get_dictionnary (i : int) = List.nth dictionnaryList i

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
  Log.log_info "IN MAKE WORD HASH" ;
  make ~word:letters ~level ~politicien:politician ~head

let make_word_on_blockletters level letters politician head : word =
  let head_hash = Crypto.hash head in
  Log.log_info "IN MAKE WORD BLOCK" ;
  make_word_on_hash level letters politician head_hash

let send_new_word st level =
  (* generate a word above the blockchain head, with the adequate letters *)
  (* then send it to the server *)
  Log.log_info "HERE SEND LEVEL %i" level ;
  Option.iter
    (fun (head:word) ->
      Log.log_info "HERE SEND 1" ;
      let lettersFromStore = (List.of_seq (Hashtbl.to_seq_values st.letter_store.letters_table))  in
      Log.log_info "HERE FROM STORE : ";
      print_letter_list lettersFromStore ;
      let word = make_word_on_blockletters level lettersFromStore st.politician (Word.to_bigstring head) in
      Store.add_word st.word_store word ;
      let message = Messages.Inject_word word in
      Client_utils.send_some message)
    (Consensus.head ~level:(level - 1) st.word_store)
  
  

let run ?(max_iter = 0) () =
  (* Generate public/secret keys *)
  let (pk, sk) = Crypto.genkeys () in

  Log.log_info "POLITICIEN TEST" ;

  (* Get initial wordpool *)
  let getpool = Messages.Get_full_wordpool in
    Log.log_info "BEFORE woordpool" ;
    Client_utils.send_some getpool ;
    Log.log_info "AFTER WORDPOOL woordpool" ;
  let wordpool =
    match Client_utils.receive () with
    | Messages.Full_wordpool wordpool -> wordpool
    | _ -> assert false
  in

  (* Generate initial blocktree *)
  let wStore = Store.init_words () in
    Store.add_words wStore wordpool.words ;
  
  Log.log_info "reached woordpool" ;

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

  Log.log_info "reached letterpool" ;

  (* Create and send first word *)
  let pol = {sk ; pk} in
  let state = {politician=pol ; word_store=wStore ; letter_store=lStore ; next_words=[]} in
  send_new_word state wordpool.current_period ;
  
  (* start listening to server messages *)
  Log.log_info "reached before listen to server" ;
  Client_utils.send_some Messages.Listen ;
  (*  main loop *)
  let level = ref wordpool.current_period in
  let rec loop max_iter =
    if max_iter = 0 then ()
    else (
      ( match Client_utils.receive () with
      | Messages.Inject_word w ->
          Log.log_info "RECEIVED INJECT WORD" ;
          Store.add_word wStore w ;
          Option.iter
            (fun head ->
              if head = w then (
                Log.log_info "Head updated to incoming word %a@." Word.pp w ;
                send_new_word state !level )
              else Log.log_info "incoming word %a not a new head@." Word.pp w)
            (Consensus.head ~level:(!level - 1) wStore)
      | Messages.Next_turn p -> 
          Log.log_info "RECEIVED NEXT TURN" ;
          level := p; send_new_word state !level ; 
      | Messages.Inject_letter l ->
          Log.log_info "RECEIVED INJECT LETTER" ;
          Store.add_letter lStore l ;
          Option.iter
            ( fun head ->
              (* (to avoid unused variable head) *)
              Log.log_info "Current head  :  %a@." Word.pp head ;
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
    run ~max_iter:(20) ()
  in
  main
