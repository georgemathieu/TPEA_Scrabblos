(* open Messages *)
open Word
open Crypto
open Letter

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
  {word ; Word.level ; head ; politician=politicien.pk ; signature}


let make_word_on_hash level letters politician head_hash : word =
  let head = head_hash in
  make ~word:letters ~level ~politicien:politician ~head

let make_word_on_blockletters level letters politician head : word =
  let head_hash = Crypto.hash head in
  make_word_on_hash level letters politician head_hash

let send_new_word st level =
  (* generate a word above the blockchain head, with the adequate letters *)
  (* then send it to the server *)
  Option.iter
    (fun (head:word) ->
      let lettersFromStore = Store.get_letters st.letter_store head.head in
      let word = make_word_on_blockletters level lettersFromStore st.politician (Word.to_bigstring head) in
      let message = Messages.Inject_word word in
      Client_utils.send_some message)
    (Consensus.head ~level:(level - 1) st.word_store)
  
  

let run ?(max_iter = 0) () =
  (* Generate public/secret keys *)
  let (pk, sk) = Crypto.genkeys () in

  Log.log_info "POLITICIEN TEST" ;

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
          Store.add_word wStore w ;
          Option.iter
            (fun head ->
              if head = w then (
                Log.log_info "Head updated to incoming word %a@." Word.pp w ;
                send_new_word state !level )
              else Log.log_info "incoming word %a not a new head@." Word.pp w)
            (Consensus.head ~level:(!level - 1) wStore)
      | Messages.Next_turn p -> level := p
      | Messages.Inject_letter _ | _ -> () ) ;
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
