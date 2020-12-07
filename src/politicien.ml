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

let make ~(pk : Crypto.pk) ~(sk : Crypto.sk) ~(word : letter list) ~(head : hash) ~(level : int)  =
  (* Build politcian *)
  let politician = {sk ; pk} in
  (* Create message *)
  let msg = Word.pre_bigstring ~word ~level ~head ~pk in
  (* Build signature *)
  let signature = Crypto.sign ~sk ~msg in
  {word ; Word.level ; head ; politician=pk ; signature}


let make_word_on_hash level letters politician head_hash : word =
  let head = head_hash in
  make ~word:letters ~level ~pk:politician.pk ~sk:politician.sk ~head

let make_word_on_blockletters level letters politician head : word =
  let head_hash = Crypto.hash head in
  make_word_on_hash level letters politician head_hash

let send_new_word st level =
  (* generate a word above the blockchain head, with the adequate letters *)
  (* then send it to the server *)
  Option.iter
    (fun head ->
      let lettersFromStore = Store.get_letters st.letter_store head in
      let word = make_word_on_blockletters level lettersFromStore st.politician (Crypto.hash_to_bigstring head) in
      let message = Messages.Inject_word word in
      Client_utils.send_some message)
    (Consensus.head ~level:(level - 1) st.word_store) (* typing error ? *)
  
  

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
  let store = Store.init_words () in
    Store.add_words store wordpool.words ;

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
  let state = {pol ; store ; lStore ; next_words=[]} in
  
  (* start listening to server messages *)
  Log.log_warn "TODO" ;
  (*  main loop *)
  failwith ("à programmer" ^ __LOC__)

let _ =
  let main =
    Random.self_init () ;
    let () = Client_utils.connect () in
    run ~max_iter:(-1) ()
  in
  main
