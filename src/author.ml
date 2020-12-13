open Messages
open Letter
open Crypto

(* Sructure de l'auteur avec son score et le letter bag
Le score augmente quand une de ses lettres est utilise dans un mot qui est integre a la chaine
Le letter bag n'est pas utilise pour le moment *)
type state = {
  score : int ref ;
  letter_bag : char list ;
}

let make_letter_on_hash sk pk level head_hash letter : letter =
  Letter.make ~letter ~head:head_hash ~level ~pk ~sk

let make_letter_on_block sk pk level block letter : letter =
  let head_hash = hash block in
  make_letter_on_hash sk pk level head_hash letter

(* we remove non-alphabetical caracters and uppercase caracters *)
let random_char () = Random.int (122 - 97) + 97 |> Char.chr

let send_new_letter sk pk level store =
  (* Get blockchain head *)
  Option.iter
    (fun head ->
      (* Create new random letter *)
      Log.log_info "CURRENT HEAD : %a@." Word.pp head;
      let letter =
        make_letter_on_block
          sk
          pk
          level
          (Word.to_bigstring head)
          (random_char ())
      in
      (* Send letter *)
      let message = Messages.Inject_letter letter in
      Client_utils.send_some message)
    (Consensus.head ~level:(level - 1) store)

let rec add_score (pk : Id.author_id) (word : letter list) : int = 
  match word with
  | [] -> 0
  | x::xs -> if pk = x.author then Consensus.lettre_score x else add_score pk xs

let run ?(max_iter = 0) () =
  (* Generate public/secret keys *)
  let (pk, sk) = Crypto.genkeys () in

  (* Register to the server *)
  let reg_msg = Messages.Register pk in
  Client_utils.send_some reg_msg ;

  (* drop provided letter_bag *)
  let rec wait_lbag () =
  match Client_utils.receive () with
  | Messages.Letters_bag letter_bag -> letter_bag
  | _ -> wait_lbag ()
  in
  
  (* we use wait functions in case we don't receive the correct message directly *)
  let lbag = wait_lbag () in

  (* Get initial wordpool *)
  let getpool = Messages.Get_full_wordpool in
  Client_utils.send_some getpool ;
  let rec wait_wordpool () : Messages.wordpool =
    match Client_utils.receive () with
    | Messages.Full_wordpool wordpool -> wordpool
    | Messages.Diff_wordpool diff_wp -> diff_wp.wordpool
    | _ -> wait_wordpool ()
  in

  let wordpool = wait_wordpool () in

  (* Generate initial blocktree *)
  let store = Store.init_words () in
  Store.add_words store wordpool.words ;

  (* Create and send first letter *)
  let state = {score=ref 0 ; letter_bag=lbag} in
  send_new_letter sk pk wordpool.current_period store ;

  (* start listening to server messages *)
  Client_utils.send_some Messages.Listen ;

  let nombre_tours = 50 in

  (* start main loop *)
  let level = ref wordpool.current_period in
  Log.log_info "AUTHOR LEVEL %i" !level;
  let rec loop max_iter =
    if max_iter = 0 then ()
    else (
      ( match Client_utils.receive () with
      | Messages.Inject_word w ->
          Log.log_info "RECEIVED INJECT WORD@." ;
          Store.add_word store w ;
          Option.iter
            (fun head ->
              (* Log.log_info "current head : %a@."  Word.pp head; *)
              Log.log_info "current LEVEL : %i@."  !level;
              if head = w then (
                Log.log_info "Head updated to incoming word %a@." Word.pp w ;
                state.score := !(state.score) + (add_score pk w.word);
                (* Log.log_info "Current author score %i@." !(state.score) ; *) )
              else Log.log_info "incoming word %a not a new head@." Word.pp w )
            (Consensus.head ~level:(!level - 1) store)
      | Messages.Next_turn p -> 
          Log.log_info "RECEIVED NEXT TURN@." ;
          level := p ; 
          if (!level = nombre_tours) then Client_utils.send_some Messages.Fin_de_Partie else send_new_letter sk pk !level store
      | Messages.Fin_de_Partie -> Log.log_info "Final author score : %i@." !(state.score) ; ()
      | Messages.Inject_letter _ | _ -> () ) ;
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
