val fitness : Store.word_store -> Word.t -> int

val head : ?level:int -> Store.word_store -> Word.t option

val lettre_score : Letter.t -> int

val word_score : Word.t -> int
