# 1 "src/lexer.mll"
  
    open Parser;;        (* The type token is defined in parser.mli *)
    open Utils ;;
    let keyword_table = Hashtbl.create 100
    let _ =
        List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
        ["AND", AND;
        "And", AND;
        "and", AND;
        "OR", OR;
        "Or", OR;
        "or", OR;
        "NOT", NOT;
        "Not", NOT;
        "not", NOT;
        "FALSE", BOT;
        "False", BOT;
        "false", BOT;
        "TRUE", TOP;
        "True", TOP;
        "true", TOP;
        "null", NULL;
        "Null", NULL;
        "NULL", NULL;
        ]
(*		exception Eof
*)
 
# 31 "src/lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\224\255\225\255\226\255\227\255\003\000\030\000\032\000\
    \235\255\236\255\237\255\238\255\002\000\014\000\075\000\123\000\
    \086\000\003\000\198\000\212\000\018\000\254\255\255\255\038\000\
    \076\000\001\000\253\255\251\255\252\255\241\255\014\000\249\255\
    \250\255\240\255\234\000\247\000\010\001\032\001\042\001\127\000\
    \096\000\031\001\243\255\242\255\231\255\234\255\233\255\230\255\
    ";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\027\000\031\000\026\000\
    \255\255\255\255\255\255\255\255\031\000\031\000\011\000\010\000\
    \023\000\031\000\016\000\007\000\031\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\008\000\008\000\255\255\255\255\
    \009\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\039\000\255\255\255\255\025\000\000\000\000\000\030\000\
    \030\000\025\000\000\000\000\000\000\000\000\000\030\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\039\000\
    \255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\022\000\021\000\028\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \032\000\000\000\000\000\000\000\026\000\000\000\000\000\000\000\
    \022\000\000\000\000\000\000\000\000\000\020\000\000\000\017\000\
    \010\000\009\000\040\000\004\000\011\000\003\000\018\000\043\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\013\000\042\000\007\000\008\000\005\000\012\000\
    \047\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\046\000\006\000\044\000\045\000\016\000\
    \033\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\023\000\029\000\039\000\
    \024\000\255\255\000\000\000\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\040\000\000\000\
    \000\000\000\000\014\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\041\000\000\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\015\000\000\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \002\000\027\000\035\000\255\255\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\031\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\038\000\000\000\038\000\
    \000\000\034\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\000\000\000\000\255\255\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\034\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\034\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\041\000\255\255\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\025\000\255\255\017\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \030\000\255\255\255\255\255\255\020\000\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\000\000\255\255\000\000\
    \000\000\000\000\017\000\000\000\000\000\000\000\000\000\012\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
    \005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\000\000\007\000\007\000\000\000\
    \023\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\020\000\024\000\040\000\
    \020\000\039\000\255\255\255\255\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\039\000\255\255\
    \255\255\255\255\014\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\016\000\255\255\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\015\000\255\255\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \000\000\025\000\019\000\017\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\030\000\255\255\
    \255\255\255\255\020\000\255\255\255\255\034\000\255\255\034\000\
    \255\255\019\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\255\255\255\255\023\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\019\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\024\000\255\255\255\255\036\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\036\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\041\000\039\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 31 "src/lexer.mll"
                         ( token lexbuf )
# 220 "src/lexer.ml"

  | 1 ->
# 32 "src/lexer.mll"
                         ( Lexing.new_line lexbuf; token lexbuf )
# 225 "src/lexer.ml"

  | 2 ->
# 33 "src/lexer.mll"
                        ( Lexing.new_line lexbuf; token lexbuf )
# 230 "src/lexer.ml"

  | 3 ->
# 34 "src/lexer.mll"
                                             ( Lexing.new_line lexbuf; token lexbuf )
# 235 "src/lexer.ml"

  | 4 ->
# 35 "src/lexer.mll"
                                            ( Lexing.new_line lexbuf; token lexbuf )
# 240 "src/lexer.ml"

  | 5 ->
# 36 "src/lexer.mll"
                                              ( Lexing.new_line lexbuf; token lexbuf )
# 245 "src/lexer.ml"

  | 6 ->
# 37 "src/lexer.mll"
                                             ( Lexing.new_line lexbuf; token lexbuf )
# 250 "src/lexer.ml"

  | 7 ->
let
# 38 "src/lexer.mll"
                    lxm
# 256 "src/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 38 "src/lexer.mll"
                           ( INT (int_of_string lxm) )
# 260 "src/lexer.ml"

  | 8 ->
let
# 39 "src/lexer.mll"
                                                                 lxm
# 266 "src/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 39 "src/lexer.mll"
                                                                      ( FLOAT (float_of_string (lxm)) )
# 270 "src/lexer.ml"

  | 9 ->
let
# 40 "src/lexer.mll"
                                           lxm
# 276 "src/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 40 "src/lexer.mll"
                                                ( STRING(lxm) )
# 280 "src/lexer.ml"

  | 10 ->
let
# 41 "src/lexer.mll"
                                           lxm
# 286 "src/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 41 "src/lexer.mll"
                                                ( 
        try
            Hashtbl.find keyword_table lxm
        with Not_found -> RELNAME(lxm) )
# 293 "src/lexer.ml"

  | 11 ->
let
# 45 "src/lexer.mll"
                                       lxm
# 299 "src/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 45 "src/lexer.mll"
                                            (
        try
            Hashtbl.find keyword_table lxm
        with Not_found -> VARNAME(lxm)
						)
# 307 "src/lexer.ml"

  | 12 ->
# 50 "src/lexer.mll"
                        ( IMPLIEDBY )
# 312 "src/lexer.ml"

  | 13 ->
# 51 "src/lexer.mll"
                          ( QMARK )
# 317 "src/lexer.ml"

  | 14 ->
# 52 "src/lexer.mll"
                           ( QMARK )
# 322 "src/lexer.ml"

  | 15 ->
# 53 "src/lexer.mll"
                                    (UMARK)
# 327 "src/lexer.ml"

  | 16 ->
# 54 "src/lexer.mll"
                         ( DOT )
# 332 "src/lexer.ml"

  | 17 ->
# 55 "src/lexer.mll"
                         ( SEP )
# 337 "src/lexer.ml"

  | 18 ->
# 56 "src/lexer.mll"
                         ( LPAREN )
# 342 "src/lexer.ml"

  | 19 ->
# 57 "src/lexer.mll"
                         ( RPAREN )
# 347 "src/lexer.ml"

  | 20 ->
# 58 "src/lexer.mll"
                         ( EQ )
# 352 "src/lexer.ml"

  | 21 ->
# 59 "src/lexer.mll"
                          ( NE )
# 357 "src/lexer.ml"

  | 22 ->
# 60 "src/lexer.mll"
                          ( NE )
# 362 "src/lexer.ml"

  | 23 ->
# 61 "src/lexer.mll"
                                                ( ANONVAR )
# 367 "src/lexer.ml"

  | 24 ->
# 62 "src/lexer.mll"
                                                ( LE )
# 372 "src/lexer.ml"

  | 25 ->
# 63 "src/lexer.mll"
                                                ( GE )
# 377 "src/lexer.ml"

  | 26 ->
# 64 "src/lexer.mll"
                                                ( LT )
# 382 "src/lexer.ml"

  | 27 ->
# 65 "src/lexer.mll"
                                                ( GT )
# 387 "src/lexer.ml"

  | 28 ->
# 66 "src/lexer.mll"
                                                ( PLUS )
# 392 "src/lexer.ml"

  | 29 ->
# 67 "src/lexer.mll"
                                                ( MINUS )
# 397 "src/lexer.ml"

  | 30 ->
# 68 "src/lexer.mll"
                  ( EOF )
# 402 "src/lexer.ml"

  | 31 ->
# 69 "src/lexer.mll"
                          ( spec_lex_error lexbuf )
# 407 "src/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

