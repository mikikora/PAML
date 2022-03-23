
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | WHERE
    | VALID
    | TRUE
    | STRTACTIC of (
# 21 "parser.mly"
       (string)
# 17 "parser.ml"
  )
    | SMPLTACTIC of (
# 20 "parser.mly"
       (string)
# 22 "parser.ml"
  )
    | SEMICOLON
    | RPAR
    | RBRACK
    | RBRACE
    | QED
    | PROOF
    | LPAR
    | LBRACK
    | LBRACE
    | INTRO
    | INT of (
# 23 "parser.mly"
       (int32)
# 37 "parser.ml"
  )
    | ID of (
# 12 "parser.mly"
       (string)
# 42 "parser.ml"
  )
    | FOCUS
    | DOT
    | DIA
    | BOX
    | ASSIGN
    | AS
    | ARROW
    | APPLY
    | AND
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState41
  | MenhirState32
  | MenhirState25
  | MenhirState23
  | MenhirState22
  | MenhirState14
  | MenhirState12
  | MenhirState11
  | MenhirState9
  | MenhirState8
  | MenhirState7

# 3 "parser.mly"
  
    open Ast

    let locate loc x = {loc = mkLocation loc; v=x}

# 84 "parser.ml"

[@@@ocaml.warning "-4-39"]

let rec _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_option_preceded_AS_ID__ : _menhir_env -> 'ttv_tail -> 'tv_option_preceded_AS_ID__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv209 * Lexing.position) * _menhir_state * 'tv_form) = Obj.magic _menhir_stack in
    let (_v : 'tv_option_preceded_AS_ID__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv207 * Lexing.position) * _menhir_state * 'tv_form) = Obj.magic _menhir_stack in
    let ((id : 'tv_option_preceded_AS_ID__) : 'tv_option_preceded_AS_ID__) = _v in
    let ((_menhir_stack, _startpos__1_), _, (f : 'tv_form)) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _v : 'tv_raw_statement = 
# 67 "parser.mly"
    ( Apply (f, id))
# 106 "parser.ml"
     in
    (_menhir_goto_raw_statement _menhir_env _menhir_stack _v _startpos : 'freshtv208)) : 'freshtv210)

and _menhir_goto_env_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_env_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv171 * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv169 * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env_list) = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (f : 'tv_form)), _, (tl : 'tv_env_list)) = _menhir_stack in
        let _v : 'tv_env_list = 
# 104 "parser.mly"
    ( f :: tl )
# 123 "parser.ml"
         in
        (_menhir_goto_env_list _menhir_env _menhir_stack _menhir_s _v : 'freshtv170)) : 'freshtv172)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * _menhir_state) * _menhir_state * 'tv_env_list) = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        ((match _tok with
        | RBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv201 * _menhir_state) * _menhir_state * 'tv_env_list) = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv199 * _menhir_state) * _menhir_state * 'tv_env_list) = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_env_list)) = _menhir_stack in
            let _v : 'tv_env = 
# 98 "parser.mly"
    ( _2 )
# 142 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv197) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_env) = _v in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (((match _menhir_s with
            | MenhirState22 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv187 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 155 "parser.ml"
                ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env) = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                ((match _tok with
                | TRUE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((('freshtv179 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 165 "parser.ml"
                    ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env) = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    ((match _tok with
                    | AND ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((((('freshtv173 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 175 "parser.ml"
                        ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env)) = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        ((match _tok with
                        | LBRACK ->
                            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv174)
                    | DOT ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((((('freshtv175 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 191 "parser.ml"
                        ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env)) = Obj.magic _menhir_stack in
                        let ((((_menhir_stack, _startpos__1_), (id : (
# 12 "parser.mly"
       (string)
# 196 "parser.ml"
                        ))), _, (f : 'tv_form)), _, (e : 'tv_env)) = _menhir_stack in
                        let _startpos = _startpos__1_ in
                        let _v : 'tv_raw_statement = 
# 51 "parser.mly"
    ( Proof (id, f, Some e, None))
# 202 "parser.ml"
                         in
                        (_menhir_goto_raw_statement _menhir_env _menhir_stack _v _startpos : 'freshtv176)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((((('freshtv177 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 212 "parser.ml"
                        ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env)) = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv178)) : 'freshtv180)
                | VALID ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((('freshtv183 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 221 "parser.ml"
                    ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env) = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((('freshtv181 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 228 "parser.ml"
                    ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env) = Obj.magic _menhir_stack in
                    let ((((_menhir_stack, _startpos__1_), (id : (
# 12 "parser.mly"
       (string)
# 233 "parser.ml"
                    ))), _, (f : 'tv_form)), _, (e : 'tv_env)) = _menhir_stack in
                    let _startpos = _startpos__1_ in
                    let _v : 'tv_raw_statement = 
# 53 "parser.mly"
    ( Proof (id, f, None, Some e))
# 239 "parser.ml"
                     in
                    (_menhir_goto_raw_statement _menhir_env _menhir_stack _v _startpos : 'freshtv182)) : 'freshtv184)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((('freshtv185 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 249 "parser.ml"
                    ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env) = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv186)) : 'freshtv188)
            | MenhirState32 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((((('freshtv195 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 258 "parser.ml"
                ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env))) * _menhir_state * 'tv_env) = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                ((match _tok with
                | VALID ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((((((('freshtv191 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 268 "parser.ml"
                    ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env))) * _menhir_state * 'tv_env) = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((((((('freshtv189 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 275 "parser.ml"
                    ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env))) * _menhir_state * 'tv_env) = Obj.magic _menhir_stack in
                    let (((((_menhir_stack, _startpos__1_), (id : (
# 12 "parser.mly"
       (string)
# 280 "parser.ml"
                    ))), _, (f : 'tv_form)), _, (e1 : 'tv_env)), _, (e2 : 'tv_env)) = _menhir_stack in
                    let _startpos = _startpos__1_ in
                    let _v : 'tv_raw_statement = 
# 55 "parser.mly"
    ( Proof (id, f, Some e1, Some e2))
# 286 "parser.ml"
                     in
                    (_menhir_goto_raw_statement _menhir_env _menhir_stack _v _startpos : 'freshtv190)) : 'freshtv192)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((((((('freshtv193 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 296 "parser.ml"
                    ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env))) * _menhir_state * 'tv_env) = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv194)) : 'freshtv196)
            | _ ->
                _menhir_fail ()) : 'freshtv198) : 'freshtv200)) : 'freshtv202)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv203 * _menhir_state) * _menhir_state * 'tv_env_list) = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv204)) : 'freshtv206)
    | _ ->
        _menhir_fail ()

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_form_raw -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOX ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | DIA ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | LPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_goto_form_raw : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_form_raw -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        ((match _tok with
        | ARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv103 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (f : 'tv_form_raw)) = _menhir_stack in
            let _v : 'tv_form_raw = 
# 90 "parser.mly"
    ( Box f)
# 362 "parser.ml"
             in
            (_menhir_goto_form_raw _menhir_env _menhir_stack _menhir_s _v : 'freshtv104)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv105 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv106)) : 'freshtv108)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state * 'tv_form_raw)) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        ((match _tok with
        | ARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv109 * _menhir_state * 'tv_form_raw)) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (f1 : 'tv_form_raw)), _, (f2 : 'tv_form_raw)) = _menhir_stack in
            let _v : 'tv_form_raw = 
# 88 "parser.mly"
    ( Imp (f1, f2))
# 387 "parser.ml"
             in
            (_menhir_goto_form_raw _menhir_env _menhir_stack _menhir_s _v : 'freshtv110)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv111 * _menhir_state * 'tv_form_raw)) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv112)) : 'freshtv114)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        ((match _tok with
        | ARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv115 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (f : 'tv_form_raw)) = _menhir_stack in
            let _v : 'tv_form_raw = 
# 92 "parser.mly"
    ( Dia f)
# 412 "parser.ml"
             in
            (_menhir_goto_form_raw _menhir_env _menhir_stack _menhir_s _v : 'freshtv116)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv117 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv118)) : 'freshtv120)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        ((match _tok with
        | ARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv123 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv121 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (f : 'tv_form_raw)) = _menhir_stack in
            let _v : 'tv_form_raw = 
# 94 "parser.mly"
    ( f )
# 440 "parser.ml"
             in
            (_menhir_goto_form_raw _menhir_env _menhir_stack _menhir_s _v : 'freshtv122)) : 'freshtv124)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv125 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv126)) : 'freshtv128)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        ((match _tok with
        | ARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv163 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv161 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_form_raw)) = _menhir_stack in
            let _v : 'tv_form = 
# 82 "parser.mly"
    ( _2 )
# 468 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv159) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_form) = _v in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (((match _menhir_s with
            | MenhirState7 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv135 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 481 "parser.ml"
                ))) * _menhir_state * 'tv_form) = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                ((match _tok with
                | WHERE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv129 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 491 "parser.ml"
                    ))) * _menhir_state * 'tv_form) = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    ((match _tok with
                    | LBRACK ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv130)
                | DOT ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv131 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 507 "parser.ml"
                    ))) * _menhir_state * 'tv_form) = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _startpos__1_), (id : (
# 12 "parser.mly"
       (string)
# 512 "parser.ml"
                    ))), _, (f : 'tv_form)) = _menhir_stack in
                    let _startpos = _startpos__1_ in
                    let _v : 'tv_raw_statement = 
# 49 "parser.mly"
    ( Proof (id, f, None, None))
# 518 "parser.ml"
                     in
                    (_menhir_goto_raw_statement _menhir_env _menhir_stack _v _startpos : 'freshtv132)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv133 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 528 "parser.ml"
                    ))) * _menhir_state * 'tv_form) = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv134)) : 'freshtv136)
            | MenhirState25 | MenhirState23 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv143 * _menhir_state * 'tv_form) = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                ((match _tok with
                | SEMICOLON ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv137 * _menhir_state * 'tv_form) = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    ((match _tok with
                    | LBRACE ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv138)
                | RBRACK ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv139 * _menhir_state * 'tv_form) = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (f : 'tv_form)) = _menhir_stack in
                    let _v : 'tv_env_list = 
# 102 "parser.mly"
    ( [f] )
# 557 "parser.ml"
                     in
                    (_menhir_goto_env_list _menhir_env _menhir_stack _menhir_s _v : 'freshtv140)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv141 * _menhir_state * 'tv_form) = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv142)) : 'freshtv144)
            | MenhirState41 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv157 * Lexing.position) * _menhir_state * 'tv_form) = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                ((match _tok with
                | AS ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv151) = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    ((match _tok with
                    | ID _v ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv147) = Obj.magic _menhir_stack in
                        let (_v : (
# 12 "parser.mly"
       (string)
# 585 "parser.ml"
                        )) = _v in
                        let _menhir_env = _menhir_discard _menhir_env in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv145) = Obj.magic _menhir_stack in
                        let ((x : (
# 12 "parser.mly"
       (string)
# 593 "parser.ml"
                        )) : (
# 12 "parser.mly"
       (string)
# 597 "parser.ml"
                        )) = _v in
                        let _v =
                          let x = 
# 180 "<standard.mly>"
    ( x )
# 603 "parser.ml"
                           in
                          (
# 113 "<standard.mly>"
    ( Some x )
# 608 "parser.ml"
                           : 'tv_option_preceded_AS_ID__)
                        in
                        (_menhir_goto_option_preceded_AS_ID__ _menhir_env _menhir_stack _v : 'freshtv146)) : 'freshtv148)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv149) = Obj.magic _menhir_stack in
                        (raise _eRR : 'freshtv150)) : 'freshtv152)
                | DOT ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv153) = Obj.magic _menhir_stack in
                    let _v : 'tv_option_preceded_AS_ID__ = 
# 111 "<standard.mly>"
    ( None )
# 624 "parser.ml"
                     in
                    (_menhir_goto_option_preceded_AS_ID__ _menhir_env _menhir_stack _v : 'freshtv154)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv155 * Lexing.position) * _menhir_state * 'tv_form) = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv156)) : 'freshtv158)
            | _ ->
                _menhir_fail ()) : 'freshtv160) : 'freshtv162)) : 'freshtv164)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv165 * _menhir_state) * _menhir_state * 'tv_form_raw) = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv166)) : 'freshtv168)
    | _ ->
        _menhir_fail ()

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOX ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | DIA ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | LPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 668 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv101) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 12 "parser.mly"
       (string)
# 678 "parser.ml"
    )) : (
# 12 "parser.mly"
       (string)
# 682 "parser.ml"
    )) = _v in
    let _v : 'tv_form_raw = 
# 86 "parser.mly"
    ( Var _1 )
# 687 "parser.ml"
     in
    (_menhir_goto_form_raw _menhir_env _menhir_stack _menhir_s _v : 'freshtv102)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOX ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | DIA ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOX ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | DIA ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | LPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_goto_option_ID_ : _menhir_env -> 'ttv_tail -> 'tv_option_ID_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv99 * Lexing.position) = Obj.magic _menhir_stack in
    let (_v : 'tv_option_ID_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv97 * Lexing.position) = Obj.magic _menhir_stack in
    let ((id : 'tv_option_ID_) : 'tv_option_ID_) = _v in
    let (_menhir_stack, _startpos__1_) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _v : 'tv_raw_statement = 
# 59 "parser.mly"
    ( Intro id )
# 742 "parser.ml"
     in
    (_menhir_goto_raw_statement _menhir_env _menhir_stack _v _startpos : 'freshtv98)) : 'freshtv100)

and _menhir_goto_option_INT_ : _menhir_env -> 'ttv_tail -> 'tv_option_INT_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv95 * Lexing.position) = Obj.magic _menhir_stack in
    let (_v : 'tv_option_INT_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv93 * Lexing.position) = Obj.magic _menhir_stack in
    let ((num : 'tv_option_INT_) : 'tv_option_INT_) = _v in
    let (_menhir_stack, _startpos__1_) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _v : 'tv_raw_statement = 
# 57 "parser.mly"
    ( Focus num)
# 759 "parser.ml"
     in
    (_menhir_goto_raw_statement _menhir_env _menhir_stack _v _startpos : 'freshtv94)) : 'freshtv96)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv72)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv73 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 775 "parser.ml"
        ))) * _menhir_state * 'tv_form)) * _menhir_state * 'tv_env))) = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv74)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * 'tv_form)) = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv76)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv78)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv79 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 794 "parser.ml"
        ))) * _menhir_state * 'tv_form)) = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv80)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_form_raw)) = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv82)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv84)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv86)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv88)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        (_menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv90)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv91 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 828 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv92)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOX ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | DIA ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | LPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_goto_raw_statement : _menhir_env -> 'ttv_tail -> 'tv_raw_statement -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _v _startpos ->
    let _menhir_stack = (_menhir_stack, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv69 * 'tv_raw_statement * Lexing.position) = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    ((match _tok with
    | DOT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * 'tv_raw_statement * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * 'tv_raw_statement * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_menhir_stack, (_1 : 'tv_raw_statement), _startpos__1_) = _menhir_stack in
        let _v =
          let _endpos = _endpos__2_ in
          let _startpos = _startpos__1_ in
          let _loc = (_startpos, _endpos) in
          (
# 45 "parser.mly"
                      (locate _loc _1)
# 874 "parser.ml"
           : (
# 10 "parser.mly"
      (Ast.statement)
# 878 "parser.ml"
          ))
        in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61) = _menhir_stack in
        let (_v : (
# 10 "parser.mly"
      (Ast.statement)
# 886 "parser.ml"
        )) = _v in
        (((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59) = Obj.magic _menhir_stack in
        let (_v : (
# 10 "parser.mly"
      (Ast.statement)
# 893 "parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57) = Obj.magic _menhir_stack in
        let ((_1 : (
# 10 "parser.mly"
      (Ast.statement)
# 900 "parser.ml"
        )) : (
# 10 "parser.mly"
      (Ast.statement)
# 904 "parser.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv58)) : 'freshtv60)) : 'freshtv62) : 'freshtv64)) : 'freshtv66)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * 'tv_raw_statement * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv68)) : 'freshtv70)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and statement : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 10 "parser.mly"
      (Ast.statement)
# 929 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv55) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    ((match _tok with
    | APPLY ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        ((match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv3 * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState41 in
            let (_v : (
# 12 "parser.mly"
       (string)
# 958 "parser.ml"
            )) = _v in
            let _menhir_env = _menhir_discard _menhir_env in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1 * Lexing.position) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((id : (
# 12 "parser.mly"
       (string)
# 967 "parser.ml"
            )) : (
# 12 "parser.mly"
       (string)
# 971 "parser.ml"
            )) = _v in
            let (_menhir_stack, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : 'tv_raw_statement = 
# 69 "parser.mly"
    ( ApplyThm id )
# 978 "parser.ml"
             in
            (_menhir_goto_raw_statement _menhir_env _menhir_stack _v _startpos : 'freshtv2)) : 'freshtv4)
        | LBRACE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv6)
    | FOCUS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        ((match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
            let (_v : (
# 23 "parser.mly"
       (int32)
# 1001 "parser.ml"
            )) = _v in
            let _menhir_env = _menhir_discard _menhir_env in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
            let ((x : (
# 23 "parser.mly"
       (int32)
# 1009 "parser.ml"
            )) : (
# 23 "parser.mly"
       (int32)
# 1013 "parser.ml"
            )) = _v in
            let _v : 'tv_option_INT_ = 
# 113 "<standard.mly>"
    ( Some x )
# 1018 "parser.ml"
             in
            (_menhir_goto_option_INT_ _menhir_env _menhir_stack _v : 'freshtv8)) : 'freshtv10)
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
            let _v : 'tv_option_INT_ = 
# 111 "<standard.mly>"
    ( None )
# 1027 "parser.ml"
             in
            (_menhir_goto_option_INT_ _menhir_env _menhir_stack _v : 'freshtv12)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv13 * Lexing.position) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv14)) : 'freshtv16)
    | INTRO ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        ((match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "parser.mly"
       (string)
# 1050 "parser.ml"
            )) = _v in
            let _menhir_env = _menhir_discard _menhir_env in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
            let ((x : (
# 12 "parser.mly"
       (string)
# 1058 "parser.ml"
            )) : (
# 12 "parser.mly"
       (string)
# 1062 "parser.ml"
            )) = _v in
            let _v : 'tv_option_ID_ = 
# 113 "<standard.mly>"
    ( Some x )
# 1067 "parser.ml"
             in
            (_menhir_goto_option_ID_ _menhir_env _menhir_stack _v : 'freshtv18)) : 'freshtv20)
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv21) = Obj.magic _menhir_stack in
            let _v : 'tv_option_ID_ = 
# 111 "<standard.mly>"
    ( None )
# 1076 "parser.ml"
             in
            (_menhir_goto_option_ID_ _menhir_env _menhir_stack _v : 'freshtv22)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv23 * Lexing.position) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv24)) : 'freshtv26)
    | PROOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        ((match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv31 * Lexing.position) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "parser.mly"
       (string)
# 1099 "parser.ml"
            )) = _v in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            ((match _tok with
            | ASSIGN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv27 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 1110 "parser.ml"
                )) = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                ((match _tok with
                | LBRACE ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv28)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv29 * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 1128 "parser.ml"
                )) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv30)) : 'freshtv32)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv33 * Lexing.position) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv34)) : 'freshtv36)
    | QED ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_env = _menhir_discard _menhir_env in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37) = Obj.magic _menhir_stack in
        let (_startpos__1_ : Lexing.position) = _startpos in
        let _startpos = _startpos__1_ in
        let _v : 'tv_raw_statement = 
# 70 "parser.mly"
          ( Qed )
# 1149 "parser.ml"
         in
        (_menhir_goto_raw_statement _menhir_env _menhir_stack _v _startpos : 'freshtv38)) : 'freshtv40)
    | SMPLTACTIC _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43) = Obj.magic _menhir_stack in
        let (_v : (
# 20 "parser.mly"
       (string)
# 1158 "parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_env = _menhir_discard _menhir_env in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41) = Obj.magic _menhir_stack in
        let ((t : (
# 20 "parser.mly"
       (string)
# 1167 "parser.ml"
        )) : (
# 20 "parser.mly"
       (string)
# 1171 "parser.ml"
        )) = _v in
        let (_startpos_t_ : Lexing.position) = _startpos in
        let _startpos = _startpos_t_ in
        let _v : 'tv_raw_statement = 
# 72 "parser.mly"
    ( match t with
        | "undia" -> Poss
        | "unbox" -> Valid
        | "fromtrue" -> FromTrue
        | "unfocus" -> Unfocus
        | _ -> failwith "absurd"
    )
# 1184 "parser.ml"
         in
        (_menhir_goto_raw_statement _menhir_env _menhir_stack _v _startpos : 'freshtv42)) : 'freshtv44)
    | STRTACTIC _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51) = Obj.magic _menhir_stack in
        let (_v : (
# 21 "parser.mly"
       (string)
# 1193 "parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        ((match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv47 * (
# 21 "parser.mly"
       (string)
# 1205 "parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "parser.mly"
       (string)
# 1210 "parser.ml"
            )) = _v in
            let _menhir_env = _menhir_discard _menhir_env in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv45 * (
# 21 "parser.mly"
       (string)
# 1217 "parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let ((id : (
# 12 "parser.mly"
       (string)
# 1222 "parser.ml"
            )) : (
# 12 "parser.mly"
       (string)
# 1226 "parser.ml"
            )) = _v in
            let (_menhir_stack, (ap : (
# 21 "parser.mly"
       (string)
# 1231 "parser.ml"
            )), _startpos_ap_) = _menhir_stack in
            let _startpos = _startpos_ap_ in
            let _v : 'tv_raw_statement = 
# 61 "parser.mly"
    ( match ap with
        | "applyt" -> Applyt id
        | "applyv" -> Applyv id 
        | _ -> failwith "absurd"
    )
# 1241 "parser.ml"
             in
            (_menhir_goto_raw_statement _menhir_env _menhir_stack _v _startpos : 'freshtv46)) : 'freshtv48)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv49 * (
# 21 "parser.mly"
       (string)
# 1251 "parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv50)) : 'freshtv52)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv54)) : 'freshtv56))
