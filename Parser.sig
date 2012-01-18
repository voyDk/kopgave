local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = (int*int)
type t__4__ = char*(int*int)
type t__5__ = (int*int)
type t__6__ = (int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = (int*int)
type t__10__ = (int*int)
type t__11__ = string*(int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = (int*int)
type t__18__ = (int*int)
type t__19__ = int*(int*int)
type t__20__ = (int*int)
type t__21__ = (int*int)
type t__22__ = (int*int)
type t__23__ = (int*int)
type t__24__ = (int*int)
type t__25__ = (int*int)
type t__26__ = string*(int*int)
type t__27__ = (int*int)
type t__28__ = (int*int)
type t__29__ = (int*int)
in
datatype token =
    ASSIGN of t__1__
  | CATCH of t__2__
  | CHAR of t__3__
  | CHARCONST of t__4__
  | COLON of t__5__
  | COMMA of t__6__
  | DEREF of t__7__
  | ELSE of t__8__
  | EOF of t__9__
  | EQUAL of t__10__
  | ID of t__11__
  | IF of t__12__
  | INT of t__13__
  | LBRACE of t__14__
  | LBRACK of t__15__
  | LESS of t__16__
  | LPAR of t__17__
  | MINUS of t__18__
  | NUM of t__19__
  | PLUS of t__20__
  | RBRACE of t__21__
  | RBRACK of t__22__
  | RETURN of t__23__
  | RPAR of t__24__
  | SEMICOLON of t__25__
  | STRINGCONST of t__26__
  | THROW of t__27__
  | TRY of t__28__
  | WHILE of t__29__
end;

val Prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> S100.Prog;
