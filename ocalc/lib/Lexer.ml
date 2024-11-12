class lexer (source : string) =
  object (self)
    val mutable start : int = 0
    val mutable current : int = 0
    val mutable tokens : Token.t list = []
    method is_at_end ?(n : int = 0) () : bool = current + n >= String.length source
    method synchronize () = start <- current

    method peek ?(n : int = 0) () : char =
      match self#is_at_end ~n () with
      | true -> '\x00'
      | false -> String.unsafe_get source (current + n)

    method advance ?(steps : int = 1) () =
      match steps > 0 with
      | false -> failwith "advance: steps must be strictly positive"
      | true -> current <- current + steps

    method consume () : char =
      let char = self#peek () in
      self#advance ();
      char

    method get_lexeme () : string =
      if self#is_at_end ~n:(-1) ()
      then "\x00"
      else String.sub source start (current - start)

    method is_digit (char : char) : bool = '0' <= char && char <= '9'

    method scan_integer () : Token_kind.t =
      match self#is_digit (self#peek ()) with
      | false -> Token_kind.INTEGER
      | true ->
        self#advance ();
        self#scan_integer ()

    method scan_token () : Token_kind.t =
      match self#consume () with
      | ' ' | '\r' | '\t' | '\n' ->
        self#synchronize ();
        self#scan_token ()
      | '1' .. '9' -> self#scan_integer ()
      | '0' when not (self#is_digit (self#peek ())) -> Token_kind.INTEGER
      | '+' -> Token_kind.PLUS
      | '-' -> Token_kind.MINUS
      | '*' -> Token_kind.ASTERISK
      | '/' -> Token_kind.SLASH
      | _ -> failwith "bad token"

    method build_token (kind : Token_kind.t) : Token.t = kind, self#get_lexeme (), start
    method add_token (token : Token.t) = tokens <- token :: tokens

    method add_eof () =
      self#synchronize ();
      self#advance ();
      Token_kind.EOF |> self#build_token |> self#add_token

    method tokenize () : Token.t list =
      while not (self#is_at_end ()) do
        self#synchronize ();
        () |> self#scan_token |> self#build_token |> self#add_token
      done;
      self#add_eof ();
      List.rev tokens
  end
