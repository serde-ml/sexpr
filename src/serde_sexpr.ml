open Serde

let ( let* ) = Result.bind
let ( let@ ) = Option.bind

(*
  1. Serailize strings without quotes - Done
  2. Serialize none as a unit - Done
  3. Fix parser for strings and options - Maybe done?
  4. Fix all th borked deserializers
 *)

module Sexpr = struct
  module Parser = struct
    module Sedlex = struct
      include Sedlexing.Utf8

      let next = Sedlexing.next
      let rollback = Sedlexing.rollback
      let lexeme_length = Sedlexing.lexeme_length
    end

    let ( let* ) = Result.bind

    type t = { mutable lexbuf : Sedlexing.lexbuf }

    let unexpected_lex_error ~expected ~received =
      Error
        (`Msg
          (Format.sprintf "Expected \"%s\" but received %s" expected received))
    ;;

    let of_string string = { lexbuf = Sedlex.from_string string }
    let current_match lexer = Sedlex.lexeme lexer.lexbuf
    let current_match_length lexer = Sedlex.lexeme_length lexer.lexbuf

    let current_match_range ~from ~length lexer =
      Format.sprintf "%s" (Sedlex.sub_lexeme lexer.lexbuf from length)
    ;;

    let peek ({ lexbuf } as lexer) =
      match%sedlex lexbuf with
      | eof -> None
      | any ->
          let value = Some (current_match lexer) in
          Sedlex.rollback lexer.lexbuf;
          value
      | _ -> assert false
    ;;

    let skip_whitespace { lexbuf } =
      match%sedlex lexbuf with Star white_space -> Ok () | _ -> assert false
    ;;

    let read_whitespace ({ lexbuf } as lexer) =
      match%sedlex lexbuf with
      | white_space -> Ok ()
      | any ->
          unexpected_lex_error ~expected:"<whitespace>"
            ~received:(current_match lexer)
      | _ -> assert false
    ;;

    let read_lparen ({ lexbuf } as lexer) =
      let* _ = skip_whitespace lexer in
      match%sedlex lexbuf with
      | "(" -> Ok ()
      | any ->
          unexpected_lex_error ~expected:"(" ~received:(current_match lexer)
      | _ -> assert false
    ;;

    let read_rparen ({ lexbuf } as lexer) =
      let* _ = skip_whitespace lexer in
      match%sedlex lexbuf with
      | ")" -> Ok ()
      | any ->
          unexpected_lex_error ~expected:")" ~received:(current_match lexer)
      | _ -> assert false
    ;;

    let read_string ({ lexbuf } as lexer) =
      let* _ = skip_whitespace lexer in
      match%sedlex lexbuf with
      | '"', Star (Compl '"'), '"' ->
          Ok
            (lexer
            |> current_match_range ~from:1
                 ~length:(current_match_length lexer - 2))
      | '"', Star (Compl '"'), eof ->
          unexpected_lex_error ~expected:"<string>"
            ~received:(current_match lexer)
      | ( Compl (white_space | '(' | ')' | '#' | ';' | '\'' | '|'),
          Star (Compl (white_space | '(' | ')')) ) ->
          Ok (lexer |> current_match)
      | Star (Compl (white_space | '(' | ')')) -> Ok (lexer |> current_match)
      | eof -> unexpected_lex_error ~expected:"<string>" ~received:"<eof>"
      | _ -> assert false
    ;;

    (* Should the deserializer interface be read_uint8 instead? Do we want/need both?*)
    let read_uint8 ({ lexbuf } as lexer) =
      let* _ = skip_whitespace lexer in
      let uint8_of_string int_str =
        int_str
        |> int_of_string_opt
        |> Option.to_result ~none:(`Msg "int overflow")
        |> Fun.flip Result.bind (fun int ->
               if int < 0 || int > 255 then Error (`Msg "uint8 overflow")
               else Ok (char_of_int int))
      in
      match%sedlex lexbuf with
      | Opt '-', Plus '0' .. '9' -> lexer |> current_match |> uint8_of_string
      | any ->
          unexpected_lex_error ~expected:"<uint8>"
            ~received:(current_match lexer)
      | _ -> assert false
    ;;

    let read_int ({ lexbuf } as lexer) =
      let* _ = skip_whitespace lexer in
      match%sedlex lexbuf with
      | Opt '-', Plus '0' .. '9' -> Ok (lexer |> current_match |> int_of_string)
      | any ->
          unexpected_lex_error ~expected:"<int>" ~received:(current_match lexer)
      | _ -> assert false
    ;;

    let read_int32 ({ lexbuf } as lexer) =
      let* _ = skip_whitespace lexer in
      match%sedlex lexbuf with
      | Opt '-', Plus '0' .. '9' ->
          lexer
          |> current_match
          |> Int32.of_string_opt
          |> Option.to_result ~none:(`Msg "int32 overflow")
      | any ->
          unexpected_lex_error ~expected:"<int32>"
            ~received:(current_match lexer)
      | _ -> assert false
    ;;

    let read_int64 ({ lexbuf } as lexer) =
      let* _ = skip_whitespace lexer in
      match%sedlex lexbuf with
      | Opt '-', Plus '0' .. '9' ->
          lexer
          |> current_match
          |> Int64.of_string_opt
          |> Option.to_result ~none:(`Msg "int64 overflow")
      | any ->
          unexpected_lex_error ~expected:"<int64>"
            ~received:(current_match lexer)
      | _ -> assert false
    ;;

    let read_float ({ lexbuf } as lexer) =
      let* _ = skip_whitespace lexer in
      match%sedlex lexbuf with
      | Opt '-', Plus '0' .. '9', Opt '.', Star '0' .. '9' ->
          Ok (lexer |> current_match |> float_of_string)
      | any ->
          unexpected_lex_error ~expected:"<float>"
            ~received:(current_match lexer)
      | _ -> assert false
    ;;

    let read_bool lexer =
      let* _ = skip_whitespace lexer in
      let* identifier = read_string lexer in
      match identifier with
      | "true" -> Ok true
      | "false" -> Ok false
      | received -> unexpected_lex_error ~expected:"<bool>" ~received
    ;;
  end
end

module Fmt = struct
  let write w buf = Rio.write w ~buf |> Result.map (fun _ -> ())
  let none w = write w "()"
  let whitespace w = write w " "

  let atom w ~first value =
    match first with
    | true -> write w (Format.sprintf "%S" value)
    | false -> write w (Format.sprintf " %S" value)
  ;;

  let begin_list w = write w "("
  let end_list w = write w ")"
end

module Serializer = struct
  type output = unit
  type kind = First | Rest
  type state = S : { fmt : 'w Rio.Writer.t; mutable kind : kind } -> state

  let nest (S { fmt; _ }) = S { fmt; kind = First }

  let serialize_bool _self (S { fmt; _ }) bool =
    Rio.write_all fmt ~buf:(Format.sprintf "%S" @@ Bool.to_string bool)
  ;;

  let serialize_string _self (S { fmt; _ }) string =
    let num_words = string |> String.split_on_char ' ' |> List.length in
    if num_words > 1 then Rio.write_all fmt ~buf:(Format.sprintf "%S" string)
    else Rio.write_all fmt ~buf:string
  ;;

  let serialize_int8 _self (S { fmt; _ }) int =
    Rio.write_all fmt ~buf:(String.make 1 int)
  ;;

  let serialize_int16 _self (S { fmt; _ }) int =
    Rio.write_all fmt ~buf:(Int.to_string int)
  ;;

  let serialize_int31 _self (S { fmt; _ }) int =
    Rio.write_all fmt ~buf:(Int.to_string int)
  ;;

  let serialize_int32 _self (S { fmt; _ }) int =
    Rio.write_all fmt ~buf:(Int32.to_string int)
  ;;

  let serialize_int64 _self (S { fmt; _ }) int =
    Rio.write_all fmt ~buf:(Int64.to_string int)
  ;;

  let serialize_float _self (S { fmt; _ }) float =
    Rio.write_all fmt ~buf:(Float.to_string float)
  ;;

  let serialize_none _self (S { fmt; _ }) = Fmt.none fmt
  let serialize_some self _state value = Ser.serialize self value

  (** Serializes a sequence of elements
  {[
    let sexp = List [Atom "foo"; Atom "bar"]

    (* sexp would serialize to: 
      "(foo bar)"
    *)
  ]} *)
  let serialize_sequence self (S state) ~size elements =
    state.kind <- First;
    let* () = Fmt.begin_list state.fmt in
    let* () = if size = 0 then Ok () else Ser.serialize self elements in
    Fmt.end_list state.fmt
  ;;

  let serialize_element self (S state) elements =
    let* () = if state.kind = First then Ok () else Fmt.whitespace state.fmt in
    state.kind <- Rest;
    Ser.serialize self elements
  ;;

  (** Serializes a variant constructor that carries no data
  {[
    type unit_variant  = Single_constructor

    (* my_variant would serialize to: "(Single_constructor)" *)
    let my_variant = Single_constructor
  ]} *)
  let serialize_unit_variant _self (S state) ~var_type:_ ~cstr_idx:_ ~cstr_name
      =
    Rio.write_all state.fmt ~buf:(Format.sprintf "%S" cstr_name)
  ;;

  (** Serializes a tuple variant constructor
  {[
    type tuple_variant  = Point of int * int

    (* serializes to: "(Point (6 9))"*)
    let point = Point (6, 9) 
  ]}
  *)
  let serialize_tuple_variant self (S state) ~var_type:_ ~cstr_idx:_ ~cstr_name
      ~size values =
    state.kind <- First;
    let* () = Fmt.begin_list state.fmt in
    let* () = Fmt.atom ~first:true state.fmt cstr_name in
    let* () = Fmt.whitespace state.fmt in
    let* () = Ser.serialize_sequence self size values in
    Fmt.end_list state.fmt
  ;;

  (** Serializes a record variant constructor
  {[
    type record_variant  = Point of { x: int; y: int; }

    (* serializes to: "(Point ((x 6) (y 9)))"*)
    let point = Point { x = 6; y = 9; } 
  ]}
  *)
  let serialize_record_variant self (S state) ~var_type:_ ~cstr_idx:_ ~cstr_name
      ~size values =
    state.kind <- First;
    let* () = Fmt.begin_list state.fmt in
    let* () = Fmt.atom ~first:true state.fmt cstr_name in
    let* () = Ser.serialize_record self "" size values in
    Fmt.end_list state.fmt
  ;;

  (** Serializes a newtype variant constructor
  {[
    type newtype_variant  = Int of int

    (* serializes to: "(Int 69)"*)
    let my_int = Int 69
  ]}
  *)
  let serialize_newtype_variant self (S state) ~var_type:_ ~cstr_idx:_
      ~cstr_name field =
    state.kind <- First;
    let* () = Fmt.begin_list state.fmt in
    let* () = Fmt.atom ~first:true state.fmt cstr_name in
    let* () = Fmt.whitespace state.fmt in
    let* () = Ser.serialize self field in
    Fmt.end_list state.fmt
  ;;

  (** Serializes a record
  {[
    type point  = { x: int; y: int; }

    (* serializes to: "((x 6) (y 9))"*)
    let point = { x = 6; y = 9; }
  ]}
  *)
  let serialize_record self (S { fmt; _ }) ~rec_type:_ ~size:_ values =
    let* () = Fmt.begin_list fmt in
    let* () = Ser.serialize self values in
    Fmt.end_list fmt
  ;;

  (** Serializes a records field
  {[
    type point  = { x: int; y: int; }

    let point = { x = 6; y = 9; } in

    (* serializes to: "(x 6)"*)
    let x = point.x
  ]}
  *)
  let serialize_field self (S state) ~name values =
    let* () = Fmt.begin_list state.fmt in
    let* () = Fmt.atom ~first:true state.fmt name in
    let* () = Fmt.whitespace state.fmt in
    let* () = Ser.serialize self values in
    let* () = Fmt.end_list state.fmt in
    Ok ()
  ;;
end

module Deserializer = struct
  open Sexpr

  type kind = First | Rest
  type state = { reader : Parser.t; mutable kind : kind }

  let nest { reader; _ } = { reader; kind = First }
  let deserialize_int8 _self state = Parser.read_uint8 state.reader
  let deserialize_int16 _self state = Parser.read_int state.reader
  let deserialize_int31 _self state = Parser.read_int state.reader
  let deserialize_int32 _self state = Parser.read_int32 state.reader
  let deserialize_int64 _self state = Parser.read_int64 state.reader
  let deserialize_float _self state = Parser.read_float state.reader
  let deserialize_bool _self state = Parser.read_bool state.reader
  let deserialize_string _self state = Parser.read_string state.reader

  let deserialize_option self { reader; _ } de =
    match Parser.peek reader with
    | Some "(" ->
        let* () = Parser.read_lparen reader in
        let* () = Parser.read_rparen reader in
        Ok None
    | _ ->
        let* value = De.deserialize self de in
        Ok (Some value)
  ;;

  let deserialize_identifier self _state visitor =
    let* str = De.deserialize_string self in
    Visitor.visit_string self visitor str
  ;;

  let deserialize_sequence self s ~size de =
    let* () = Parser.read_lparen s.reader in
    let* v = De.deserialize self (de ~size) in
    let* () = Parser.read_rparen s.reader in
    Ok v
  ;;

  let deserialize_element self s de =
    match Parser.peek s.reader with
    | Some ")" -> Ok None
    | _ ->
        let* () =
          if s.kind = First then Ok () else Parser.read_whitespace s.reader
        in
        s.kind <- Rest;
        let* v = De.deserialize self de in
        Ok (Some v)
  ;;

  let deserialize_unit_variant _self _state = Ok ()

  (** Deserializes a newtype variant constructor
  {[
    type newtype_variant  = Int of int

    (* deserializes from: "(Int 69)"*)
    let my_int = Int 69
  ]}
  *)
  let deserialize_newtype_variant self _state de = De.deserialize self de

  (** Deserializes a tuple variant constructor
  {[
    type tuple_variant  = Point of int * int

    (* deserializes from: "(Point (6 9))"*)
    let point = Point (6, 9) 
  ]}
  *)
  let deserialize_tuple_variant self { reader; _ } ~size de =
    match Parser.peek reader with
    | Some "(" ->
        let* () = Parser.read_lparen reader in
        let* value = De.deserialize_sequence self size de in
        let* () = Parser.read_rparen reader in
        Ok value
    | Some _ -> De.deserialize_sequence self size de
    | _ -> assert false
  ;;

  (** Serializes a record variant constructor
  {[
    type record_variant  = Point of { x: int; y: int; }

    (* serializes to: "(Point ((x 6) (y 9)))"*)
    let point = Point { x = 6; y = 9; } 
  ]}
  *)
  let deserialize_record_variant self { reader = _; _ } ~size de =
    De.deserialize_record self "" size (de ~size)
  ;;

  let deserialize_variant self { reader; _ } de ~name:_ ~variants:_ =
    match Parser.peek reader with
    | Some "(" ->
        let* () = Parser.read_lparen reader in
        let* value = De.deserialize self de in
        let* () = Parser.read_rparen reader in
        Ok value
    | Some "\"" -> De.deserialize self de
    | _ -> assert false
  ;;

  (** Deserializes a record
  {[
    type point  = { x: int; y: int; }

    (* Deserializes from: "((x 6) (y 9))"*)
    let point = { x = 6; y = 9; }
  ]}
  *)
  let deserialize_record self { reader; _ } ~name:_ ~size:_ fields =
    let* () = Parser.read_lparen reader in
    let* value = De.deserialize self fields in
    let* () = Parser.read_rparen reader in
    Ok value
  ;;

  let deserialize_key self state visitor =
    match Parser.peek state.reader with
    | Some ")" -> Ok None
    | _ ->
        let* () = Parser.read_lparen state.reader in
        let* str = Parser.read_string state.reader in
        let* key = Visitor.visit_string self visitor str in
        let* () = Parser.read_whitespace state.reader in
        Ok (Some key)
  ;;

  (** Deserializes a records field
  {[
    type point  = { x: int; y: int; }

    let point = { x = 6; y = 9; } in
    let x = point.x
  ]}
  *)
  let deserialize_field self state ~name:_ de =
    let value = De.deserialize self de in
    let* () = Parser.read_rparen state.reader in
    value
  ;;

  let deserialize_ignored_any _self _s = failwith "unexpect ignored_any"
end

let to_string ser value =
  let buf = Buffer.create 0 in
  let state = Serializer.S { fmt = Rio.Buffer.to_writer buf; kind = First } in
  let* _ = Serde.serialize (module Serializer) state ser value in
  Ok (Buffer.to_bytes buf |> Bytes.unsafe_to_string)
;;

let of_string de string =
  let reader = Sexpr.Parser.of_string string in
  let state = Deserializer.{ reader; kind = First } in
  Serde.deserialize (module Deserializer) state de
;;
