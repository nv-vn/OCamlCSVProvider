open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Lwt
open Cohttp
open Cohttp_lwt_unix

let get_csv url =
  let is_web =
    let open Batteries in
    String.starts_with url "http://" || String.starts_with url "https://" in
  if is_web then
    Client.get (Uri.of_string url) >>= fun (resp, body) ->
    body |> Cohttp_lwt_body.to_string >|= fun body -> body
  else
    let fd = Lwt_unix.run @@ Lwt_io.open_file Input url in
    Lwt_io.read fd

let infer s =
  begin try (int_of_string s; "int") with
    | _ ->
      begin try (float_of_string s; "float") with
        | _ -> "string"
      end
  end

let lexer_friendly str =
  let open Batteries in
  let stripped = String.replace_chars (function ' ' -> "" | '\t' -> "" | c -> String.of_char c)  str in
  String.uncapitalize stripped

let record_of_list loc list example =
  let fields = List.map2 (fun i e ->
                           Type.field ~loc {txt = lexer_friendly i; loc = loc}
                             (Typ.constr {txt = Lident (infer e); loc = loc} []))
      list example in
  Str.type_ ~loc [Type.mk {txt = "t"; loc = loc} ~kind:(Ptype_record fields)]

let rec make_list loc f = function
  | [] -> Exp.construct ~loc { txt = Lident "[]"; loc = loc } None
  | h :: t -> Exp.construct ~loc { txt = Lident "::"; loc = loc } (Some (Exp.tuple ~loc [f h; make_list loc f t]))

let ast_of_csv loc =
  let f = make_list loc (fun x -> Exp.constant ~loc (Const_string (x, None))) in
  make_list loc f

let struct_of_url ?(sep=',') url loc =
  get_csv url >>= fun text ->
  let data = Csv.of_string ~separator:sep text |> Csv.input_all in
  let format = List.hd data
  and rows = List.tl data in
  let embed = [%stri let embed = ref [%e ast_of_csv loc data]]
  and type_ = record_of_list loc format (List.hd rows) in
  return @@ Mod.structure ~loc [embed; (* Maybe we should abandon the F#-style interface *)
                                type_; (* and shift to a more idiomatic OCaml style?     *)
                                [%stri let load ?(sep=',') url = embed := !embed];
                                [%stri let rows () = !embed];
                                [%stri let get_sample () = !embed]]

let csv_mapper argv =
  {default_mapper with
   module_expr = begin fun mapper mod_expr ->
     match mod_expr with
     | { pmod_attributes; pmod_loc; pmod_desc = Pmod_extension ({txt = "csv"; loc}, pstr) } ->
       begin match pstr with
         | PStr [{ pstr_desc =
                     Pstr_eval ({ pexp_loc = loc;
                                  pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] ->
           Lwt_unix.run @@ struct_of_url sym loc
         | PStr [{ pstr_desc =
                     Pstr_eval ({ pexp_loc = loc;
                                  pexp_desc = Pexp_tuple
                                      [{ pexp_desc = Pexp_constant (Const_string (sym, None)); _ };
                                       { pexp_desc = Pexp_constant (Const_char sep); _ }]}, _)}] ->
           Lwt_unix.run @@ struct_of_url ~sep sym loc
         | _ ->
           raise (Location.Error
                    (Location.error ~loc "[%csv ...] accepts a string, e.g. [%csv \"https://google.com\"]"))
       end
     | x -> default_mapper.module_expr mapper x
   end
  }

let _ = register "csv_provider" csv_mapper
