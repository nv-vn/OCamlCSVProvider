open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Batteries

open Lwt
open Cohttp
open Cohttp_lwt_unix

let get_csv url =
  let is_web =
    let open Batteries in
    String.starts_with url "http://" || String.starts_with url "https://" in
  if is_web then
    Client.get (Uri.of_string url) >>= fun (resp, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body -> body
  else
    let fd = Lwt_main.run @@ Lwt_io.open_file Input url in
    Lwt_io.read fd

let infer s =
  begin try (int_of_string s; "int") with
    | _ ->
      begin try (float_of_string s; "float") with
        | _ -> "string"
      end
  end

let inferf loc s i =
  infer s |> function
  | "int" -> Exp.apply ~loc (Exp.ident ~loc { txt = Lident "int_of_string"; loc = loc }) [Nolabel, i]
  | "float" -> Exp.apply ~loc (Exp.ident ~loc { txt = Lident "float_of_string"; loc = loc }) [Nolabel, i]
  | "string" -> i

let inferf' loc s i =
  infer s |> function
  | "int" -> Exp.apply ~loc (Exp.ident ~loc { txt = Lident "string_of_int"; loc = loc }) [Nolabel, i]
  | "float" -> Exp.apply ~loc (Exp.ident ~loc { txt = Lident "string_of_float"; loc = loc }) [Nolabel, i]
  | "string" -> i

let replace_keyword s s' str =
  String.replace ~str ~sub:s ~by:s' |> snd

let rec replace_keywords str = function
  | [] -> str
  | s :: t -> replace_keywords (replace_keyword s (s ^ "_") str) t

let lexer_friendly str =
  let stripped = String.replace_chars (function ' ' -> "" | '\t' -> "" | c -> String.of_char c)  str in
  let uncaps = String.uncapitalize stripped in
  replace_keywords uncaps ["and"; "as"; "assert"; "begin"; "class"; "constraint"; "do"; "done"; "downto";
                           "else"; "end"; "exception"; "external"; "false"; "for"; "fun"; "function";
                           "functor"; "if"; "in"; "include"; "inherit"; "initializer"; "lazy"; "let";
                           "match"; "method"; "module"; "mutable"; "new"; "object"; "of"; "open"; "or";
                           "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val";
                           "virtual"; "when"; "while"; "with"]

let record_of_list loc list example =
  let fields = List.map2 (fun i e ->
                           Type.field ~loc {txt = lexer_friendly i; loc = loc}
                             (Typ.constr {txt = Lident (infer e); loc = loc} []))
      list example in
  Str.type_ ~loc Nonrecursive [Type.mk {txt = "row"; loc = loc} ~kind:(Ptype_record fields)]

let converter_of_list loc list example =
  let names = List.map (fun n -> lexer_friendly n) list in
  let pattern =
    List.fold_right
      (fun n r -> Pat.construct ~loc { txt = Lident "::"; loc = loc} (Some (Pat.tuple ~loc [Pat.var {txt = n; loc = loc}; r])))
      names (Pat.construct ~loc { txt = Lident "[]"; loc = loc} None) in
  let rhs =
    Exp.record ~loc
      (List.map2 (fun i e ->
                   let n = { txt = Lident (lexer_friendly i); loc = loc } in
                   (n, inferf loc e (Exp.ident ~loc n)))
          list example) None in
  let matcher = Exp.function_ ~loc [Exp.case pattern rhs] in
  [%stri let row_of_list = [%e matcher]]

let formatter_of_list loc list example =
  let names = List.map2 (fun n e -> (lexer_friendly n, e)) list example in
  let str =
    List.fold_right
      (fun (n, e) r -> Exp.apply ~loc
                         (Exp.ident ~loc { txt = Lident "^^^"; loc = loc })
                         [(Nolabel, inferf' loc e (Exp.field ~loc (Exp.ident ~loc { txt = Lident "x"; loc = loc }) { txt = Lident n; loc = loc }));
                          (Nolabel, r)])
      names (Exp.constant ~loc (Pconst_string ("", None))) in
  [%stri let show (h, xs) =
           let (^^^) a b = if b = "" then a else a ^ " | " ^ b in
           let header = List.fold_right (^^^) h "" ^ "\n"
           and msg = List.fold_right (^) (List.mapi (fun i x -> (string_of_int i ^ ". ") ^ [%e str] ^ "\n") xs) ""
           in header ^ msg]

let rec make_list loc f = function
  | [] -> Exp.construct ~loc { txt = Lident "[]"; loc = loc } None
  | h :: t -> Exp.construct ~loc { txt = Lident "::"; loc = loc } (Some (Exp.tuple ~loc [f h; make_list loc f t]))

let stringify_of_list loc list example =
  let names = List.map2 (fun n e -> (lexer_friendly n, e)) list example in
  let str = make_list loc (fun (n, e) ->
                            inferf' loc e (Exp.field ~loc (Exp.ident ~loc { txt = Lident "x"; loc = loc }) { txt = Lident n; loc = loc })) names in
  [%stri let list_of_row x = [%e str]]

let ast_of_csv loc =
  let f = make_list loc (fun x -> Exp.constant ~loc (Pconst_string (x, None))) in
  make_list loc f

let struct_of_url ?(sep=',') url loc =
  get_csv url >>= fun text ->
  let data = Csv.of_string ~separator:sep text |> Csv.input_all in
  let format = List.hd data
  and rows = List.tl data in
  let headers = make_list loc (fun x -> Exp.constant ~loc (Pconst_string (x, None))) format in
  let embed = [%stri let embed = ([%e headers], [%e ast_of_csv loc rows])]
  and type_ = record_of_list loc format (List.hd rows)
  and conv = converter_of_list loc format (List.hd rows)
  and show = formatter_of_list loc format (List.hd rows)
  and raw_ = stringify_of_list loc format (List.hd rows) in
  return @@ Mod.structure ~loc [embed;
                                type_;
                                conv;
                                show;
                                raw_;
                                [%stri let raw (h, xs) =
                                         (h, List.map list_of_row xs)];
                                [%stri let load ?(sep=',') url =
                                         let open Lwt in
                                         let open Cohttp in
                                         let open Cohttp_lwt_unix in
                                         let get = Client.get (Uri.of_string url) >>= fun (resp, body) ->
                                                   body |> Cohttp_lwt_body.to_string >|= fun body -> body
                                         in get >>= fun text ->
                                         return (Csv.of_string ~separator:sep text |> Csv.input_all
                                                 |> fun (h::xs) -> (h, xs))];
                                [%stri let local_load ?(sep=',') filename =
                                         let open Lwt in
                                         let fd = Lwt_main.run @@ Lwt_io.open_file Input filename in
                                         Lwt_io.read fd >>= fun text ->
                                         return (Csv.of_string ~separator:sep text |> Csv.input_all
                                                 |> fun (h::xs) -> (h, xs))];
                                [%stri let save ?(sep=',') ~name data =
                                         let data' = fst data :: snd data in
                                         Lwt.return @@ Csv.save ~separator:sep name data'];
                                [%stri let rows data = (fst data, List.map row_of_list (snd data))];
                                [%stri let rec take ?(acc=[]) amount list = match amount, snd list with
                                       | 0, _ | _, [] -> (fst list, acc)
                                       | n, x :: xs -> take ~acc:(acc @ [x]) (pred n) (fst list, xs)];
                                [%stri let rec drop amount = function
                                       | (h, _ :: xs) when amount > 0 -> (h, xs)
                                       | (h, xs) -> (h, xs)];
                                [%stri let rec truncate amount list =
                                         if amount >= List.length (snd list) then (fst list, [])
                                         else take (List.length (snd list) - amount) list];
                                [%stri let range ~from ~until list =
                                         take (until - from + 1) @@ drop from list];
                                [%stri let get_sample ?(amount = 10) data =
                                         (fst data, List.map row_of_list (snd (take amount data)))];
                                [%stri let map f (h, xs) = (h, List.map f xs)];
                                [%stri let filter p (h, xs) = (h, List.filter p xs)]]

let csv_mapper argv =
  {default_mapper with
   module_expr = begin fun mapper mod_expr ->
     match mod_expr with
     | { pmod_attributes; pmod_loc; pmod_desc = Pmod_extension ({txt = "csv"; loc}, pstr) } ->
       begin match pstr with
         | PStr [{ pstr_desc =
                     Pstr_eval ({ pexp_loc = loc;
                                  pexp_desc = Pexp_constant (Pconst_string (sym, None))}, _)}] ->
           Lwt_main.run @@ struct_of_url sym loc
         | PStr [{ pstr_desc =
                     Pstr_eval ({ pexp_loc = loc;
                                  pexp_desc = Pexp_tuple
                                      [{ pexp_desc = Pexp_constant (Pconst_string (sym, None)); _ };
                                       { pexp_desc = Pexp_constant (Pconst_char sep); _ }]}, _)}] ->
           Lwt_main.run @@ struct_of_url ~sep sym loc
         | _ ->
           raise (Location.Error
                    (Location.error ~loc "[%csv ...] accepts a string, e.g. [%csv \"https://google.com\"]"))
       end
     | x -> default_mapper.module_expr mapper x
   end
  }
