# OCamlCSVProvider
F# CSV type provider ported to OCaml

# Todo:
* Generate lexer-friendly names
  - Or use Haskell-style record convenience functions?
* Generate `t_of_string_list` functions
  - Rename `t` to `row`?
* Generate loader function
* Switch from F#-style "class" interface
  - Use returned data instead of mutable `embed`
* Add more functions:
  - MyData.map : row list -> (row -> row) -> row list
  - MyData.filter : row list -> (row -> bool) -> row list
  - MyData.save : row list -> name:string -> () Lwt.t
  - MyData.truncate : row list -> int -> row list
  - MyData.take : row list -> int -> row list
  - MyData.range : row list -> top:int -> bottom:int -> row list
