# OCamlCSVProvider
F# CSV type provider ported to OCaml

# Todo:
* Generate loader function
* Switch from F#-style "class" interface
  - Use returned data instead of mutable `embed`
* Add more functions:
  - MyData.map : row list -> (row -> row) -> row list
  - MyData.filter : row list -> (row -> bool) -> row list
  - MyData.save : row list -> name:string -> () Lwt.t
  - MyData.range : row list -> top:int -> bottom:int -> row list
* Filter out keywords
