# OCamlCSVProvider
F# CSV type provider ported to OCaml

# Todo:
* Generate loader function
* Add more functions:
  - MyData.map : row list -> (row -> row) -> row list
  - MyData.filter : row list -> (row -> bool) -> row list
  - MyData.save : row list -> name:string -> () Lwt.t
* Filter out keywords
