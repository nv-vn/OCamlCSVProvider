# OCamlCSVProvider
F# CSV type provider ported to OCaml

# Todo:
* Generate loader function
* Add more functions:
  - MyData.load : string -> string list list Lwt.t
  - MyData.save : row list -> name:string -> () Lwt.t
  - MyData.show : row list -> string
* Filter out keywords
