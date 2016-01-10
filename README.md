# OCamlCSVProvider
F# CSV type provider ported to OCaml

# Using csvprovider
In order to use the csvprovider in your OCaml code, use the findlib package
named `ppx_csv_provider`. Additionally, you must compile with the packages:
`csv`, `lwt`, and `cohttp.lwt`.

# Tutorial
`ppx_csv_provider` defines a single extension, which occurs at the module
level. In order to create a type provider for your data, simpy invoke the
`[%csv ...]` extension with a URL to a local file (accessible at compile-
time) or a CSV file hosted online. This will then generate a `struct` for
you to use in a module expression, such as assigning that to a module or
passing it as an argument to a functor. Finally, to use an alternative
separator (the default being a comma), you can use add a comma, followed
by a character, into the extension syntax. For example:

```ocaml
module MyTest = [%csv "./myTestFile.csv", ';'] (* ';' is the separator *)
module OtherTest = CsvInterface.Make ([%csv "http://example.com/test.csv"])
```

After creating your module, it will match the following signature:

```ocaml
(* Record that represents a single row in your data, generated based on simple type inference rules *)
type row
(* Embedded copy of the file used to generate the interface from *)
val embed : string list * string list list
(* Converts a single row of type string list into a value of type row *)
val row_of_list : string list -> row
(* Returns a string-representation of the CSV file *)
val show : string list * row list -> string
(* Returns the original format of the row, as a string list *)
val list_of_row : row -> string list
(* Gives the original representation of the csv file *)
val raw : string list * row list -> string list * string list list
(* Loads a CSV file from the internet, given a URL and optional separator to use instead of "," *)
val load : ?sep:char -> string -> (string list * string list list) Lwt.t
(* Loads a CSV file from the local filesystem, given a URL and optional separator to use instead of "," *)
val local_load : ?sep:char -> string -> (string list * string list list) Lwt.t
(* Saves the data to a CSV file, given an optional separator and a filename *)
val save : ?sep:char -> ~name:string -> string list * string list list -> unit Lwt.t
(* Returns the data representation of the raw data *)
val rows : string list * string list list -> string list * row list
(* Takes the first `n` rows from the given data *)
val take : int -> string list * row list -> string list * row list
(* Drops the first `n` rows from the given data *)
val drop : int -> string list * row list -> string list * row list
(* Truncates the last `n` rows from the given data *)
val truncate : int -> string list * row list -> string list * row list
(* Takes all items from `from` to `until` from the given data *)
val range : ~from:int -> ~until:int -> string list * row list -> string list * row list
(* Takes a raw list and converts it to the standard representation, taking `amount` (default 10) items *)
val get_sample : ?amount:int -> string list * string list list -> string list * row list
(* Maps a function over each item of the data *)
val map : (row -> row) -> string list * row list -> string list * row list
(* Filters the given data, removing all items that return false when applied to the predicate *)
val filter : (row -> bool) -> string list * row list -> string list * row list
```
