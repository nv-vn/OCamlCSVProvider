module M = [%csv "http://ichart.finance.yahoo.com/table.csv?s=MSFT"]
module N = [%csv "./test.csv"]

let _ =
  let open N in
  print_endline "Date | Open | High | Low | Close | Volume | Adj Close";
  List.map
    (function
      [d; o; h; l; c; v; a] ->
        Printf.printf "%s | %s | %s | %s | %s | %s | %s\n" d o h l c v a) !embed;
  print_endline "Date | Open | High | Low | Close | Volume | Adj Close";
  List.map
    (function
      {date = d; open_ = o; high = h; low = l; close = c; volume = v; adjClose = a} ->
        Printf.printf "%s | %f | %f | %f | %f | %d | %f\n" d o h l c v a)
    (N.rows ())
