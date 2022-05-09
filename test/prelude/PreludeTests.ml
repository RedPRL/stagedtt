module Tbl = OrderedHashTblTests

let () = Alcotest.run "Prelude Tests" [
    "Hash Tables", Tbl.tests
  ]
