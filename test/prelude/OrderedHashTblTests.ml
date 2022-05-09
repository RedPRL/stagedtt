open Prelude

module Tbl = OrderedHashTbl.Make(String)

let test_create size () =
  let _ = Tbl.create size
  in ()

let test_add_get () =
  let tbl = Tbl.create 128 in
  let v = 1 in
  Tbl.add "aaa" v tbl;
  let res = Tbl.get "aaa" tbl in
  Alcotest.(check (option int)) "We can read our writes" (Some v) res 

let test_get_missing () =
  let tbl = Tbl.create 128 in
  let v = 1 in
  Tbl.add "aaa" v tbl;
  let res = Tbl.get "bbb" tbl in
  Alcotest.(check (option int)) "Missing keys resolve to None" None res 

let test_add_many size num_items read_item () =
  let tbl = Tbl.create size in
  for i=0 to num_items do
    Tbl.add (Int.to_string i) i tbl;
  done;
  let res = Tbl.get (Int.to_string read_item) tbl in
  Alcotest.(check (option int)) "Read back our write" (Some read_item) res

let test_pop_many size num_items () =
  let tbl = Tbl.create size in
  let expected = Array.init (num_items + 1) (fun ix -> num_items - ix) in
  let actual = Array.make (num_items + 1) (-1) in
  for i=0 to num_items do
    Tbl.add (Int.to_string i) i tbl;
  done;
  for i=0 to num_items do
    actual.(i) <- snd (Tbl.pop tbl);
  done;
  Alcotest.(check (array int)) "Items get popped in correct order" expected actual

let test_get_idx size num_items read_item () =
  let tbl = Tbl.create size in
  for i=0 to num_items do
    Tbl.add (Int.to_string i) i tbl;
  done;
  let res = Tbl.get_idx (Int.to_string read_item) tbl in
  Alcotest.(check (option int)) "We get the correct index" (Some read_item) res

let tests = [
  Alcotest.test_case "Create a small hashmap" `Quick @@ test_create 100;
  Alcotest.test_case "Create a big hashmap" `Slow @@ test_create 100000;
  Alcotest.test_case "Insert a single value" `Quick test_add_get;
  Alcotest.test_case "Read a missing value" `Quick test_get_missing;
  Alcotest.test_case "Insert a few items" `Quick @@ test_add_many 128 64 32;
  Alcotest.test_case "Insert many items [resize]" `Quick @@ test_add_many 128 1024 512;
  Alcotest.test_case "Pop a few items" `Quick @@ test_pop_many 128 64;
  Alcotest.test_case "Pop many items [resize]" `Quick @@ test_pop_many 128 1024;
  Alcotest.test_case "Get index of a key" `Quick @@ test_get_idx 128 64 32;
  Alcotest.test_case "Get index of a key [resize]" `Quick @@ test_get_idx 128 1024 512;
]
