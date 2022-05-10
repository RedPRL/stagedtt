open Prelude
open Bechamel

module StdTbl = Hashtbl.Make(String)
module OTbl = OrderedHashTbl.Make(String)

let init_ordered_table size () =
  let tbl = OTbl.create size in
  for i=0 to size do
    OTbl.push (Int.to_string i) i tbl
  done;
  tbl

let init_stdlib_table size () =
  let tbl = StdTbl.create size in
  for i=0 to size do
    StdTbl.add tbl (Int.to_string i) i 
  done;
  tbl

(* Creation *)
let test_create_ordered_tbl size =
  Test.make ~name:"create ordered" @@
  Staged.stage @@ fun () ->
  let tbl = OTbl.create size in
  for i=0 to size do
    OTbl.push (Int.to_string i) i tbl
  done

let test_create_stdlib_tbl size =
  Test.make ~name:"create stdlib" @@
  Staged.stage @@ fun () ->
  let tbl = StdTbl.create size in
  for i=0 to size do
    StdTbl.add tbl (Int.to_string i) i 
  done

(* Lookups *)
let test_lookup_ordered_tbl size =
  Test.make_with_resource ~name:"lookup ordered" Test.uniq ~allocate:(init_ordered_table size) ~free:(fun _ -> ()) @@
  Staged.stage @@ fun tbl ->
  for i=0 to size do
    ignore @@ OTbl.find (Int.to_string i) tbl
  done

let test_lookup_stdlib_tbl size =
  Test.make_with_resource ~name:"lookup stdlib" Test.uniq ~allocate:(init_stdlib_table size) ~free:(fun _ -> ()) @@
  Staged.stage @@ fun tbl ->
  for i=0 to size do
    ignore @@ StdTbl.find tbl (Int.to_string i)
  done
