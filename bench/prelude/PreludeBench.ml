open Bechamel
open Toolkit

module BenchTbl = OrderedHashTblBench

let tbl_size = 1024

let test  = Test.make_grouped ~name:"Tables:" ~fmt:"%s %s" [
    BenchTbl.test_create_ordered_tbl tbl_size;
    BenchTbl.test_create_stdlib_tbl tbl_size;
    BenchTbl.test_lookup_ordered_tbl tbl_size;
    BenchTbl.test_lookup_stdlib_tbl tbl_size;
  ]

let benchmark () =
  let instances = Instance.[ monotonic_clock ] in
  let cfg = Benchmark.cfg ~limit:2000 ~stabilize:true
      ~quota:(Time.second 0.5) () in
  Benchmark.all cfg instances test

let analyze results =
  let ols = Analyze.ols ~bootstrap:0 ~r_square:true
      ~predictors:[| Measure.run |] in
  let results = Analyze.all ols Instance.monotonic_clock results in
  Analyze.merge ols [ Instance.monotonic_clock ] [ results ]

let () =
  Bechamel_notty.Unit.add
    Instance.monotonic_clock
    (Measure.unit Instance.monotonic_clock)

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let () =
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w= 80; h= 1; } in
  let results = benchmark () in
  let results = analyze results in
  img (window, results) |> eol |> output_image
