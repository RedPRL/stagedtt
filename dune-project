(lang dune 2.9)
(using menhir 1.0)
;; Dune uses ocamlformat for automatic formatting,
;; which I don't really like. Therefore, we disable
;; it in favor of ocp-indent as an editor plugin.
(formatting disabled)
;; Let's generate the requisite opam files instead
;; of writing them by hand.
(generate_opam_files true)

(name stagedtt)

(authors "Reed Mullanix")
(maintainers "Reed Mullanix")

(source (github TOTBWF/stagedtt))
(homepage "https://www.github.com/TOTBWF/stagedtt")
(bug_reports "https://www.github.com/TOTBWF/stagedtt")


(package
  (name stagedtt)
  (synopsis "A staged type theory")
  (depends
    (bwd (>= 1.2))
    (cmdliner (>= 1.1))
    (containers (>= 3.6))
    (ppxlib (>= 0.25.0))
    ;; Pinned Deps
    algaeff
    bantorra
    yuujinchou
    ppx_effects
    ;; Tests
    (alcotest (>= 1.5 :with-test))
    (bechamel (>= 0.2 :with-test))
    (bechamel-notty (>= 0.2 :with-test))))

