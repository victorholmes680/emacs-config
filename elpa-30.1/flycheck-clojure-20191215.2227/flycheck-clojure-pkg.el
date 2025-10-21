;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "flycheck-clojure" "20191215.2227"
  "Flycheck: Clojure support."
  '((cider     "0.22.0")
    (flycheck  "32-cvs")
    (let-alist "1.0.1")
    (emacs     "25"))
  :url "https://github.com/clojure-emacs/squiggly-clojure"
  :commit "bc85f9dfe1bcfa66a98d2ca5da955e7eab4ae00d"
  :revdesc "bc85f9dfe1bc"
  :authors '(("Peter Fraenkel" . "pnf@podsnap.com")
             ("Sebastian Wiesner" . "swiesner@lunaryorn.com"))
  :maintainers '(("Peter Fraenkel" . "pnf@podsnap.com")))
