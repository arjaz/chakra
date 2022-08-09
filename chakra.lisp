;;;; chakra.lisp

(in-package #:chakra)

;; exports all symbols in package
;; seems reckless, but convenient
(let ((pack (find-package :chakra)))
  (do-all-symbols (sym pack)
    (when (eql (symbol-package sym) pack)
      (export sym))))




