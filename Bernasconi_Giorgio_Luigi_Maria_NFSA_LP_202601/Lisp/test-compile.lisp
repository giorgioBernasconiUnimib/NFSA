(load "nfsa.lisp")

(defun check (name ok)
  (format t "~&~A: ~A~%" name (if ok "OK" "FAIL"))
  (assert ok))

(defun safe-recognize (fa input)
  (handler-case
      (nfsa-recognize fa input)
    (error () :error)))

(defun run-recognize-tests ()
  ;; Esempio prof: basic-nfsa-1 = regex 'a
  (let ((fa (nfsa-compile-regex 'a)))
    (check "a: input non-lista -> NIL"
           (null (safe-recognize fa 'a)))
    (check "a: (a) -> T"
           (eq (safe-recognize fa '(a)) t))
    (check "a: () -> NIL"
           (null (safe-recognize fa '())))

    (check "a: (b) -> NIL"
           (null (safe-recognize fa '(b)))))

  ;; Esempio prof: (c 0 (o 1) 0)
  (let ((fa (nfsa-compile-regex '(c 0 (o 1) 0))))
    (check "c0(o1)0: (0 1 0) -> T"
           (eq (safe-recognize fa '(0 1 0)) t))
    (check "c0(o1)0: (0 1 1 1 0) -> T"
           (eq (safe-recognize fa '(0 1 1 1 0)) t))
    (check "c0(o1)0: (0 0) -> NIL"
           (null (safe-recognize fa '(0 0))))
    (check "c0(o1)0: (0 1) -> NIL"
           (null (safe-recognize fa '(0 1)))))

  ;; Kleene star: z(a) accetta anche vuoto
  (let ((fa (nfsa-compile-regex '(z a))))
    (check "z(a): () -> T"
           (eq (safe-recognize fa '()) t))
    (check "z(a): (a a a) -> T"
           (eq (safe-recognize fa '(a a a)) t))
    (check "z(a): (b) -> NIL"
           (null (safe-recognize fa '(b)))))

  ;; Plus: o(a) NON accetta vuoto
  (let ((fa (nfsa-compile-regex '(o a))))
    (check "o(a): () -> NIL"
           (null (safe-recognize fa '())))
    (check "o(a): (a) -> T"
           (eq (safe-recognize fa '(a)) t))
    (check "o(a): (a a) -> T"
           (eq (safe-recognize fa '(a a)) t)))

  ;; Concat con 1 arg: c(a) (ammessa) equivale ad a
  (let ((fa (nfsa-compile-regex '(c a))))
    (check "c(a): (a) -> T"
           (eq (safe-recognize fa '(a)) t))
    (check "c(a): () -> NIL"
           (null (safe-recognize fa '()))))

  ;; Error richiesto: FA non valido
  (check "FA non valido -> ERROR"
         (eq (safe-recognize 42 '(1 2 3)) :error))

  (format t "~&Tutti i test recognize sono passati.~%"))

(run-recognize-tests)
(quit)
