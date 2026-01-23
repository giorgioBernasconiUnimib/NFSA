;;;;Bernasconi	Giorgio	Luigi	Maria	885948


;;; Struttura dell'automa NFSA
(defstruct nfsa
  init
  finals
  delta)

;;;Definisce le epsilon transizioni con gensym, evita di riservare "EPS"
(defparameter epsilon-label (gensym "EPS"))


;;;Variabili globali
(defparameter reserved-regex-ops '(c a z o))


;;;Funzioni ausiliari usate nel codice


;;; reserved-regex-op-p
;;; True se X è uno degli operatori riservati delle regex (c, a, z, o).
(defun reserved-regex-op-p (x)
  (and (symbolp x)
       (member x reserved-regex-ops :test #'eq)))


;;; proper-list-p
;;; Verifica che X sia una lista propria (no dotted pair, no liste cicliche).
(defun proper-list-p (x)
  (loop with slow = x
        with fast = x
        do (cond
             ((null fast) (return t))
             ((atom fast) (return nil))
             ((null (cdr fast)) (return t))
             ((atom (cdr fast)) (return nil)))
           (setf slow (cdr slow)
                 fast (cddr fast))
        when (eq slow fast) do (return nil)))

;;;Aggiorna gli stati in modo che abbiano nomi diversi
(defun fresh-state ()
  (gensym "Q"))

;;;aggiunge davanti a delta la tripla da aggiungere
(defun add-delta (from label to delta)
  (cons (list from label to) delta))

;;;evita warning in compilazione dato che le funzioni si chiamano a vicenda
(declaim (ftype function compile-frag compile-concat compile-alt))


;;; Compilazione a frammenti
(defun compile-frag (re)
  (cond
    ;;caso z
    ((and (consp re) (eq (car re) 'z))
     (let ((s (fresh-state))
           (f (fresh-state)))
       (multiple-value-bind (s1 f1 d1) (compile-frag (cadr re))
         (let ((d d1))
           (setf d (add-delta s  epsilon-label f  d))
           (setf d (add-delta s  epsilon-label s1 d))
           (setf d (add-delta f1 epsilon-label s1 d))
           (setf d (add-delta f1 epsilon-label f  d))
           (values s f d)))))
    ;;caso o
    ((and (consp re) (eq (car re) 'o))
     (let ((s (fresh-state))
           (f (fresh-state)))
       (multiple-value-bind (s1 f1 d1) (compile-frag (cadr re))
         (let ((d d1))
           (setf d (add-delta s  epsilon-label s1 d))
           (setf d (add-delta f1 epsilon-label s1 d))
           (setf d (add-delta f1 epsilon-label f  d))
           (values s f d)))))
    ;;caso c
    ((and (consp re) (eq (car re) 'c))
     (compile-concat (cdr re)))
    ;;caso a
    ((and (consp re) (eq (car re) 'a))
     (compile-alt (cdr re)))
    ;;Simbolo dell'alfabeto
    (t
     (let ((s (fresh-state))
           (f (fresh-state))
           (d nil))
       (setf d (add-delta s re f d))
       (values s f d)))))

;;;Frammento della concatenazione (caso c)
(defun compile-concat (args)
  (multiple-value-bind (s f d) (compile-frag (car args))
    (dolist (r (cdr args))
      (multiple-value-bind (s2 f2 d2) (compile-frag r)
        (setf d (append d2 (list (list f epsilon-label s2)) d))
        (setf f f2)))
    (values s f d)))

;;;Frammento della concatenazione (caso a)
(defun compile-alt (args)
  (let ((s (fresh-state))
        (f (fresh-state))
        (d nil))
    (dolist (r args)
      (multiple-value-bind (sr fr dr) (compile-frag r)
        (setf d (append dr
                        (list (list s  epsilon-label sr)
                              (list fr epsilon-label f))
                        d))))
    (values s f d)))


;;;Verifica che FA abbia la struttura di un NFSA creato da nfsa-compile-regex.
(defun nfsa-ok-p (fa)
  (and (nfsa-p fa)
       (symbolp (nfsa-init fa))
       (proper-list-p (nfsa-finals fa))
       (every #'symbolp (nfsa-finals fa))
       (proper-list-p (nfsa-delta fa))
       (every (lambda (tr)
                (and (proper-list-p tr)
                     (= (length tr) 3)
                     (symbolp (first tr))
                     (symbolp (third tr))))
              (nfsa-delta fa))))


;;;Calcola gli stati raggiungibili da STATES leggendo SYM (no epsilon).
(defun move (delta states sym)
  ;; Stati confrontati con EQ (gensym), simboli input con EQUAL (S-exp).
  (let ((state-set (make-hash-table :test #'eq))
        (tos nil))
    (dolist (s states)
      (setf (gethash s state-set) t))
    (dolist (tr delta)
      (destructuring-bind (from label to) tr
        (when (and (gethash from state-set)
                   (equal label sym))
          (push to tos))))
    (remove-duplicates tos :test #'eq)))

;;;Calcola la epsilon-closure a partire da SEEDS.
(defun epsilon-closure (delta seeds)
  (let ((visited (make-hash-table :test #'eq))
        (queue nil)
        (out nil))
    (dolist (s seeds)
      (unless (gethash s visited)
        (setf (gethash s visited) t)
        (push s queue)))
    (loop while queue do
      (let ((s (pop queue)))
        (push s out)
        (dolist (tr delta)
          (destructuring-bind (from label to) tr
            (when (and (eq from s)
                       (eq label epsilon-label)
                       (not (gethash to visited)))
              (setf (gethash to visited) t)
              (push to queue))))))
    out))

;;;has-final-state-p
;;;True se STATES contiene almeno uno stato finale.
(defun has-final-state-p (states finals)
  (some (lambda (s) (member s finals :test #'eq)) states))


;;;Funzioni dell'interfaccia

;;;is-regex
;;;verifica se l'argomento è una regex
(defun is-regex (re)
  (cond
    ((atom re) t)
    ((consp re)
     (let ((op (car re)))
       (cond
	 ((eq op 'z)
	  (and (proper-list-p re)
	       (consp (cdr re))
	       (null (cddr re))
	       (is-regex (cadr re))))
	 ((eq op 'o)
	  (and (proper-list-p re)
	       (consp (cdr re))
	       (null (cddr re))
	       (is-regex (cadr re))))
	 ((eq op 'c)
          (and (proper-list-p re)
               (consp (cdr re))
	       (every #'is-regex (cdr re))))
	 ((eq op 'a)
          (and (proper-list-p re)
               (consp (cdr re))
               (every #'is-regex (cdr re))))
	 ((reserved-regex-op-p op) nil)
	 (t t))))
    (t nil)))
    

;;;nfsa-compile-regex
;;;restituisce l'automa corrispondente alla regex fornita
(defun nfsa-compile-regex (re)
  (if (is-regex re)
      (multiple-value-bind (s f d) (compile-frag re)
        (make-nfsa :init s
                  :finals (list f)
                  :delta d))
      nil))


;;; nfsa-recognize
;;; Ritorna T se FA riconosce INPUT, NIL altrimenti.
;;; Se FA non e' un automa valido, segnala un errore.
(defun nfsa-recognize (fa input)
  (unless (nfsa-ok-p fa)
    (error "~S is not a Finite State Automata." fa))
  (unless (proper-list-p input)
    (return-from nfsa-recognize nil))
  (let* ((delta (nfsa-delta fa))
         (finals (nfsa-finals fa))
         (curr (epsilon-closure delta (list (nfsa-init fa)))))
    (dolist (sym input)
      (setf curr (epsilon-closure delta (move delta curr sym)))
      (when (null curr)
        (return-from nfsa-recognize nil)))

    (and (has-final-state-p curr finals) t)))


