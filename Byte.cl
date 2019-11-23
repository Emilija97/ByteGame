(defun drawTable() 
;;  (print state)
 (initializeTable))
(defun initializeTable() 
    (let* ((state '())
    (sizeOfTable (progn (print "Do you want to play with table 8 or 10 : ") (read)))
    ;; (print sizeOfTable)
    (modeGame (progn (print "Do you want to play human vs human or human vs computer, choose h or c: ") (read)))
    ;; (print modeGame)
    (state (chooseTable sizeOfTable))
    (result '("X" "X" "O"))
    )
    (draw sizeOfTable state)
    ;; (print (checkEnd sizeOfTable result))
    ;; (print (countStack '("X" "O" "." "." "." "." "." "." ".")))
    ;; (print (findNode '(A 1) state))
    ;; (print (checkIfNodeExists '(A 1) '((B 2) (C 3))))
    ;; (print (checkIfNeighboursHaveStack '((B 2)) state))
    ;; (print (checkIfNeighbours '(A 1) '(B 2) state))
    ;;(print (breadthFirst state '((B 2)) '()))
    (setq state (makeMove state '(B 2) '(C 1) '0))
	(draw sizeOfTable state)
	(setq state (makeMove state '(C 1) '(D 2) '0))
	(draw sizeOfTable state)
    )
)
;; Funkcija koja iscrtava tablu u zavisnosti od prosledjene velicine
(defun draw(sizeOfTable state)
    (if (equal sizeOfTable '8) (drawEightFields state) (drawTenFields state))
)
;; Inicijalizacija koja odredjuje koja ce tabela biti kreirana 8x8 ili 10x10
(defun chooseTable(sizeOfTable)
    (if (equal sizeOfTable '8) (fillEight) (fillTen))
)
;; Inicijalno popunjavanje table velicine 8 i 10 posebno
(defun fillEight()
    (let ((state '()))
    (progn 
        (push '((A 1) ((B 2)) ("." "." "." "." "." "." "." "." ".")) state)
  	    (push '((A 3) ((B 2) (B 4)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)))
  	    (push '((A 5) ((B 4) (B 6)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)))
  	    (push '((A 7) ((B 6) (B 8)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)))
        ;
        (push '((B 2) ((A 1) (A 3) (C 1) (C 3)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)))
        (push '((B 4) ((A 3) (A 5) (C 3) (C 5)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((B 6) ((A 5) (A 7) (C 5) (C 7)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((B 8) ((A 7) (C 7)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((C 1) ((B 2) (D 2)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((C 3) ((B 2) (D 2) (B 4) (D 4)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((C 5) ((B 4) (D 4) (B 6) (D 6)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((C 7) ((B 6) (D 6) (B 8) (D 8) ) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((D 2) ((C 1) (C 3) (E 1) (E 3)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((D 4) ((C 3) (C 5) (E 3) (E 5)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((D 6) ((C 5) (C 7) (E 5) (E 7)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((D 8) ((C 7) (E 7)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((E 1) ((D 2) (F 2)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((E 3) ((D 2) (F 2) (D 4) (F 4)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((E 5) ((D 4) (F 4) (D 6) (F 6)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((E 7) ((D 6) (F 6) (D 8) (F 8) ) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((F 2) ((E 1) (E 3) (G 1) (G 3)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((F 4) ((E 3) (E 5) (G 3) (G 5)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((F 6) ((E 5) (E 7) (G 5) (G 7)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((F 8) ((E 7) (G 7)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((G 1) ((F 2) (H 2)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((G 3) ((F 2) (H 2) (F 4) (H 4)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((G 5) ((F 4) (H 4) (F 6) (H 6)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((G 7) ((F 6) (H 6) (F 8) (H 8) ) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((H 2) ((G 1) (G 3)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((H 4) ((G 3) (G 5)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((H 6) ((G 5) (G 7)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((H 8) ((G 7)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)))
    )
    state    
    )
)
(defun fillTen()
    (let ((state) '())
    (progn 
        (push '((A 1) ((B 2)) ("." "." "." "." "." "." "." "." ".")) state))
        (push '((A 3) ((B 2) (B 4)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)))
        (push '((A 5) ((B 4) (B 6)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)))
        (push '((A 7) ((B 6) (B 8)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)))
        (push '((A 9) ((B 8) (B 10)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)))
        ;
        (push '((B 2) ((A 1) (A 3) (C 1) (C 3)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((B 4) ((A 3) (A 5) (C 3) (C 5)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((B 6) ((A 5) (A 7) (C 5) (C 7)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((B 8) ((A 7) (A 9) (C 7) (C 9)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((B 10) ((A 9) (C 9)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((C 1) ((B 2) (D 2)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((C 3) ((B 2) (D 2) (B 4) (D 4)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((C 5) ((B 4) (D 4) (B 6) (D 6)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((C 7) ((B 6) (D 6) (B 8) (D 8) ) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((C 9) ((B 8) (D 8) (B 10) (D 10) ) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((D 2) ((C 1) (C 3) (E 1) (E 3)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((D 4) ((C 3) (C 5) (E 3) (E 5)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((D 6) ((C 5) (C 7) (E 5) (E 7)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((D 8) ((C 7) (C 9) (E 7) (E 9)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((D 10) ((C 9) (E 9)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((E 1) ((D 2) (F 2)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((E 3) ((D 2) (F 2) (D 4) (F 4)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((E 5) ((D 4) (F 4) (D 6) (F 6)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((E 7) ((D 6) (F 6) (D 8) (F 8) ) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((E 9) ((D 8) (F 8) (D 10) (F 10) ) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((F 2) ((E 1) (E 3) (G 1) (G 3)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((F 4) ((E 3) (E 5) (G 3) (G 5)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((F 6) ((E 5) (E 7) (G 5) (G 7)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((F 8) ((E 7) (E 9) (G 7) (G 9)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((F 10) ((E 9) (G 9)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((G 1) ((F 2) (H 2)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((G 3) ((F 2) (H 2) (F 4) (H 4)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((G 5) ((F 4) (H 4) (F 6) (H 6)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((G 7) ((F 6) (H 6) (F 8) (H 8) ) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((G 9) ((F 8) (H 8) (F 10) (H 10) ) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((H 2) ((I 1) (I 3) (G 1) (G 3)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((H 4) ((I 3) (I 5) (G 3) (G 5)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((H 6) ((I 5) (I 7) (G 5) (G 7)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((H 8) ((I 7) (I 9) (G 7) (G 9)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((H 10) ((I 9) (G 9)) ("X" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((I 1) ((H 2) (J 2)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((I 3) ((H 2) (J 2) (H 4) (J 4)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((I 5) ((H 4) (J 4) (H 6) (J 6)) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((I 7) ((H 6) (J 6) (H 8) (J 8) ) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((I 9) ((H 8) (J 8) (H 10) (J 10) ) ("O" "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        ;
        (push '((J 2) ((I 1) (I 3)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((J 4) ((I 3) (I 5)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((J 6) ((I 5) (I 7)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((J 8) ((I 7) (I 9)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)) )
        (push '((J 10) ((I 9)) ("." "." "." "." "." "." "." "." ".")) (cdr (last state)) )
    state
    )
    )
(defun drawEightFields(state) 
    (progn 
    (print "8")
	(format t "~%    1    2    3    4    5    6    7    8")
    (drawEightTable state '8)
	)
)
;; Kreiranje asocijativne liste koja ima prvi element kao key slovo, tj.oznaku vrste, a drugi je vrednost od koje pocinju elementi kolona u state
(defun drawEightTable(state val)
    (let ((alista '((A 0) (B 4) (C 8) (D 12) (E 16) (F 20) (G 24) (H 28))))
        (drawFields alista '0 '8 state val)
    )
)
;; Iscrtava polja, i to za svaku vrstu
(defun drawFields(alista num size state val) 
    (cond ((equal val '8)
        (cond ((null alista) '())
        (t (progn (drawRow (caar alista) (cadar alista) num size state) (drawFields (cdr alista) (1+ num) size state val)))
    ))
    (t(progn (cond ((null alista) '())
        (t (progn (drawTenRow (caar alista) (cadar alista) num size state) (drawFields (cdr alista) (1+ num) size state val)))
    )
    ))
    )
)
(defun fillRow(startPosition size state formatText mode letter)
    (if (equal mode '2)(format t formatText
            (nth size (caddr (nth startPosition state))) (nth (- size 1) (caddr (nth startPosition state))) (nth (- size 2) (caddr (nth startPosition state))) 
            (nth size (caddr (nth (+ 1 startPosition) state))) (nth (- size 1) (caddr (nth (+ 1 startPosition) state))) (nth (- size 2) (caddr (nth (+ 1 startPosition) state))) 
            (nth size (caddr (nth (+ 2 startPosition) state))) (nth (- size 1) (caddr (nth (+ 2 startPosition) state))) (nth (- size 2) (caddr (nth (+ 2 startPosition) state))) 
            (nth size (caddr (nth (+ 3 startPosition) state))) (nth (- size 1) (caddr (nth (+ 3 startPosition) state))) (nth (- size 2) (caddr (nth (+ 3 startPosition) state))) 
    )
    (format t formatText letter
            (nth size (caddr (nth startPosition state))) (nth (- size 1) (caddr (nth startPosition state))) (nth (- size 2) (caddr (nth startPosition state))) 
            (nth size (caddr (nth (+ 1 startPosition) state))) (nth (- size 1) (caddr (nth (+ 1 startPosition) state))) (nth (- size 2) (caddr (nth (+ 1 startPosition) state))) 
            (nth size (caddr (nth (+ 2 startPosition) state))) (nth (- size 1) (caddr (nth (+ 2 startPosition) state))) (nth (- size 2) (caddr (nth (+ 2 startPosition) state))) 
            (nth size (caddr (nth (+ 3 startPosition) state))) (nth (- size 1) (caddr (nth (+ 3 startPosition) state))) (nth (- size 2) (caddr (nth (+ 3 startPosition) state))) 
    ))
)
(defun drawRow(letter startPosition type size state)
    (cond ((<= (mod type 2) '0) 
       (progn 
		(fillRow startPosition size state "~%   ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
        (fillRow startPosition (- size 3) state "~%~a  ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 1 letter)
	    (fillRow startPosition (- size 6) state"~%   ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
       ))
       (t (progn
          (fillRow startPosition size state "~%        ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
		  (fillRow startPosition (- size 3) state "~%~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 1 letter)
          (fillRow startPosition (- size 6) state "~%        ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)  
       ))
    )
)
(defun drawTenFields(state)
    (progn
        (format t "~%    1    2    3    4    5    6    7    8    9    10")
        (drawTenTable state '10)
    )
)
(defun drawTenTable(state val)
    (let ((alista '((A 0) (B 5) (C 10) (D 15) (E 20) (F 25) (G 30) (H 35) (I 40) (J 45))))
        (drawFields alista '0 '8 state val)
    )
)
(defun drawTenRow(letter startPosition type size state)
    (cond ((<= (mod type 2) '0) 
       (progn 
		(fillTenRow startPosition size state "~%   ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
        (fillTenRow startPosition (- size 3) state "~%~a  ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 1 letter)
	    (fillTenRow startPosition (- size 6) state"~%   ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
       ))
       (t (progn
          (fillTenRow startPosition size state "~%        ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
		  (fillTenRow startPosition (- size 3) state "~%~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 1 letter)
          (fillTenRow startPosition (- size 6) state "~%        ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)  
       ))
    )
)
(defun fillTenRow(startPosition size state formatText mode letter)
    (if (equal mode '2)(format t formatText
            (nth size (caddr (nth startPosition state))) (nth (- size 1) (caddr (nth startPosition state))) (nth (- size 2) (caddr (nth startPosition state))) 
            (nth size (caddr (nth (+ 1 startPosition) state))) (nth (- size 1) (caddr (nth (+ 1 startPosition) state))) (nth (- size 2) (caddr (nth (+ 1 startPosition) state))) 
            (nth size (caddr (nth (+ 2 startPosition) state))) (nth (- size 1) (caddr (nth (+ 2 startPosition) state))) (nth (- size 2) (caddr (nth (+ 2 startPosition) state))) 
            (nth size (caddr (nth (+ 3 startPosition) state))) (nth (- size 1) (caddr (nth (+ 3 startPosition) state))) (nth (- size 2) (caddr (nth (+ 3 startPosition) state)))
            (nth size (caddr (nth (+ 4 startPosition) state))) (nth (- size 1) (caddr (nth (+ 4 startPosition) state))) (nth (- size 2) (caddr (nth (+ 4 startPosition) state))) 
    )
    (format t formatText letter
            (nth size (caddr (nth startPosition state))) (nth (- size 1) (caddr (nth startPosition state))) (nth (- size 2) (caddr (nth startPosition state))) 
            (nth size (caddr (nth (+ 1 startPosition) state))) (nth (- size 1) (caddr (nth (+ 1 startPosition) state))) (nth (- size 2) (caddr (nth (+ 1 startPosition) state))) 
            (nth size (caddr (nth (+ 2 startPosition) state))) (nth (- size 1) (caddr (nth (+ 2 startPosition) state))) (nth (- size 2) (caddr (nth (+ 2 startPosition) state))) 
            (nth size (caddr (nth (+ 3 startPosition) state))) (nth (- size 1) (caddr (nth (+ 3 startPosition) state))) (nth (- size 2) (caddr (nth (+ 3 startPosition) state)))
            (nth size (caddr (nth (+ 4 startPosition) state))) (nth (- size 1) (caddr (nth (+ 4 startPosition) state))) (nth (- size 2) (caddr (nth (+ 4 startPosition) state)))  
    ))
)
;; Proverava da li je kraj, ako je popunjeno 2/3 ili 3/5 ili ako je do kraja napunjen, ne vraca
(defun checkEnd (num result) 
  (cond ((equal num 8) (if (equal (length result) 3) t 
                         (cond ((equal (countO result) 2) t ) 
                               ((equal (countX result) 2) t ) 
                               (t nil ))))
        ((equal num 10) (if (equal (length result) 5) t
                          (cond ((equal (countO result) 3) t ) 
                               ((equal (countX result) 3) t ) 
                               (t nil ))))
    ))
;; Racuna broj O u steku ili X
(defun countO (result) (if (null result) '0 
                           (if (equal (car result) "O") (+ 1 (countO (cdr result))) 
                             (countO (cdr result)))))

(defun countX (result) (if (null result) '0 
                           (if (equal (car result) "X") (+ 1 (countX (cdr result))) 
                             (countX (cdr result)))))
;;Izracunava broj elemenata u steku 
(defun countStack(stack) (if (null stack) '0 
                             (+ (countO stack) (countX stack))
                        )
)
;; Pronalazi trazeni cvor u grafu i vraca ga u obliku npr. ((A 1) ((B 2)) ("." "." "." "." "." "." "." "." "."))
(defun findNode (node state) (if (null state) '() 
                                (if (equal (caar state) node) (car state) (findNode node (cdr state)))))
;; Proverava da li neki cvor postoji u prosledjenoj listi
(defun checkIfNodeExists(node lista) 
    (cond ((null lista) '())
            ((equal (car lista) node) t)
            (t(checkIfNodeExists node (cdr lista)))
    )
)
;; Proverava susedne cvorove da li poseduju neki element/stek na njima
(defun checkIfNeighboursHaveStack(neighbours state)
    (cond ((null neighbours) '())
            ((> (countStack (caddr (findNode (car neighbours) state))) 0) t)
            (t (checkIfNeighboursHaveStack (cdr neighbours) state))
    )
)
;; Proverava da li su dva cvora susedi
(defun checkIfNeighbours(node1 node2 state)
    (if (findInNeighbours node2 (cadr (findNode node1 state))) t nil)
)
;; Proverava da li se cvor nalazi u prosledjenoj listi suseda
(defun findInNeighbours(node listn)
    (cond ((null listn) '())
            ((equal node (car listn)) t)
            (t (findInNeighbours node (cdr listn)))
    )
)
;; Funkcija koja smesta nove cvorove potomke, koji se vec ne nalaze u listi obradjenih i vraca iste
;; razlika skupova
(defun new-nodes(descendants nodes)
    (cond ((null descendants) '())
            (t (if (processNode (car descendants) nodes) 
                (cons (car descendants) (new-nodes (cdr descendants) nodes))
                (new-nodes (cdr descendants) nodes)
                )
            )
    )
)
;; Funkcija koja proverava da li je neki cvor clan obradjenih cvorova, tj.da li se potomak vec nalazi u listi cvorova
;; ako se ne nalazi, vraca t, ako je obradjen pre vraca nil
(defun processNode(node listOfNodes)
;;ako smo stigli do kraja liste znaci da taj cvor nije obradjen pre
    (cond ((null listOfNodes) t)
        ((equal node (car listOfNodes)) nil)
        (t (processNode node (cdr listOfNodes)))
    )
)
;; graph je state, trazenje po sirini sve dok ne nadje cvor koji ima stek
(defun breadthFirst(graph startNode processed)
    (cond ((null startNode) '())
        ((> (countStack (caddr (findNode (car startNode) graph))) 0) (list (car startNode)))
        (t (let* (
                    (newProcessed (createProcessed processed startNode))
                    (descendants (createDescendants startNode newProcessed graph))
                    (newStartNode (append (cdr startNode) descendants))
                    (path (breadthFirst graph newStartNode newProcessed)) ;;kreiramo putanju u kojoj ce biti svi cvorovi kroz koje se proslo
        )
            (cond ((null path) '())
                ((checkIfNodeExists (car path) descendants) (cons (car startNode) path))
                (t path)
            )
        ))
    )
)
;; Kreira listu obradjenih cvorova
(defun createProcessed(processed lista)
    (append processed (list (car lista)))
)
;; Kreira listu suseda
(defun createDescendants(lista processed graph)
    (new-nodes (cadr (findNode (car lista) graph)) (append (cadr lista) processed))
)
;; Funkcija koja za svoje susede poziva breadthFirst i pravi listu puteva gde moze da se ide
(defun findPathInNeighbours(state node)
	(getPaths state (cadr (findNode node state)) node)
)
;; Za sve prosledjene cvorove poziva breadthFirst
(defun getPaths(state nodes node)
	(cond 
		((null nodes) '())
		(t (cons (breadthFirst state (list (car nodes)) (list node)) (getPaths state (cdr nodes) node) ))
	)
)
;; Nalazi element sa najmanjom velicinom
(defun findMinLength(lista minLength)
	(cond 
		((null lista) minLength)
		((null (car lista)) (findMinLength (cdr lista) minLength) )
		((> minLength (length (car lista))) (findMinLength (cdr lista) (length (car lista))))
		(t (findMinLength (cdr lista) minLength))
	)
)
;; Vraca listu elemenata sa najmanjom duzinom do tog trenutka
;; (returningMinPath lista (findMinLength lista '100))
(defun returningMinPath(listNearby minLength)
	(cond 
		((null listNearby) '())
		((null (car listNearby)) (returningMinPath (cdr listNearby) minLength))
		((> (length (car listNearby)) minLength) (returningMinPath (cdr listNearby) minLength))
		((< (length (car listNearby)) minLength) (cons (car listNearby) (returningMinPath (cdr listNearby) (length (car listNearby))) ))
		(t (cons (car listNearby) (returningMinPath (cdr listNearby) minLength))) 
	)
)
;; Poziva funkciju koja proverava da li se end cvor nalazi u listi validnih koja se kreira u ovoj fji
(defun checkIfExists(state start end)
	(let* 
		((lista (findPathInNeighbours state start)) 
		(validList (returningMinPath lista (findMinLength lista '100)))
		)
		(checkNodeInValidList end validList)
	)
)
;; Proverava da li je end cvor u listi validnih
(defun checkNodeInValidList(node list)
	(cond 
		((null list) nil)
		((equal node (caar list)) t)
		(t (checkNodeInValidList node (cdr list)))
	)
)
;; Proverava da li je potez validan
(defun validateMove (state start endPoint stackHeight) 
	;; da li su susedi
	(if (checkIfNeighbours start endPoint state) 
		;; ako jesu susedi da li susedi imaju stack
		(if (checkIfNeighboursHaveStack (cadr (findNode start state)) state )
			;; susedi su i susedi imaju stack
			;; proverava da li je visina sa koje se pomera jednaka 0 jer u tom slucaju ne delimo stack
			(if (equal stackHeight '0)
				;; ako je 0 onda rezultujuci stack mora da ima vrednost manju od 0
				(if (> (+ (- (countStack (caddar (findNode start state))) stackHeight)
		                    (countStack (caddar (findNode endPoint state)))) '8 )
		        		nil
		    			t
					)
			
				;; ako delimo stack onda proverava da li endPoint ima stack jednak 0
		    	(if (equal (countStack (caddar (findNode endPoint state))) '0 ) 
					;; ako ima vrati nil
		    	    nil
					;; da li je visina steka na koji se pomera visa od plocice sa koje se pomera
					(if (< (countStack (caddar (findNode endPoint state))) stackHeight) 
		    	    	nil
						;; rezultujuci stack mora da ima manje od 8 plocica
		    			(if (> (+ (- (countStack (caddar (findNode start state))) stackHeight)
		    	                (countStack (caddar (findNode endPoint state)))) '8 )
		    	    		nil
		    				t
						)
					)
				)
			)
			;; susedi su ali susedi nemaju stack
		    (checkIfExists state start endPoint))
		;; nisu susedi vrati nil
		nil
		)
)
;; Ukoliko je potez validan odigrava potez
(defun makeMove (state startPoint endPoint stackHeight ) 
	(cond 
		((not (validateMove state startPoint endPoint stackHeight)) nil)
		;; odigraj potez
		(t 
			(let* 
				(
					(newState (changeElement state startPoint (combineStacks '("." "." "." "." "." "." "." "." ".") (getStackTo (caddr (findNode startPoint state)) stackHeight )) ) )
					(newState2 (changeElement newState endPoint (combineStacks (caddr (findNode endPoint state)) (getStackFrom (caddr (findNode startPoint state)) stackHeight))))
				)
				newState2
			)
		)
	)
)
;; (trace validateMove)
;; Vraca stack pocevsi od elementa n pa do prve tacke
(defun getStackFrom(stack n)
	(cond 
		((null stack) '())
		((equal n '0) 
			(if (equal (car stack) ".")
				'()
				(cons (car stack) (getStackFrom (cdr stack) n))
			)
		)
		(t (getStackFrom (cdr stack) (- n 1)))
	)
)
;; Vraca stack pocevsi od 0 do n
(defun getStackTo(stack n)
	(cond 
		((<= n '0) '())
		(t (cons (car stack) (getStackTo (cdr stack) (- n 1))))
	)
)
;; Na stack1 dodaje stack2
(defun combineStacks(stack1 stack2)
	(cond 
		((null stack2) stack1)
		((equal (car stack1) ".") (cons (car stack2) (combineStacks (cdr stack1) (cdr stack2))))
		(t (cons (car stack1) (combineStacks (cdr stack1) stack2)))
	)
)
;; Menja stack prosledjenom elementu
(defun changeElement(state elem stack)
	(cond 
		((null state) '())
		((equal (caar state) elem) (cons (append (append (list (caar state)) (list (cadar state))) (list stack)) (cdr state) ))
		(t (cons (car state) (changeElement (cdr state) elem stack)))
	)
)
;; (trace breadthFirst)
(drawTable)