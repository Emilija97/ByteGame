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
    )
    (if (equal sizeOfTable '8) (drawEightFields state) )
    (print state)
    )
)

(defun chooseTable(sizeOfTable)
    (if (equal sizeOfTable '8) (fillEight) (fillTen))
)

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
    (drawAllEightTable state)
	;; (drawOdd 8 0 "A" state )
	;; ;
	;; (drawEven 8 4 "B" state)
	;; ;
  	;; (drawOdd 8 8 "C" state)
  	;; ;
  	;; (drawEven 8 12 "D" state)
	;; ;
  	;; (drawOdd 8 16 "E" state)
  	;; ;
  	;; (drawEven 8 20 "F" state)
	;; ;
  	;; (drawOdd 8 24 "G" state)
  	;; ;
  	;; (drawEven 8 28 "H" state)
	)
)

(trace drawEightFields)

(defun drawAllEightTable(state)
    (let ((alista '((A 0) (B 4) (C 8) (D 12) (E 16) (F 20) (G 24) (H 28))))
        (drawFields alista '0 '8 state)
    )
)

(defun drawFields(alista num size state) 
    (cond ((null alista) '())
        (t (progn (drawRow (caar alista) (cadar alista) num size state) (drawFields (cdr alista) (1+ num) size state)))
    )
)

(defun drawRow2(startPosition size state str pom letter)
(if (equal pom '2)(format t str
        (nth size (caddr (nth startPosition state))) (nth (- size 1) (caddr (nth startPosition state))) (nth (- size 2) (caddr (nth startPosition state))) 
		(nth size (caddr (nth (+ 1 startPosition) state))) (nth (- size 1) (caddr (nth (+ 1 startPosition) state))) (nth (- size 2) (caddr (nth (+ 1 startPosition) state))) 
		(nth size (caddr (nth (+ 2 startPosition) state))) (nth (- size 1) (caddr (nth (+ 2 startPosition) state))) (nth (- size 2) (caddr (nth (+ 2 startPosition) state))) 
		(nth size (caddr (nth (+ 3 startPosition) state))) (nth (- size 1) (caddr (nth (+ 3 startPosition) state))) (nth (- size 2) (caddr (nth (+ 3 startPosition) state))) 
)
(format t str letter
        (nth size (caddr (nth startPosition state))) (nth (- size 1) (caddr (nth startPosition state))) (nth (- size 2) (caddr (nth startPosition state))) 
		(nth size (caddr (nth (+ 1 startPosition) state))) (nth (- size 1) (caddr (nth (+ 1 startPosition) state))) (nth (- size 2) (caddr (nth (+ 1 startPosition) state))) 
		(nth size (caddr (nth (+ 2 startPosition) state))) (nth (- size 1) (caddr (nth (+ 2 startPosition) state))) (nth (- size 2) (caddr (nth (+ 2 startPosition) state))) 
		(nth size (caddr (nth (+ 3 startPosition) state))) (nth (- size 1) (caddr (nth (+ 3 startPosition) state))) (nth (- size 2) (caddr (nth (+ 3 startPosition) state))) 
))
)

(defun drawRow(letter startPosition type size state)
    (cond ((<= (mod type 2) '0) 
       (progn 
		(drawRow2 startPosition size state "~%   ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
        (drawRow2 startPosition (- size 3) state "~%~a  ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 1 letter)
	    (drawRow2 startPosition (- size 6) state"~%   ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
       ))
       (t (progn
          (drawRow2 startPosition size state "~%        ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
		  (drawRow2 startPosition (- size 3) state "~%~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 1 letter)
          (drawRow2 startPosition (- size 6) state "~%        ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)  
       ))
    )
)
;; (defun drawOdd(size startPosition letter state)
;;     (format t "~%   ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 
;; 		(nth size (caddr (nth startPosition state))) (nth (- size 1) (caddr (nth startPosition state))) (nth (- size 2) (caddr (nth startPosition state))) 
;; 		(nth size (caddr (nth (+ 1 startPosition) state))) (nth (- size 1) (caddr (nth (+ 1 startPosition) state))) (nth (- size 2) (caddr (nth (+ 1 startPosition) state))) 
;; 		(nth size (caddr (nth (+ 2 startPosition) state))) (nth (- size 1) (caddr (nth (+ 2 startPosition) state))) (nth (- size 2) (caddr (nth (+ 2 startPosition) state))) 
;; 		(nth size (caddr (nth (+ 3 startPosition) state))) (nth (- size 1) (caddr (nth (+ 3 startPosition) state))) (nth (- size 2) (caddr (nth (+ 3 startPosition) state))) 
;; 		)
;; 	(format t "~%~a  ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" letter
;; 		(nth (- size 3) (caddr (nth startPosition state))) (nth (- size 4) (caddr (nth startPosition state))) (nth (- size 5) (caddr (nth startPosition state))) 
;; 		(nth (- size 3) (caddr (nth (+ 1 startPosition) state))) (nth (- size 4) (caddr (nth (+ 1 startPosition) state))) (nth (- size 5) (caddr (nth (+ 1 startPosition) state))) 
;; 		(nth (- size 3) (caddr (nth (+ 2 startPosition) state))) (nth (- size 4) (caddr (nth (+ 2 startPosition) state))) (nth (- size 5) (caddr (nth (+ 2 startPosition) state))) 
;; 		(nth (- size 3) (caddr (nth (+ 3 startPosition) state))) (nth (- size 4) (caddr (nth (+ 3 startPosition) state))) (nth (- size 5) (caddr (nth (+ 3 startPosition) state))) 
;; 		)
;; 	(format t "~%   ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 
;; 		(nth (- size 6) (caddr (nth startPosition state))) (nth (- size 7) (caddr (nth startPosition state))) (nth (- size 8) (caddr (nth startPosition state))) 
;; 		(nth (- size 6) (caddr (nth (+ 1 startPosition) state))) (nth (- size 7) (caddr (nth (+ 1 startPosition) state))) (nth (- size 8) (caddr (nth (+ 1 startPosition) state))) 
;; 		(nth (- size 6) (caddr (nth (+ 2 startPosition) state))) (nth (- size 7) (caddr (nth (+ 2 startPosition) state))) (nth (- size 8) (caddr (nth (+ 2 startPosition) state))) 
;; 		(nth (- size 6) (caddr (nth (+ 3 startPosition) state))) (nth (- size 7) (caddr (nth (+ 3 startPosition) state))) (nth (- size 8) (caddr (nth (+ 3 startPosition) state))) 
;; 		)
;; )

;; (defun drawEven(size startPosition letter state)
;;      (format t "~%        ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 
;; 		(nth size (caddr (nth startPosition state))) (nth (- size 1) (caddr (nth startPosition state))) (nth (- size 2) (caddr (nth startPosition state))) 
;; 		(nth size (caddr (nth (+ 1 startPosition) state))) (nth (- size 1) (caddr (nth (+ 1 startPosition) state))) (nth (- size 2) (caddr (nth (+ 1 startPosition) state))) 
;; 		(nth size (caddr (nth (+ 2 startPosition) state))) (nth (- size 1) (caddr (nth (+ 2 startPosition) state))) (nth (- size 2) (caddr (nth (+ 2 startPosition) state))) 
;; 		(nth size (caddr (nth (+ 3 startPosition) state))) (nth (- size 1) (caddr (nth (+ 3 startPosition) state))) (nth (- size 2) (caddr (nth (+ 3 startPosition) state))) 
;; 		)
;;     (format t "~%~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a"  letter
;; 		(nth (- size 3) (caddr (nth startPosition state))) (nth (- size 4) (caddr (nth startPosition state))) (nth (- size 5) (caddr (nth startPosition state))) 
;; 		(nth (- size 3) (caddr (nth (+ 1 startPosition) state))) (nth (- size 4) (caddr (nth (+ 1 startPosition) state))) (nth (- size 5) (caddr (nth (+ 1 startPosition) state))) 
;; 		(nth (- size 3) (caddr (nth (+ 2 startPosition) state))) (nth (- size 4) (caddr (nth (+ 2 startPosition) state))) (nth (- size 5) (caddr (nth (+ 2 startPosition) state))) 
;; 		(nth (- size 3) (caddr (nth (+ 3 startPosition) state))) (nth (- size 4) (caddr (nth (+ 3 startPosition) state))) (nth (- size 5) (caddr (nth (+ 3 startPosition) state))) 
;; 		)
;; 	(format t "~%        ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 
;; 		(nth (- size 6) (caddr (nth startPosition state))) (nth (- size 7) (caddr (nth startPosition state))) (nth (- size 8) (caddr (nth startPosition state))) 
;; 		(nth (- size 6) (caddr (nth (+ 1 startPosition) state))) (nth (- size 7) (caddr (nth (+ 1 startPosition) state))) (nth (- size 8) (caddr (nth (+ 1 startPosition) state))) 
;; 		(nth (- size 6) (caddr (nth (+ 2 startPosition) state))) (nth (- size 7) (caddr (nth (+ 2 startPosition) state))) (nth (- size 8) (caddr (nth (+ 2 startPosition) state))) 
;; 		(nth (- size 6) (caddr (nth (+ 3 startPosition) state))) (nth (- size 7) (caddr (nth (+ 3 startPosition) state))) (nth (- size 8) (caddr (nth (+ 3 startPosition) state))) 
;; 		)
;; )

(defun drawTenFields(state)
    (print "Usao sam u 10"))

(drawTable)