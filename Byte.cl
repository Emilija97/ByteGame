(defun drawTable()
;;  (print state)
 (initializeTable))
(defun initializeTable() 
    (let* ((state '())
    (sizeOfTable (progn (print "Do you want to play with table 8 or 10: ") (read)))
    ;; (print sizeOfTable)
    (modeGame (progn (print "Who do you want to play first, choose h or c: ") (read)))
    ;; (print modeGame)
    (state (chooseTable sizeOfTable))
    )
    (playGame state sizeOfTable modeGame)
    ;; (print state)
    ;; (draw sizeOfTable state)
    ;; (setq bott T)
    ;; (setq BW T)
    ;; (setq result '())
    ;; (print result)
    ;; (print (checkEnd sizeOfTable result))
    ;; (print (minimax state 1 alpha beta BW sizeOfTable '(".")))
    ;; (print (findAppropriateNodes state BW))
    ;; (setq BW (not BW))
    ;; (print (findAppropriateNodes state BW))
    ;; (setq BW (not BW))
    ;; (print (evaluateState state BW))
    ;;(print (makeAllMoves state))
    ;; (setq nodes (findNeighbours '(B 2) state))
    ;; (print (makeMove state '(B 2)  'findAppropriateNodes () node) '0))
    ;; (print (findNeighbours '(B 2) state))
    ;; (loop 
    ;;     for x in (findAppropriateNodes state BW)
    ;;     append (loop for y in (findNeighbours x state)
    ;;      do (print y))
    ;;         ;;(print (findNeighbours x state))
    ;;         ;; (let ((endNodes (findNeighbours x state))
    ;;         ;;     ;; (loop for e in endNodes 
    ;;         ;;     ;;     do (print e)
    ;;         ;;     ;; )
    ;;         ;;     (print endNodes)
    ;;         ;;     )
    ;;         ;; )
    ;; )
    ;; (print (loop for x downfrom 999 to 900
    ;;   append (loop for y downfrom 999 to 900
    ;;                collect (* x y))))
    ;; (print (findPathInNeighbours state '(B 2)))
    ;; (print (makeAllMoves state))
    ;; (setq firstPlayerTurn '1)
    ;; B-black/W-white turn
    ;;(setq BW T)
    ;; (print (checkIfStackStartsWith (caddr (findNode '(D 2) state)) 2))
    ;; (print (checkIfStackStartsWith (caddr (findNode '(D 2) state)) 1))
    ;; (print (checkEnd sizeOfTable result))
    ;; (print (countStack '("X" "O" "." "." "." "." "." "." ".")))
    ;; (print (findNode '(A 1) state))
    ;; (print (checkIfNodeExists '(A 1) '((B 2) (C 3))))
    ;; (print (checkIfNeighboursHaveStack '((B 2)) state))
    ;; (print (checkIfNeighbours '(A 1) '(B 2) state))
    ;; (print (breadthFirst state 'let*((B 2)) '()))
    ;; (print (makeAllMoves state))
    ;; (print (findNeighbours '(E 5) state))
    ;; (print (findNeighbours '(E 5) state))
    ;; (print (findAppropriateNodes state BW '()))
    ;; (print (findPathInNeighbours state '(B 4)))
    ;; (print (caar (findPathInNeighbours state '(B 4))))
    ;; (print (caadr (findPathInNeighbours state '(B 4))))
    ;; (print (caaddr (findPathInNeighbours state '(B 4))))
    ;; (print (caaddr (cdr (findPathInNeighbours state '(B 4)))))
    ;; (print (breadthFirst state '((E 5)) '()))
    ;; (print (new-states state state T))
    ;; (print (caaddr (car state)))
    ;; (loop while
    ;; (setq firstPlayerTurn '1)
    ;; (loop 
    ;;  (setq state (car (minimax state 2 alpha beta BW sizeOfTable result)))
    ;;     (setq result (checkFullStack state result))
    ;;     (setq state (clearFullStack state))
    ;;     ;; (print (makeAllMoves state))
    ;;     ;; (draw sizeOfTable state)
    ;;     ;; (setq BW (not BW))
    ;;     ;; (setq bott (not bott))
    ;;     ;; (setq state (max-value state alpha beta '2 BW sizeOfTable))
    ;;     (draw sizeOfTable state)
    ;;     (setq BW (not BW))
    ;;     (setq bott (not bott))
    ;;     (setq move (progn (print "Play the move: ") (read)))
    ;;     (setq state (makeMove state (car move) (cadr move) (caddr move) bott))
    ;;     ;; (draw sizeOfTable state)
    ;;     (setq BW (not BW))
    ;;     (setq bott (not bott))
    ;;     (setq result (checkFullStack state result))
    ;;     (setq state (clearFullStack state))
    ;;     (print result)
    ;;     ;;(print state)
    ;; )
    ;; (setq state (makeMove state '(B 2) '(C 1) '0))
    ;; (draw sizeOfTable state)
    ;; (setq state (makeMove state '(C 1) '(D 2) '0))
    ;; (setq state (makeMove state '(C 3) '(D 4) '0))
    ;; (setq state (makeMove state '(D 4) '(E 3) '0))
    ;; (setq state (makeMove state '(F 4) '(E 3) '0))
    ;; (draw sizeOfTable state)
    ;; (setq state (makeMove state '(D 2) '(E 3) '2))
    ;;  (draw sizeOfTable state)
    )
)
(defun playGame(state sizeOfTable modeGame)
    (draw sizeOfTable state)
    (setq BW T)
    (setq result '())
    (if (equal modeGame 'c)
        (setq bott T)
        (setq bott nil)
    )
    (print bott)
    (if bott
        (playBot state sizeOfTable)
    (loop
        (if (checkEnd sizeOfTable result) (print "Pobednik je: " result))
        (setq move (progn (print "Play the move: ") (read)))
        (setq state (makeMove state (car move) (cadr move) (caddr move) bott))
        (setq result (checkFullStack state result))
        (setq state (clearFullStack state))
        (draw sizeOfTable state)
        (setq BW (not BW))
        (setq bott (not bott))
        (setq state (car (minimax state 1 alpha beta BW sizeOfTable result)))
        (draw sizeOfTable state)
        (setq BW (not BW))
        (setq bott (not bott))
        (setq result (checkFullStack state result))
        (setq state (clearFullStack state))
    )
    )
)
(defun playBot(state sizeOfTable)
    (cond 
        ((checkEnd sizeOfTable result) (progn (print "Pobednik je: ") (print  result))) 
        (t (progn (setq state (car (minimax state 1 alpha beta BW sizeOfTable result)))
        (setq result (checkFullStack state result))
        (setq state (clearFullStack state))
        (draw sizeOfTable state)
        (setq BW (not BW))
        (setq state (car (minimax state 1 alpha beta BW sizeOfTable result)))
        (draw sizeOfTable state)
        (setq BW (not BW))
        (setq result (checkFullStack state result))
        (setq state (clearFullStack state))
        (playBot state sizeOfTable)
        )))
)
;; 
(defun createClearList(state BW)
    (let* ((nodes (makeAllMoves state BW)))
                (loop for s in nodes collect
                    (loop for x in s
                        if (not (null x))
                        collect x
                ))
    )
)
;; Funkcija koja iscrtava tablu u zavisnosti find prosledjene velicine
(defun draw(sizeOfTable state)
   (drawUniversalFields state sizeOfTable)
)
;; Inicijalizacija koja odredjuje koja ce tabela biti kreirana 8x8 ili 10x10
(defun chooseTable(sizeOfTable)
    ;; (if (equal sizeOfTable '8) (fillEight) (fillTen))
    (fillTable sizeOfTable 1 1)
)
;; kreiramo fju koja menja broj u slovo
(defun numberToLetter(number)
    (cond 
        ((equal 1 number) 'A)
        ((equal 2 number) 'B)
        ((equal 3 number) 'C)
        ((equal 4 number) 'D)
        ((equal 5 number) 'E)
        ((equal 6 number) 'F)
        ((equal 7 number) 'G)
        ((equal 8 number) 'H)
        ((equal 9 number) 'I)
        ((equal 10 number) 'J)
        ((equal 11 number) 'K)
        ((equal 12 number) 'L)
        ((equal 13 number) 'M)
        ((equal 14 number) 'N)
        ((equal 15 number) 'O)
        ((equal 16 number) 'P)
        ((equal 17 number) 'Q)
        ((equal 18 number) 'R)
        ((equal 19 number) 'S)
        ((equal 20 number) 'T)
    )
)
;; kreiramo pomocnu funkciju za izracunavanje cvorova suseda za zadati cvor, vraca listu, flag-kad dodje do 2 da onda dodajemo jedan vrsti
(defun createNeighbourList(n row column)
    (let* ((temp '())) 
        (progn 
            (if (and (> (1- row) 0) (> (1- column) 0)) (push (createList (1- row) (1- column)) temp))         
            (if (and (> (1- row) 0) (<= (1+ column) n)) (push (createList (1- row) (1+ column)) temp))
            (if (and (<= (1+ row) n) (> (1- column) 0)) (push (createList (1+ row) (1- column)) temp))
            (if (and (<= (1+ row) n) (<= (1+ column) n)) (push (createList (1+ row) (1+ column)) temp))
        )
    ;; lista
    ;; (print temp)
    temp
    )
)
;; (print (createNeighbourList 8 1 7))
;; pomocna funkcija za kreiranje cvora suseda
(defun createList(row column)
    (list (numberToLetter row) column)
)
;; kreira cvor koji cine polje, njegovi susedi i stanje na tabeli
(defun createNode(row column n)
    (list (list (numberToLetter row) column) (createNeighbourList n row column) (stateOfField row n))
)
;; inicijalizacija stanja
(defun stateOfField(row n)
    (cond 
        ((or (equal row 1) (equal row n)) '("." "." "." "." "." "." "." "." "."))
        ((equal (mod row 2) 0) '("X" "." "." "." "." "." "." "." "."))
        (t '("O" "." "." "." "." "." "." "." "."))
    )
)
;; saljemo inicijalno za row i column 1 da bi mogao da broji, korak je 2 za kolone
;; vodi racuna da parne vrste krecu od 2 za kolone
(defun fillTable(n row column)
    (let* ((state '()))
        (cond 
        ;;radimo za parnu vrstu
            ((> row n) '())
            ((equal (mod row 2) 0) 
                (cond 
                    ;; ((equal column 1) (fillTable n row (1+ column))) ;;jer kolona krece od 2 za parnu vrstu
                    ((<= (+ column 2) (+ n 2)) (cons (createNode row column n) (fillTable n row (+ column 2))))
                    (t (fillTable n (1+ row) 1))
                )
            )
            (t (cond
                ((<= (+ column 2) (+ n 2)) (cons (createNode row column n) (fillTable n row (+ column 2))))
                (t (fillTable n (1+ row) 2))
            )
            )
        )
    )
)
;; prikaz table uz pomoc funkcije format(omogucen odabir moda(8x8 ili 10x10)
(defun drawUniversalFields(state sizeOfTable)
    (progn
        (cond 
            ((equal sizeOfTable 8) (format t "~%    1    2    3    4    5    6    7    8"))
            ((equal sizeOfTable 10) (format t "~%    1    2    3    4    5    6    7    8    9    10"))
        )
        (drawUniversalTable state sizeOfTable)
    )
)
;; funkcija koja kreira tabelu za bilo koju unetu vrednost za n
(defun drawUniversalTable(state val)
    (cond
            ((equal val 8) (let ((alista '((A 0) (B 4) (C 8) (D 12) (E 16) (F 20) (G 24) (H 28)))) (drawFields alista '0 '8 state val)))
            ((equal val 10) (let ((alista '((A 0) (B 5) (C 10) (D 15) (E 20) (F 25) (G 30) (H 35) (I 40) (J 45)))) (drawFields alista '0 '8 state val)))
    )
)
;; Iscrtava polja, i to za svaku vrstu
(defun drawFields(alista num size state val) 
    (cond ((equal val '8)
        (cond ((null alista) '())
        (t (progn (drawEightRow (caar alista) (cadar alista) num size state) (drawFields (cdr alista) (1+ num) size state val)))
    ))
    (t(progn (cond ((null alista) '())
        (t (progn (drawTenRow (caar alista) (cadar alista) num size state) (drawFields (cdr alista) (1+ num) size state val)))
    )
    ))
    )
)
;; Iscrtava vrste za 8x8
(defun drawEightRow(letter startPosition type size state)
    (cond ((<= (mod type 2) '0) 
       (progn 
        (fillEightRow startPosition size state "~%   ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
        (fillEightRow startPosition (- size 3) state "~%~a  ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 1 letter)
        (fillEightRow startPosition (- size 6) state "~%   ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
       ))
       (t (progn
          (fillEightRow startPosition size state "~%        ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
          (fillEightRow startPosition (- size 3) state "~%~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 1 letter)
          (fillEightRow startPosition (- size 6) state "~%        ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)  
       ))
    )
)
;; Iscrtava vrste za 10x10
(defun drawTenRow(letter startPosition type size state)
    (cond ((<= (mod type 2) '0) 
       (progn 
        (fillTenRow startPosition size state "~%   ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
        (fillTenRow startPosition (- size 3) state "~%~a  ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 1 letter)
        (fillTenRow startPosition (- size 6) state "~%   ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
       ))
       (t (progn
          (fillTenRow startPosition size state "~%        ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)
          (fillTenRow startPosition (- size 3) state "~%~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 1 letter)
          (fillTenRow startPosition (- size 6) state "~%        ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a       ~a~a~a" 2 letter)  
       ))
    )
)
;; Popunjava vrste za 8x8
(defun fillEightRow(startPosition size state formatText mode letter)
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
;; Popunjava vrste za 10x10
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
  (cond ((null result) nil)
        ((equal num 8) (if (equal (length result) 3) t 
                         (cond ((equal (countO result) 2) t ) 
                               ((equal (countX result) 2) t ) 
                               (t nil ))))
        ((equal num 10) (if (equal (length result) 5) t
                          (cond ((equal (countO result) 3) t ) 
                               ((equal (countX result) 3) t ) 
                               (t nil ))))
    ))
;; Racuna broj O u steku ili X
(defun countO (stack) (if (null stack) '0 
                           (if (equal (car stack) "O") (+ 1 (countO (cdr stack))) 
                             (countO (cdr stack)))))

(defun countX (stack) (if (null stack) '0 
                           (if (equal (car stack) "X") (+ 1 (countX (cdr stack))) 
                             (countX (cdr stack)))))
;;Izracunava broj elemenata u steku 
(defun countStack(stack) (if (null stack) '0 
                             (+ (countO stack) (countX stack))
                        )
)
;; Proveravamo da li je stek dosao do visine 8 da bismo upisali u result vrednost ciji je, ko vodi ka pobedi
(defun checkFullStack(state result)
	(cond 
		((null state) result)
		((equal (countStack (caddar state)) '8)
			(append result (list  (nth '7 (caddar state))))
		)
		(t (checkFullStack (cdr state) result))
	)
)
;; Moramo da oslobodimo ona stanja gde je dosao do 8 stek, cistimo tablu
(defun clearFullStack(state)
	(cond 
		((null state) '())
		((equal (countStack (caddar state)) '8)
			(cons (append (cons (caar state) (list (cadar state))) (list '("." "." "." "." "." "." "." "." "."))) (cdr state))
		)
		(t (cons (car state) (clearFullStack (cdr state))))
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
;; Vraca sve potomke prosledjenog cvora
(defun findNeighbours (node state)
    (cond ((null node) '())
        ((equal (caar state) node)
            (cadar state)
        )
        (t (findNeighbours node (cdr state)))
    )
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
(defun checkIfEndNodeExists(state start end)
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
;; proverava ciljni cvor
(defun checkNeighbourStack(endPoint state)
    (if (> (countStack (caddr (findNode endPoint state))) 0) t nil)
)
;; Proverava da li je potez validan
(defun validateMove (state start endPoint stackHeight) 
    ;; proverava da li su cvorovi susedi
    (if (checkIfNeighbours start endPoint state) 
        ;; proverava da li susedi imaju stack
        (if (checkIfNeighboursHaveStack (cadr (findNode start state)) state )
            ;; susedi imaju stack-> ali sad nas zanima da li taj ciljni ima stek
            (if (checkNeighbourStack endPoint state)
                ;; proverava da li je visina stacka sa kojeg se pomera jednaka 0, tada se ne deli stack
                (if (equal stackHeight '0)
                    ;; ukoliko je 0 onda rezultujuci stack mora da ima vrednost manju od 0
                    (if (> (+ (- (countStack (caddr (findNode start state))) stackHeight)
                                (countStack (caddr (findNode endPoint state)))) '8 )
                                nil
                                t
                    )
                    ;; ako delimo stack onda proverava da li endPoint ima stack jednak 0
                    (if (equal (countStack (caddr (findNode endPoint state))) '0 ) 
                        ;; vraca nil ako ima
                    nil
                        ;; da li je visina steka na koji se pomera visa od plocice sa koje se pomera
                            (if (< (countStack (caddr (findNode endPoint state))) stackHeight) 
                                nil
                                ;; rezultujuci stack ne sme da ima vise od 8 plocica
                                (if (> (+ (- (countStack (caddr (findNode start state))) stackHeight)
                                        (countStack (caddr (findNode endPoint state)))) '8 )
                                          nil
                                    t
                                )
                            )
                        ;; nije ispunjen taj uslov za deobu steka
                        ;; nil
                        ;; )
                    )
                )
                ;;ukoliko ciljni cvor nema elemenata
                nil
            )
            ;; susedi su, ali susedi nemaju stack
            (checkIfEndNodeExists state start endPoint))
        ;; nisu susedi vrati nil
        nil
        )
)
;; Nudi novi potez ukoliko je odigran pogresan
(defun offerNewMove(state)
    (print "Your move was invalid, please enter a new one: ")
    (setq move (read))
    (setq state (makeMove state (car move) (cadr move) (caddr move) nil))
)
;; Proverava da li stack koji pokusavamo da pomerimo pocinje odredjenim znakom("X" ili "O")
(defun checkIfStackStartsWith (stack height)
    (if BW (if (equal (nth height stack) "X") t nil) (if (equal (nth height stack) "O") t nil))
    
)
;; Ukoliko je potez validan odigrava potez
(defun makeMove (state startPoint endPoint stackHeight bot) 
    (cond
        ;; Ako nismo na potezu ne mozemo da pomerimo protivnicki stack
        ((not (checkIfStackStartsWith (caddr (findNode startPoint state)) stackHeight))  (if (not bot) (offerNewMove state))) 
        ((if (not (validateMove state startPoint endPoint stackHeight))
            ;; ako nije validan potez onda proveravamo da li igra bot da ne bismo uvek nudili novi potez, potrebna su nam sva stanja
            (if (not bot) (offerNewMove state))
            ;; ako je potez validan idemo u true granu i treba da vratimo novo stanje
            (let* 
                (
                    (newStack (mergeStacks '("." "." "." "." "." "." "." "." ".") (getStackTo (caddr (findNode startPoint state)) stackHeight )))
                    (newState (changeStackOfElement state startPoint newStack))
                    (newState2 (changeStackOfElement newState endPoint (mergeStacks (caddr (findNode endPoint state)) (getStackStartingFrom (caddr (findNode startPoint state)) stackHeight))))   
                )
                newState2
            )
        ))
    )
)
;;vraca cvorove ciji stekovi pocinju sa X ili O u zavisnosti od BW
(defun findAppropriateNodes(state BW)
    (cond   ((null state) '())
            ((equal (car (caddr (findNode (caar state) state))) ".") (findAppropriateNodes (cdr state) BW))
            ;; ((checkIfStackStartsWith (caddr (findNode (caar state) state)) 0) (cons (caar state) (findAppropriateNodes (cdr state) BW)))
            (t (cons (caar state) (findAppropriateNodes (cdr state) BW)))
    )
)
;; Za svaki cvor koji vrati findAppropriateNodes da se pozove makeMove gde je startPoint iz findAppropriate, a endPoint iz findNeighbours
(defun makeAllMoves (state BW)
     (let* ((nodes (findAppropriateNodes state BW)))
        (cond ((null nodes) nil)
              (t(loop for x in nodes collect
                    (goodNeighbour state x (findNeighbours x state))
                )
              )
        )
    )
)
;; proverava da li moze sa neke visine steka da odigra
(defun createDifferentMoves (state startPoint endPoint)
    (let* ((nodeStack (caddr (findNode startPoint state))))
           (createListWithHeights state startPoint endPoint (countStack nodeStack) 0)
    )
)
;; 
(defun createListWithHeights(state startPoint endPoint nodeStack val)
    (cond ((equal val nodeStack) nil)
            (t (cons (makeMove state startPoint endPoint val T) 
                        (createListWithHeights state startPoint endPoint nodeStack (1+  val))))
    )
)
;; 
(defun goodNeighbour(state startPoint goodNodes)
    (cond ((null goodNodes) nil)
            (t (append (createDifferentMoves state startPoint (car goodNodes)) 
                            (goodNeighbour state startPoint (cdr goodNodes))))
    )
)
;; (trace makeMove)
;; Vraca stack od elementa n pa do prve tacke
(defun getStackStartingFrom(stack n)
    (cond 
        ((null stack) '())
        ((equal n '0) 
            (if (equal (car stack) ".")
                '()
                (cons (car stack) (getStackStartingFrom (cdr stack) n))
            )
        )
        (t (getStackStartingFrom (cdr stack) (- n 1)))
    )
)
;; Vraca stack od 0 do n
(defun getStackTo(stack n)
    (cond 
        ((<= n '0) '())
        (t (cons (car stack) (getStackTo (cdr stack) (- n 1))))
    )
)
;; Na stack1 dodaje stack2
(defun mergeStacks(stack1 stack2)
    (cond 
        ((null stack2) stack1)
        ((equal (car stack1) ".") (cons (car stack2) (mergeStacks (cdr stack1) (cdr stack2))))
        (t (cons (car stack1) (mergeStacks (cdr stack1) stack2)))
    )
)
;; Menja stack prosledjenom elementu
(defun changeStackOfElement(state elem stack)
    (cond 
        ((null state) '())
        ((equal (caar state) elem) (cons (append (append (list (caar state)) (list (cadar state))) (list stack)) (cdr state) ))
        (t (cons (car state) (changeStackOfElement (cdr state) elem stack)))
    )
)
                                                    ;; Faza 3 - Minmax algoritam sa alfa-beta odsecanjem
;; ;; Procena stanja, koliko koje vodi do pobede 
;; (defun evaluateState (state BW)
;;     (let ((node (car (findAppropriateNodes state BW))))
;;         (let ((stacks (findNode node state)))
;;             (+ (random 100)(countStack (caddr stacks)))
;;             ;; (random 100)
;;         ) 
;;     )
;; )
;; (trace evaluateState)
;;
(setq alpha MOST-NEGATIVE-FIXNUM)
(setq beta MOST-POSITIVE-FIXNUM)
;;
(defun max-value (state alpha beta depth BW sizeOfTable) ;;graph parametar
    (cond 
        ((zerop depth) (list state (evaluateState state BW)))
        (t (let* ((maxalpha (list state alpha))
                 (listOfMovesState (createClearList state BW)))
                (loop for s in listOfMovesState do
                 (loop for x in s do
                    (setq novoalpha (min-value x alpha beta (1- depth) (not BW) sizeOfTable))
                    (setq maxalpha (if (< (cadr maxalpha) (cadr novoalpha)) (list x (cadr novoalpha)) maxalpha))
                    (if (<= beta (cadr maxalpha)) (return-from max-value (list state beta)) maxalpha)
                ))
                (return-from max-value maxalpha)
            )
        )
    )
)
;;
(defun min-value (state alpha beta depth BW sizeOfTable) ;;graph parametar
    (cond 
        ((zerop depth) (list state (evaluateState state BW)))
        (t  (let* ((minbeta (list state beta))
                    (listOfMovesState (createClearList state BW)))
                (loop for s in listOfMovesState do
                 (loop for x in s do
                    (setq novobeta (max-value x alpha beta (1- depth) (not BW) sizeOfTable))
                    (setq minbeta (if (> (cadr minbeta) (cadr novobeta)) (list x (cadr novobeta)) minbeta))
                    (if (>= alpha (cadr minbeta)) (return-from min-value (list state alpha)) minbeta)
                ))
                (return-from min-value minbeta)
            )
        )
    )
)
;; BW prosledjujemo da bismo vrsili proveru koji potez se igra da li max-value ili min-value
(defun minimax(state depth alpha beta BW sizeOfTable result)
        (cond ((or (zerop depth) (checkEnd sizeOfTable result)) (list state (evaluateState state BW)))
              (t (if BW (max-value state alpha beta depth BW sizeOfTable) (min-value state alpha beta depth BW sizeOfTable)))
        )
)
;; ---------------------------------------------Faza 4---------------------------------------------------
(defparameter *T1-RULES* '(
	(if (and (Tile ?x) (!eq ?x "X")) then (existsX))
	(if (and (Tile ?x) (!eq ?x "O")) then (existsO))
	(if (and (Tile ?x) (OnTopTile ?p) (!eq ?x ?p) (!eq ?x "X"))  then (onTopX))
	(if (and (Tile ?x) (OnTopTile ?p) (!eq ?x ?p) (!eq ?x "O"))  then (onTopO))	   
	(if (and (Tile ?x) (OnTopTile ?y) (Height ?z) (!eq ?x ?y) (!eq ?x "X")  (!eq ?z 8)) then (winnerX) )
	(if (and (Tile ?x) (OnTopTile ?y) (Height ?z) (!eq ?x ?y) (!eq ?x "O") (!eq ?z 8)) then (winnerO) )
	)
)
;; 
(defun createFacts(state player)
	(let 
		(
			(myList '())
		)
		(progn
			(if player
				(setq myList (list (list 'Tile "X")))
				(setq myList (list (list 'Tile "O")))
			)
			(setq myList (append myList (list (list 'Height (countStack (caddr state)) ))))
			(setq myList (append myList (list (list 'OnTopTile (getTopOfStack (caddr state)) ))))
			;; (setq myList (append myList (list (list 'OnTopTile (getTopOfStack (caddr state)) ))))
		)
		myList
	)
)
;; 
(defun getTopOfStack(stack)
	(cond 
		((null stack) '())
		((equal (car stack) ".") '())
		((equal (cadr stack) ".") (car stack))
		(t (getTopOfStack (cdr stack)))
	)
)
;; 
(defun evaluateState(state BW) 
    (evaluateStateStack state BW)	
)
;; 
(defun evaluateStateStack(state BW)
	(cond
		((null state) 0)
		(t 
		(+ (progn
			(setq *T1-FACTS* (createFacts (car state) BW) )
			(prepare-knowledge *T1-RULES* *T1-FACTS* 2)
			(if BW
				(+
					(* (count-results '(existsX)) 1)
					(* (count-results '(winnerX)) 1000)
					(* (count-results '(onTopX)) 5)
					(* (count-results '(existsO)) -1)
					(* (count-results '(winnerO)) -1000)
					(* (count-results '(onTopO)) -5)
				)
				(-
					(* (count-results '(existsO)) 1)
					(* (count-results '(winnerO)) 1000)
					(* (count-results '(onTopO)) 5)
					(* (count-results '(existsX)) -1)
					(* (count-results '(winnerX)) -1000)
					(* (count-results '(onTopX)) -5)
				)
			)
			)
			(evaluateStateStack (cdr state) BW)
		)
		)
	)
)
;; PREDEFINISANI PREDIKATI
(defun !eq (a b)
  (equal a b)
)
(defun !ne (a b)
  (not (equal a b))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                                                ;;;;;
;;;;;                                       INFERENCE ENGINE                                         ;;;;;
;;;;;                                                                                                ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POMOCNE FUNKCIJE
;; provera da li je parametar s izvorna promenljiva (simbol koji pocinje sa ?)
(defun true-var? (s) 
  (if (symbolp s)
      (equal #\? (char (symbol-name s) 0))
    nil))
;; provera da li je parametar s promenljiva (simbol koji pocinje sa ? ili %)
(defun var? (s) 
  (if (symbolp s)
      (let ((c (char (symbol-name s) 0)))
        (or (equal c #\?) (equal c #\%)))
    nil))
;; provera da li je parametar s funkcija (simbol koji pocinje sa =)
(defun func? (s) 
  (if (symbolp s)
      (equal #\= (char (symbol-name s) 0))
    nil))
;; provera da li je parametar s predefinisani predikat (simbol koji pocinje sa !)
(defun predefined-predicate? (s)
  (if (symbolp s)
      (equal #\! (char (symbol-name s) 0))
    nil))
;; provera da li je parametar s konstanta (ako nije promenljiva ili funkcija onda je konstanta)
(defun const? (s)
  (not (or (var? s) (func? s))))
;; rekurzivna provera da li je parametar f funkcija od parametra x
(defun func-of (f x)
  (cond
   ((null f) ; kraj rekurzije
    t)
   ((atom f)
    (equal f x))
   (t
    (or (func-of (car f) x) (func-of (cdr f) x)))))
;; provera da li funkcija f ima promenljivih
(defun has-var (f)
  (cond
   ((null f) 
    nil)
   ((atom f)
    (var? f))
   (t
    (or (has-var (car f)) (has-var (cdr f))))))
;; funkcija koja vraca konsekvencu pravila
(defun rule-consequence (r)
  (car (last r)))
;; funkcija koja vraca premisu pravila
(defun rule-premises (r)
  (let ((p (cadr r)))
    (if (and (listp p) (equal (car p) 'and))
        (cdr p)
      (list p))))
;; funkcija koja vrsi prebacivanje upita u interni format (izbacuje 'and)
(defun format-query (q)
  (if (and (listp q) (equal (car q) 'and))
      (cdr q)
    (list q)))
;; izracunavanje istinitosne vrednosti predefinisanog predikata
(defun evaluate-predicate (p ls)
  (if (has-var p) nil  ; ako poseduje slobodne promenljive vraca nil (nije validna situacija)
    (if (eval p) 
        (list ls) ; ako predikat vazi vraca ulaznu listu smena
      nil))) ; u suprotnom vraca nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFEJSNE FUNKCIJE I GLOBALNE PROMENLJIVE
(defparameter *FACTS* nil)
(defparameter *RULES* nil)
(defparameter *MAXDEPTH* 10)
;; priprema *FACTS*, *RULES* i *MAXDEPTH*
(defun prepare-knowledge (lr lf maxdepth)
  (setq *FACTS* lf *RULES* (fix-rules lr) *MAXDEPTH* maxdepth))
;; vraca broj rezulata izvodjenja
(defun count-results (q)
  (length (infer- (format-query q) '(nil) 0)))
;; vraca listu lista smena
(defun infer (q)
  (filter-results (infer- (format-query q) '(nil) 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNKCIJE KOJE VRSE DODELU NOVIH JEDINSTVENIH PROMENLJIVIH PRAVILIMA
(defun fix-rules (lr)
  (if (null lr) nil
    (cons (fix-rule (car lr)) (fix-rules (cdr lr))))
)
(defun fix-rule (r)
  (let ((ls (make-rule-ls r nil)))
    (apply-ls r ls))
)
(defun make-rule-ls (r ls)
  (cond
   ((null r)
    ls)
   ((var? r)
    (let ((a (assoc r ls)))
      (if (null a)
          (cons (list r (gensym "%")) ls)
        ls)))
   ((atom r)
    ls)   
   (t
    (make-rule-ls (cdr r) 
                  (make-rule-ls (car r) ls))))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNKCIJE KOJE VRSE PRIPREMU REZULTATA (IZBACUJU SMENE KOJE SE ODNOSE NA INTERNE PROMENLJIVE)
(defun filter-results (lls)
  (if (null lls) nil
    (cons (filter-result (car lls)) (filter-results (cdr lls))))
)
(defun filter-result (ls)
  (if (null ls) nil
    (if (true-var? (caar ls))
        (cons (car ls) (filter-result (cdr ls)))
      (filter-result (cdr ls))))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNKCIJE KOJE SE KORISTE U IZVODJENJU
;; glavna funkcija za izvodjenje, vraca listu lista smena
;; lq - predikati upita
;; lls - lista listi smena (inicijalno lista koja sadrzi nil)
;; depth - tekuca dubina (inicijalno 0)
(defun infer- (lq lls depth)
  (if (null lq) lls
    (let ((lls-n (infer-q (car lq) lls depth)))
      (if (null lls-n) nil
        (infer- (cdr lq) lls-n depth))))
)
;; izvodjenje za jedan predikat iz upita, vraca listu lista smena
(defun infer-q (q lls depth)
  (if (null lls) nil
    (let ((lls-n (infer-q-ls q (car lls) depth)))
      (if (null lls-n)
          (infer-q q (cdr lls) depth)
        (append lls-n (infer-q q (cdr lls) depth)))))
)
;; izvodjenje za jedan predikat sa jednom listom smena, vraca listu lista smena
(defun infer-q-ls (q ls depth)
  (if (predefined-predicate? (car q))
      (evaluate-predicate (apply-ls q ls) ls)
    (if (< depth *MAXDEPTH*)
        (append (infer-q-ls-lf q *FACTS* ls) (infer-q-ls-lr q *RULES* ls depth))
      (infer-q-ls-lf q *FACTS* ls)))
)      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; izvodjenje nad bazom cinjenica lf, vraca listu lista smena
(defun infer-q-ls-lf (q lf ls)
  (if (null lf) nil
    (let ((ls-n (infer-q-ls-f q (car lf) ls)))
      (if (null ls-n)
          (infer-q-ls-lf q (cdr lf) ls)
        (if (null (car ls-n)) ls-n
          (append ls-n (infer-q-ls-lf q (cdr lf) ls))))))
)
;; izvodjenje sa jednom cinjenicom, vraca listu sa listom smena
(defun infer-q-ls-f (q f ls)
  (if (= (length q) (length f)) ; provera na istu duzinu
      (infer-q-ls-f- q f ls)
    nil)
)
;; izvodjenje sa jednom cinjenicom, vraca listu sa listom smena
(defun infer-q-ls-f- (q f ls)
  (if (null q) (list ls)
    (let ((nq (apply-and-eval (car q) ls)) (nf (car f)))
      (if (var? nq) 
          (infer-q-ls-f- (cdr q) (cdr f) (append ls (list (list nq nf))))
        (if (equal nq nf) 
            (infer-q-ls-f- (cdr q) (cdr f) ls)
          nil))))
)          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; izvodjenje nad bazom pravila, vraca listu lista smena
(defun infer-q-ls-lr (q lr ls depth)
  (if (null lr) nil
    (let ((ls-n (infer-q-ls-r q (car lr) ls depth)))
      (if (null ls-n)
          (infer-q-ls-lr q (cdr lr) ls depth)
        (if (null (car ls-n)) ls-n
          (append ls-n (infer-q-ls-lr q (cdr lr) ls depth)))))))
;; izvodjenje sa jednim pravilom, vraca listu sa listom smena
(defun infer-q-ls-r (q r ls depth)
  (let ((c (rule-consequence r)))
    (if (= (length q) (length c))
        (let ((lsc (unify q c nil ls)))
          (if (null lsc) nil
            (infer- (apply-ls (rule-premises r) (car lsc)) (cdr lsc) (1+ depth))))
      nil)))
;; unifikacija predikata upita q i konsekvence pravila c primenom liste smena ls, vraca listu smena
(defun unify (q c uls ls)
  (if (or (null q) (null c))
      (if (and (null q) (null c)) (list uls ls) nil)
    (let ((eq (car q)) (ec (car c)))
      (cond
       ((equal eq ec)
        (unify (cdr q) (cdr c) uls ls))
       ((var? eq)
        (cond
         ((var? ec)
          (let ((a (assoc ec uls)))
            (cond
             ((null a)              
              (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
             ((equal (cadr a) eq)
              (unify (cdr q) (cdr c) uls ls))
             (t
              nil))))
         ((func? ec)
          nil)
         (t ;; const
          (let ((a (assoc eq ls)))
            (cond
             ((null a)
              (unify (cdr q) (cdr c) uls (cons (list eq ec) ls)))
             ((equal (cadr a) ec)
              (unify (cdr q) (cdr c) uls ls))
             (t 
              nil))))))
       ((func? eq)
        (cond
         ((var? ec)
          (if (func-of eq ec) nil
            (let ((a (assoc ec uls)))
              (cond
               ((null a)              
                (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
               ((equal (cadr a) eq)
                (unify (cdr q) (cdr c) uls ls))
               (t
                nil)))))
         ((func? ec)
          nil)
         (t ;; const
          (let ((f (apply-ls eq ls)))
            (if (has-var f) nil
              (if (equal (eval f) ec)
                  (unify (cdr q) (cdr c) uls ls)
                nil))))))
       (t ;; const
        (cond
         ((var? ec)
          (let ((a (assoc ec uls)))
            (cond
             ((null a)              
              (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
             ((equal (cadr a) eq)
              (unify (cdr q) (cdr c) uls ls))
             (t
              nil))))
         (t ;; func or const
          nil)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMENA LISTE SMENA I IZRACUNAVANJE IZRAZA
(defun apply-and-eval (x ls)
  (if (var? x)
      (apply-ls x ls)
    (if (and (listp x) (func? (car x)))
        (eval (apply-ls x ls)) 
      x)))
;; primena liste smena ls na izraz x
(defun apply-ls (x ls)
  (cond
   ((null x)
    x)
   ((var? x)
    (let ((ax (assoc x ls)))
      (if (null ax) x
        (cadr ax))))
   ((atom x)
    x)
   (t
    (cons (apply-ls (car x) ls) (apply-ls (cdr x) ls)))))
;; (trace validateMove)
;; (trace evaluateState)
;; (trace minimax)
;; (trace max-value)
;; (trace min-value)
;; (trace createClearList)
(drawTable)