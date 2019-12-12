(defun drawTable()
;;  (print state)
 (initializeTable))
(defun initializeTable() 
    (let* ((state '())
    (sizeOfTable (progn (print "Do you want to play with table 8 or 10: ") (read)))
    ;; (print sizeOfTable)
    (modeGame (progn (print "Do you want to play human vs human or human vs computer, choose h or c: ") (read)))
    ;; (print modeGame)
    (state (chooseTable sizeOfTable))
    )
    (print state)
    (draw sizeOfTable state)
    (setq firstPlayerTurn '1)
    ;; (print (checkIfStackStartsWith (caddr (findNode '(D 2) state)) 2))
    ;; (print (checkIfStackStartsWith (caddr (findNode '(D 2) state)) 1))
    ;; (print (checkEnd sizeOfTable result))
    ;; (print (countStack '("X" "O" "." "." "." "." "." "." ".")))
    ;; (print (findNode '(A 1) state))
    ;; (print (checkIfNodeExists '(A 1) '((B 2) (C 3))))
    ;; (print (checkIfNeighboursHaveStack '((B 2)) state))
    ;; (print (checkIfNeighbours '(A 1) '(B 2) state))
    ;; (print (breadthFirst state '((B 2)) '()))
    ;; (loop while
    ;; (setq firstPlayerTurn '1)
    (loop 
     (setq move (progn (print "Play the move: ") (read)))
     (setq state (makeMove state (car move) (cadr move) (caddr move)))
     (draw sizeOfTable state)
     (setq firstPlayerTurn (1+ firstPlayerTurn))
    )
    ;; (setq state (makeMove state '(B 2) '(C 1) '0))
    ;; (draw sizeOfTable state)
    ;; (setq state (makeMove state '(C 1) '(D 2) '0))
    ;; (setq state (makeMove state '(C 3) '(D 4) '0))
    ;; (setq state (makeMove state '(D 4) '(E 3) '0))
    ;; (setq state (makeMove state '(F 4) '(E 3) '0))
    ;; (draw sizeOfTable state)
    ;; (setq state (makeMove state '(D 2) '(E 3) '2))
     (draw sizeOfTable state)
    )
)
;; Funkcija koja iscrtava tablu u zavisnosti od prosledjene velicine
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
;; Proverava da li je potez validan
(defun validateMove (state start endPoint stackHeight) 
	;; proverava da li su cvorovi susedi
	(if (checkIfNeighbours start endPoint state) 
		;; proverava da li susedi imaju stack
		(if (checkIfNeighboursHaveStack (cadr (findNode start state)) state )
			;; susedi imaju stack->
			;; proverava da li je visina stacka sa kojeg se pomera jednaka 0, tada se ne deli stack
			(if (equal stackHeight '0)
				;; ukoliko je 0 onda rezultujuci stack mora da ima vrednost manju od 0
				(if (> (+ (- (countStack (caddar (findNode start state))) stackHeight)
		                    (countStack (caddar (findNode endPoint state)))) '8 )
		        		nil
		    			t
					)
				;; ako delimo stack onda proverava da li endPoint ima stack jednak 0
		    	(if (equal (countStack (caddar (findNode endPoint state))) '0 ) 
					;; vraca nil ako ima
		    	   nil
					;; da li je visina steka na koji se pomera visa od plocice sa koje se pomera
                        (if (< (countStack (caddar (findNode endPoint state))) stackHeight) 
                            nil
                            ;; rezultujuci stack ne sme da ima vise od 8 plocica
                            (if (> (+ (- (countStack (caddar (findNode start state))) stackHeight)
                                    (countStack (caddar (findNode endPoint state)))) '8 )
                                nil
                                t
                            )
                        )
                    ;; nije ispunjen taj uslov za deobu steka
                    ;; nil
                    ;; )
				)
            )
			;; susedi su ali susedi nemaju stack
		    (checkIfEndNodeExists state start endPoint))
		;; nisu susedi vrati nil
		nil
		)
)
;; Nudi novi potez ukoliko je odigran pogresan
(defun offerNewMove(state)
    (print "Your move was invalid, please enter a new one: ")
    (setq move (read))
    (setq state (makeMove state (car move) (cadr move) (caddr move)))
)
;; Proverava da li stack koji pokusavamo da pomerimo pocinje odredjenim znakom("X" ili "O")
(defun checkIfStackStartsWith (stack height)
    (if (equal (mod firstPlayerTurn 2) 1) (if (equal (nth height stack) "X") t nil) (if (equal (nth height stack) "O") t nil))
    
)
;; (print (checkIfStackStartsWith (caddr (findNode startPoint state)) 2)
;; Ukoliko je potez validan odigrava potez
(defun makeMove (state startPoint endPoint stackHeight ) 
	(cond
        ;; Ako nismo na potezu ne mozemo da pomerimo protivnicki stack
        ((not (checkIfStackStartsWith (caddr (findNode startPoint state)) stackHeight)) (offerNewMove state)) 
		((not (validateMove state startPoint endPoint stackHeight)) (offerNewMove state))
		(t 
			(let* 
				(
					(newState (changeStackOfElement state startPoint (mergeStacks '("." "." "." "." "." "." "." "." ".") (getStackTo (caddr (findNode startPoint state)) stackHeight )) ) )
					(newState2 (changeStackOfElement newState endPoint (mergeStacks (caddr (findNode endPoint state)) (getStackStartingFrom (caddr (findNode startPoint state)) stackHeight))))
                )
				newState2
			)
		)
	)
)
(trace checkIfStackStartsWith)
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
;; (trace breadthFirst)
(drawTable)
