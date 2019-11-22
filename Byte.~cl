(defun drawTable() 
 (initializeTable))

(defun initializeTable() 
    (print "Do you want to play with table 8 or 10 : ")
    (setq sizeOfTable (read))
    ;; (print sizeOfTable)
    (print "Do you want to play human-human or human-computer, choose h or c: ")
    (setq modeGame (read))
    ;; (print modeGame)
    (if (equal sizeOfTable '8) (drawEightFields) (:else (drawTenFields)))
    )

(defun drawEightFields() 
    (print "Usao sam u 8"))

(defun drawTenFields()
    (print "Usao sam u 10"))

(drawTable)