(in-package :bm)
;;; write-coll creates a text file to be read with the coll object in
;;; Max/MSP
(defun write-coll (lst path)
  (with-open-file (orc path :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (loop for i from 0 to (- (length lst) 1)
	  do (format orc "~a, ~a;~&" i (nth i lst)))))


;;(setf my-list '(0 1 1 2 3 5 8 13 21))
;;(write-coll my-list "~/Desktop/test.txt")
