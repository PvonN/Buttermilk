(in-package :bm)
;; creates a list of the first n of the fibonacci row
(defun fibo (n)
  (loop repeat n
	for x = 1 then y
	and y = 1 then (+ x y)
	collect x))

;; (fibo 10)
