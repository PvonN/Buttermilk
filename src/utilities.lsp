(in-package :bm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; deinterleave
;;; seperates a list into two lists and gives it out as a nested list
;;; in this form ((odd-indeces) (even-indeces))
(defun deinterleave (lst)
  (loop for i from 1 to (length lst)
	for e in lst
	when (oddp i) collect e into lst-1
	  when (evenp i) collect e into lst-2
	    finally (return (list lst-1 lst-2))))
#|
(deinterleave '(a b c d e f g)) 
=> ((A C E G) (B D F))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; last-value
;;; gives back the last value of a list
(defun last-value (lst)
  (car (reverse lst)))
#|
(last-value '(1 2 3 4 5))
=> 5
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fill-list
;;; fills a list of size N with value X
(defun fill-list (size val)
  (loop repeat size
	collect val))
#|
(fill-list 5 'k)
=> (K K K K K)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; between
;;; creates random values between two numbers
(defun between (mini maxi)
  (+ mini (random (- maxi mini))))
#|
(between 4 9)
=> 7

(between 4.0 9.0)
=> 5.323592
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; deviate-list
;;; deviate the values of a list by a fixed percentage, while 100 means
;;; a value of the list could be doubled or divided by 2
(defun deviate-list (lst percent)
  (let* ((deviation-amount (/ percent 100.0)))
    (loop for i in lst
	  collect (* i (between
			(abs (1- (* deviation-amount 0.5)))
			(1+ deviation-amount))))))
#|
(deviate-list '(1 2 3 4 5) 25)
=> (1.245879 2.3614287 2.6611075 4.459944 5.2167025)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-without-first-and-last
;;; give back the list without the first and last element
(defun list-without-first-and-last (lst)
  (reverse (rest (reverse (rest lst)))))
#|
(list-without-first-and-last '(1 2 3 4 5))
=> (2 3 4)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; swap-first-last
;;; swap first and last element of a list
(defun swap-first-last (lst)
  (append (last lst) (list-without-first-and-last lst)
	  (list (first lst))))
#|
(swap-first-last '(1 2 3 4 5))
=> (5 2 3 4 1)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-up-to-index
;;; return list up to index 
(defun list-up-to-index (lst index)
  (if (> index (1- (length lst)))
      (error "index is bigger then length of list")
      (loop for item in lst
	    for i from 0 to index
	    collect item)))
#|
(list-up-to-index '(1 2 3 4 5 6 7 8 9) 4)
=> (1 2 3 4 5)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-from-i-to-j
;;; return the list from index-i to index-j
(defun list-from-i-to-j (lst i j)
  (loop for item in lst
	for index from 0 to (length lst)
	when (and (<= index j) (>= index i))
	  collect item))
#|
(list-from-i-to-j '(0 1 2 3 4 5 6 7 8 9 10) 2 6)
=> (2 3 4 5 6)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-without-last
;;; return list without last element
(defun list-without-last (lst)
  (loop for item in lst
	for i from 0 to (- (length lst) 2)
	collect item))
#|
(list-without-last '(1 2 3 4 5))
=> (1 2 3 4)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-without
;;; returns list without the element with given index
(defun list-without (lst index)
  (append (list-up-to-index lst (1- index))
	  (list-from-i-to-j
	   lst (1+ index) (length lst))))
#|
(list-without '(0 1 2 3 4 5 6) 3)
=> (0 1 2 4 5 6)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; crop-list
;;; shortens a list to a given length
(defun crop-list (lst leng)
  (let* ((index (1+ (random (length lst)))))
    (cond
      ((eq (length lst) leng) lst)    
      ((eq (nth index lst)
	   (nth (1+ index) lst))
       (crop-list (list-without lst index) leng))    
      ((eq (nth index lst)
	   (nth (1- index) lst))
       (crop-list (list-without lst index) leng))
      (t (crop-list (list-without-last lst) leng)))))
#|
(crop-list '(0 1 2 3 4 5 6 7) 3)
=> (0 1 2)
|#

(defun swap-elements-of-index (lst i j)
  "swap elements of index-i  and index-j"
  (loop for item in lst
	for index from 0 to (length lst)
	when (= index i)
	  collect item into first-element
	when (= index j)
	  collect item into second-element
	when (< index i)
	  collect item into first-list
	when (and (> index i) (< index j))
	  collect item into second-list
	when (> index j)
	  collect item into last-list
	finally (return
		  (append first-list
			  second-element
			  second-list
			  first-element
			  last-list))))

(defun fill-list-with-elements (&rest elem-num-lst)
  "fills a list with a specific number of elements '(e 7) '(a 17)"
  (let* ((result '()))
    (dolist (lst elem-num-lst)
      (destructuring-bind (elem num) lst
	(dotimes (i num)
	  (push elem result))))
    (reverse result)))

(defun repeat-lst (rep lst)
  (loop repeat rep
	collect lst into new-lst
	finally (return (almost-flatten new-lst))))
;(repeat-lst 5 '(1 2 3))

(defun shorten-list-to
    (lst to-index &key (stepsize 1) (repetitions 1) (max-leng 100))
  (if (> to-index (1- (length lst)))
      (error "given index is bigger then lst") 
      (loop for i downfrom (1- (length lst)) by stepsize to to-index
	    collect (repeat-lst repetitions (list-up-to-index lst i))
	      into new-lst
	    until (>= (length (flatten new-lst)) max-leng)
	    finally (return (flatten new-lst)))))
;;(shorten-list-to '(1 2 3 4 5 6 7 8 9 10) 3 :by 2 :repetitions 2)

(defun lengthen-list-from
    (lst from-index &key (stepsize 1) (repetitions 1) (max-leng 100))
  (if (> from-index (1- (length lst)))
      (error "given index is bigger then lst") 
      (loop for i from from-index by stepsize to (1- (length lst))
	    collect (repeat-lst repetitions (list-up-to-index lst i))
	      into new-lst
	    until (>= (length (flatten new-lst)) max-leng)
	    finally (return (flatten new-lst)))))
;;(lengthen-list-from '(1 2 3 4 5 6 7 8 9 10) 9 :by 5 :repetitions 4)


(defun make-segments (lst size)
  (cond ((null lst) nil)
	((< (length lst) size)
	 (list lst))
	(t
	 (loop for i from 0 to (1- size)
	       for e in lst
	       collect e into segment
	       finally
		  (return (cons segment
				(make-segments (nthcdr size lst) size)))))))
;;(make-segments '(1 2 3 4 5 6 7 8 9 10) 6)

(defun my-quick-sort (lst)
  "quick-sort algorithm"
  (let* ((pivot-index (1- (length lst))))
	 (cond ((null lst) nil)
	       ((= pivot-index -1) nil)
	       (t (loop for item in lst
			when (< item (nth pivot-index lst))
			  collect item into left-list
			when (> item (nth pivot-index lst))
			  collect item into right-list
			when (= item (nth pivot-index lst))
			  collect item into middle-list
			finally (return (append 
					 (my-quick-sort left-list)
					 middle-list
					 (my-quick-sort right-list))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creates a list of values
;; draws a line between two values by n steps
;;(draw-line 10 90 270) => (90.0 110.0 130.0 150.0 170.0 190.0 210.0 230.0 250.0 270)
(defun line (start steps aim)
  (let* ((distance (abs (- aim start)))
	 (stepsize (/ distance (1- steps))))
    (cond ((< start aim)
	   (loop for i from start to aim by stepsize
	      collect (float i) into new-lst
	      until (eq (length new-lst) steps)
	      finally (return (append (list-without-last new-lst)
				      (list aim)))))
	  ((> start aim)
	   (loop for i from start downto aim by stepsize
	      collect (float i) into new-lst
	      until (eq (length new-lst) steps)
	      finally (return (append (list-without-last new-lst)
				      (list aim)))))
	  ((eq start aim)
	   (repeat start steps)))))
