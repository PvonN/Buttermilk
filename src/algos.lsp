;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dynamic-activities
;;; - create a nnested list
;;; find the first 1 after a 0
(defun find-step-up (ls)
  (loop for i from 0
	for current in ls and last = (car ls) then current
	when (> current last) do (return i)
	  finally (return 0)))

(defun dynamic-activities (ls activity-difference)
  (let* ((len (length ls))
	 (is-greater (> activity-difference 0))
	 (result (copy-seq ls)))
    ;; sanity checks
    (unless (loop for i in ls always (and (numberp i) (<= 0 i 1)))
      (error "all elements of ls in dynamic-activities should be 0 or 1: ~a"ls))
    (unless (> len 1)
      (error "length of list in dynamic-activities was <= 1: ~a"ls))
    ;; change list if neccessary
    (unless (= activity-difference 0)
      (loop repeat (+ (abs activity-difference)(if is-greater (apply #'+ ls) 0))
	    for i from (find-step-up ls)
	    do (setf (nth (mod i len) result)(if is-greater 1 0))))
    ;; return (altered) copy of original list
    result))

(defun activities-from-states (states nvoices)
  (loop for s in states
	and old-s = (car states) then s
	for ls = (dynamic-activities (sc::ml 0 nvoices) (car states))
	  then (dynamic-activities ls (- s old-s))
	collect ls))

(defun activitie-lists (length nvoices peak)
  (let* ((procession-list (procession length nvoices :peak peak))
	 (activities (activities-from-states procession-list
					     nvoices)))
    (loop for i from 0 below nvoices
	  collect (loop for j in activities
			collect (nth i j)))))
