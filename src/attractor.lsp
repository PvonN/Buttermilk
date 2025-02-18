(in-package :bm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lorenz-attractor
;; Generates values based on the lorenz attractor
;; (https://en.wikipedia.org/wiki/Lorenz_system)
;; The output is a nested list with three lists holding the values for
;; the x-, y- and z-axis.
;; x-start, y-start and z-start are the start values, also the first
;; values in the output lists. the sigma, rho, beta and delta values can be
;; lists of values, which are stepped through as cycling lists and are
;; reapeating their values when num-of-values > lenght of sigma, rho,
;; beta or delta.
;; if skip > 1 every nth value is collected into the output list 
(defun lorenz-attractor (num-of-values x-start y-start z-start sigma
			 rho beta &key (delta 0.001) (skip 1))  
  (let* ((sigma-cl (if (listp sigma)
		   (make-cscl sigma)
		   (make-cscl (list sigma))))
	 (rho-cl (if (listp rho)
		   (make-cscl rho)
		   (make-cscl (list rho))))
	 (beta-cl (if (listp beta)
		   (make-cscl beta)
		   (make-cscl (list beta))))
	 (delta-cl (if (listp delta)
		       (make-cscl delta)
		       (make-cscl (list delta))))
	 (first-sigma (get-next sigma-cl))
	 (first-rho (get-next rho-cl))
	 (first-beta (get-next beta-cl))
	 (first-delta (get-next delta-cl)))	 
    (loop for i from 0 below (* num-of-values skip)
	  for sigma = first-sigma then (get-next sigma-cl)
	  and rho = first-rho then (get-next rho-cl)
	  and beta = first-beta then (get-next beta-cl)
	  and delta = first-delta then (get-next delta-cl)
	  for x = x-start then
			  (+ x (* delta (* sigma (- y x))))
	  and y = y-start then
			  (+ y (* delta (- (* x (- rho z)) y)))
	  and z = z-start then
			  (+ z (* delta (- (* x y) (* beta z))))
	  if (zerop (mod i skip))
	    collect x into x-values
	    and collect y into y-values
	    and collect z into z-values 	  	 
	  finally (return (list x-values y-values z-values)))))
#|
(lorenz-attractor 10 .01 0.5 0.2 4 1 10)
==>
((0.01 0.01196 0.013910192 0.015850624 0.017781343 0.019702395 0.021613829
  0.02351569 0.025408026 0.027290883)
 (0.5 0.499508 0.49901807 0.49853024 0.4980445 0.4975608 0.4970792 0.49659964
  0.49612218 0.49564677)
 (0.2 0.198005 0.19603093 0.19407757 0.19214469 0.1902321 0.18833958 0.18646692
0.18461393 0.1827804))

(lorenz-attractor 10 .01 0.5 0.2 4 1 10 :skip 2 :delta '(0.001 0.002 0.004))
==>
((0.01 0.021681536 0.027367087 0.036694814 0.04757942 0.05287682 0.061567504
  0.07170827 0.076643445 0.08473971)
 (0.5 0.49706468 0.4956304 0.49327236 0.490511 0.4891629 0.48694754 0.48435533
  0.4830907 0.48101327)
 (0.2 0.18819739 0.18262309 0.17363591 0.16347088 0.1586707 0.15093224
  0.14218093 0.13804898 0.13138841))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; thomas-attractor
;; Generates values based on the thomas attractor
;; (https://en.wikipedia.org/wiki/Thomas'_cyclically_symmetric_attractor)
;; The output is a nested list with three lists holding the values for
;; the x-, y- and z-axis.
;; x-start, y-start and z-start are the start values, also the first
;; values in the output lists. the b and delta values can be
;; lists of values, which are stepped through as cycling lists and are
;; reapeating their values when num-of-values > lenght of b or delta.
;; if skip > 1 every nth value is collected into the output list 
(defun thomas-attractor (num-of-values x-start y-start z-start b		 
			 &key (delta 0.001) (skip 1))  
  (let* ((b-cl (if (listp b)
		   (make-cscl b)
		   (make-cscl (list b))))
	 (delta-cl (if (listp delta)
		       (make-cscl delta)
		       (make-cscl (list delta))))
	 (first-b (get-next b-cl))
	 (first-delta (get-next delta-cl)))	 
    (loop for i from 0 below (* num-of-values skip)
	  for b = first-b then (get-next b-cl)
	  and delta = first-delta then (get-next delta-cl)
	  for x = x-start then
			  (+ x (* delta (+ (* (* -1 b) x) (sin y))))
	  and y = y-start then
			  (+ y (* delta (+ (* (* -1 b) y) (sin z))))
	  and z = z-start then
			  (+ z (* delta (+ (* (* -1 b) z) (sin x))))
	  if (zerop (mod i skip))
	    collect x into x-values
	    and collect y into y-values
	    and collect z into z-values 	  	 
	  finally (return (list x-values y-values z-values)))))

#|
(thomas-attractor 10 0.01 0.9 0.3 0.986)
==>
((0.01 0.010773467 0.011545803 0.01231701 0.013087088 0.013856038 0.014623863
  0.015390561 0.016156135 0.016920585)
 (0.9 0.8994081 0.8988165 0.89822525 0.8976343 0.8970437 0.8964534 0.89586335
  0.8952737 0.8946843)
 (0.3 0.2997142 0.29942948 0.2991458 0.29886314 0.29858154 0.29830098
  0.29802147 0.297743 0.29746556))

(thomas-attractor 10 0.01 0.9 0.3 0.986 :delta '(0.001 0.01 0.1 0.1) :skip 20)
==>
((0.01 0.39782786 0.4019751 0.38270926 0.37515205 0.37135294 0.36807853
  0.36491716 0.3618975 0.35903248)
 (0.9 0.47535196 0.382231 0.37516207 0.3742683 0.3714618 0.36813834 0.36492598
  0.36189702 0.35903206)
 (0.3 0.2895947 0.3645149 0.37871227 0.37578005 0.37165445 0.36811996
  0.36491418 0.36189514 0.35903212))
|#


