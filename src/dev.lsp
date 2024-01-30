;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test for writing nummeric rhythms which are based on arithmetic
;;; operations


(defun number-to-duration (n bpm &key (sec-or-ms 0))
  (if (= sec-or-ms 0)
      (float (* (/ 1 n) (/ 4 (/ 120 60))))
      (* 1000 (float (* (/ 1 n) (/ 4 (/ 120 60)))))))

					;(number-to-duration 1/3 90 :sec-or-ms 0)

(defun numbers-to-duration (lst bpm &key (sec-or-ms 0))
  (loop for e in lst
	collect (number-to-duration e bpm :sec-or-ms sec-or-ms)))

					;(numbers-to-duration '(1 8 2 3 0.3 2) 141 :sec-or-ms 1)

(let* ((bpm 191)
       (rhythm-base (let* ((a '(8 8 8 4 16 16 16 16 8 4 2 4 8))
			   (b '())
			   (c '())
			   (d '())
			   (e '())
			   (f '())
			   (g '())
			   (h '())
			   (i '()))
		      (list a b c d e f g h i)))
       (sound-base (let* ((a (lengthen-list-from
			      '(0 0 0 1 0 0 1 0 1 1)
			      2 :repetitions 5))
			  (b '())
			  (c '())
			  (d '())
			  (e '())
			  (f '())
			  (g '())
			  (h '())
			  (i '()))
		     (list a b c d e f g h i)))
       (kick-rhythm (let* ((a (lengthen-list-from
			       (nth 0 rhythm-base)
			       4 :repetitions 5
				 :stepsize 2
				 :max-leng 20000))
			   (b '())
			   (c '())
			   (d '())
			   (e '())
			   (f '())
			   (g '())
			   (h '())
			   (i '()))
		      (append a b c d e f g h i)))
       (kick-sound (let* ((a (nth 0 sound-base))
			  (b '())
			  (c '())
			  (d '())
			  (e '())
			  (f '())
			  (g '())
			  (h '())
			  (i '()))
		     (append a b c d e f g h i))))
  (write-coll (numbers-to-duration kick-rhythm bpm :sec-or-ms 1)
	      "/Users/philippneumann/Documents/Komposition-Video/Matrix-Study/kick-rhythm.txt")
  (write-coll kick-sound
	      "/Users/philippneumann/Documents/Komposition-Video/Matrix-Study/kick-sound.txt"))
