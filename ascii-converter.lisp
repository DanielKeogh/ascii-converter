;;;; ascii-converter.lisp

(in-package #:ascii-converter)

(defvar *chars* nil)

(defun populate-chars ()
  (loop for a below (array-dimension *chars* 0) do
    (loop for b below (array-dimension *chars* 1) do
      (loop for c below (array-dimension *chars* 2) do
	(loop for d below (array-dimension *chars* 3) do
	  (format t "~%~a~a~%~a~a" a b c d)
	  (format t "~%~a" (aref *chars* a b c d))
	  (let ((r (read-line)))
	    (when (> (length r) 0)
	      (setf (aref *chars* a b c d) r))))))))

(defun get-char (a b c d)
  (aref *chars*  a b c d))

(defun img->ascii (img)
  (labels ((get-pixel (x y)
	     (let*  ((pixel (imago:image-pixel img x y))
		     (color (imago:gray-intensity pixel)))
	       (floor (* 4 (/ color 257))))))
    (loop with width = (imago:image-width img)
	  with height = (imago:image-height img)
	  for y below height by 20 do
	    (loop for x below width by 10
		  for color10 = (get-char
				 (get-pixel x y)
				 (get-pixel (+ x 5) y)
				 (get-pixel x (+ y 10))
				 (get-pixel (+ x 5) (+ y 10)))
		  do (format t "~a" color10))
	    (format t "~%"))))

(map (make-array '(5 5 5 5)))

(defstruct char-quad
  char a b c d)

(defun count-pixels (img x1 x2 y1 y2)
  (loop for x from x1 to x2
	sum
	(loop for y from y1 to y2
	      count (equal '(1 1 1) (image:get-pixel img x y)))))

(defun get-char-quad (char img)
  (image:rect img 0 0 7 7 t 0 0 0)
  (image:text img (format nil "~a" char) 0 0 1 1 1)
  (make-char-quad
   :char char
   :a (count-pixels img 0 3 0 3)
   :b (count-pixels img 4 8 0 3)
   :c (count-pixels img 0 3 5 7)
   :d (count-pixels img 4 7 5 7)))
  
(defun get-char-quadrants ()
  (let ((img (image:make-image 10 20)))
    (loop for i from 32 to 126
	  for char = (code-char i)
	  collect (get-char-quad char img))))

(defun get-proximity (q a b c d)
  (+ (abs (- a (/ (char-quad-a q) 4)))
     (abs (- b (/ (char-quad-b q) 4)))
     (abs (- c (/ (char-quad-c q) 4)))
     (abs (- d (/ (char-quad-d q) 4)))))

(defun build-char-map (chars)
  (let ((map (make-array '(4 4 4 4) :initial-element nil)))
    (loop for a below (array-dimension map 0) do
      (loop for b below (array-dimension map 1) do
	(loop for c below (array-dimension map 2) do
	  (loop for d below (array-dimension map 3) do
	    (loop for char in chars
		  for proximity = (get-proximity char a b c d)
		  for current = (aref map a b c d)
		  when (or (null current)
			   (< proximity (car current)))
		    do (setf (aref map a b c d) (cons proximity char)))))))
    mamap p))

(defun flatten-char-map (map)
  (dotimes (a (array-dimension map 0))
    (dotimes (b (array-dimension map 1))
      (dotimes (c (array-dimension map 2))
	(dotimes (d (array-dimension map 3))
	  (setf (aref map a b c d) (char-quad-char (cdr (aref map a b c d))))))))
  map)

(defun img->ascii (path)
  (unless *chars*
    (setf *chars* (flatten-char-map (build-char-map (get-char-quadrants)))))
  (loop with img = (imago:convert-to-grayscale (imago:read-image path))
	with width = (imago:image-width img)
	with height = (imago:image-height img)
	for y below height by 20 do
	  (loop for x below width by 10
		for pixel = (imago:image-pixel img x y)
		for color = (imago:gray-intensity pixel)
		for color10 = (floor (* 10 (/ color 257)))
		do (format t "~a" color10))
	  (format t "~%")))
