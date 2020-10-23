(defmodule day03
  (export all))

(defun part1 () (part1 (read-input)))

(defun part1 (input)
  (sets:size
   (sets:from_list
    (lists:foldl
      (lambda (arrow acc) `(,(deliver arrow (car acc)) . ,acc))
      '[#(0 0)] input))))

(defun part2 () (part2 (read-input)))

(defun part2 (input)
  (sets:size (sets:from_list (-part2 '#(0 0) '#(0 0) input))))

(defun -part2
  ([santa robo-santa '()] `(,santa ,robo-santa))
  ([santa robo-santa `(,arrow ,robo-arrow . ,t) ]
   (let ((santa1      (deliver arrow santa))
         (robo-santa1 (deliver robo-arrow robo-santa)))
     `(,santa ,robo-santa . ,(-part2 santa1 robo-santa1 t)))))

(defun deliver
  ([arrow `#(,x ,y)]
   (case arrow
     (#\^ `#(,x       ,(+ y 1)))
     (#\v `#(,x       ,(- y 1)))
     (#\> `#(,(+ x 1) ,y))
     (#\< `#(,(- x 1) ,y)))))

(defun read-input ()
  (let ((`#(ok ,input) (file:read_file "resources/day03-input.txt")))
    (binary_to_list input)))
