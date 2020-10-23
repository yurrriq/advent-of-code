(defmodule day01
  (export all))

(defun part1 () (part1 (read-input)))

(defun part1 (input) (lists:foldl #'move/2 0 input))

(defun part2 () (part2 (read-input)))

(defun part2 (input) (part2 0 0 input))

(defun part2
  ([-1    position _] position)
  ([_ _ '()]          'undefined)
  ([floor position `(,paren . ,rest)]
   (part2 (move paren floor) (+ position 1) rest)))

(defun move
  ([#\( floor] (+ floor 1))
  ([#\) floor] (- floor 1))
  ([_   floor] floor))

(defun read-input ()
  (let ((`#(ok ,input) (file:read_file "resources/day01-input.txt")))
    (binary_to_list input)))
