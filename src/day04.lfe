(defmodule day04
  (export all))

(defun part1 () (part1 (input)))

(defun part1 (input) (-part1 input 0))

(defun -part1 (salt n)
  (let ((digest (md5 (++ salt (integer_to_list n)))))
    (if (lists:prefix "00000" digest)
      n
      (-part1 salt (+ n 1)))))

(defun part2 () (part2 (input)))

(defun part2 (input) (-part2 input 0))

(defun -part2 (salt n)
  (let ((digest (md5 (++ salt (integer_to_list n)))))
    (if (lists:prefix "000000" digest)
      n
      (-part2 salt (+ n 1)))))

(defun md5 (string)
  (lists:flatten
   (lc ((<= (b) (erlang:md5 string)))
     (io_lib:format "~2.16.0b" `(,b)))))

(defun input () "iwrupvqb")
