(defmodule day02
  (export all))

(defun part1 () (part1 (read-input)))

(defun part1 (input) (lists:sum (-part1 input)))

(defun -part1
  (['()] '())
  ([input]
   (let* ((`#(,ls (,_ . ,input1)) (lists:splitwith #'not-x?/1 input))
          (`#(,ws (,_ . ,input2)) (lists:splitwith #'not-x?/1 input1))
          (`#(,hs (,_ . ,input3)) (lists:splitwith #'not-newline?/1 input2))
          (`(,l ,w ,h) (lists:map #'list_to_integer/1 `(,ls ,ws ,hs)))
          (sides       `(,(* l w) ,(* w h) ,(* h l)))
          (paper (+ (sum-doubles sides) (lists:min sides))))
     `(,paper . ,(-part1 input3)))))


(defun part2 () (part2 (read-input)))

(defun part2 (input) (lists:sum (-part2 input)))

(defun -part2
  (['()] '())
  ([input]
   (let* ((`#(,ls (,_ . ,input1)) (lists:splitwith #'not-x?/1 input))
          (`#(,ws (,_ . ,input2)) (lists:splitwith #'not-x?/1 input1))
          (`#(,hs (,_ . ,input3)) (lists:splitwith #'not-newline?/1 input2))
          (lengths (lists:map #'list_to_integer/1 `(,ls ,ws ,hs)))
          (paper (+ (sum-doubles (delete-max lengths))
                    (lists:foldl #'*/2 1 lengths)))
          )
     `(,paper . ,(-part2 input3)))))

(defun read-input ()
  (let ((`#(ok ,input) (file:read_file "resources/day02-input.txt")))
    (binary_to_list input)))

(defun not-x? (c) (=/= #\x c))

(defun not-newline? (c) (=/= 10 c))

(defun double (x) (* x 2))

(defun sum-doubles (lst) (lists:sum (lists:map #'double/1 lst)))

(defun delete-max (lst) (lists:delete (lists:max lst) lst))