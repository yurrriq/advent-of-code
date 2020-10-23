(defmodule day02
  (export all))

(defun part1 () (part1 (read-input)))

(defun part1 (input)
  (flet ((f
          ([`#(,l ,w ,h) sum]
           (let ((sides `(,(* l w) ,(* w h) ,(* h l))))
             (+ sum (sum-doubles sides) (lists:min sides))))))
    (lists:foldl #'f/2 0 input)))

(defun part2 () (part2 (read-input)))

(defun part2 (input)
  (flet ((f (dimensions sum)
          (let ((lengths (tuple_to_list dimensions)))
            (+ sum
               (sum-doubles (delete-max lengths))
               (lists:foldl #'*/2 1 lengths)))))
    (lists:foldl #'f/2 0 input)))

(defun read-input () (read-lines "resources/day02-input.txt"))

(defun read-lines (filename)
  (let* ((`#(ok ,file) (file:open filename '[read]))
         (lines (read-line file)))
    (file:close file)
    lines))

(defun read-line (file)
  (case (file:read_line file)
    (`#(ok ,data)
     (cons (list_to_tuple
            (lists:map #'list_to_integer/1
                       (re:split (-- data "\n") "[x]" '[#(return list)])))
           (read-line file)))
    ('eof '())))

(defun sum-doubles (lst) (lists:foldl (lambda (x sum) (+ sum (* 2 x))) 0 lst))

(defun delete-max  (lst) (lists:delete (lists:max lst) lst))
