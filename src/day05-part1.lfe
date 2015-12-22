(defmodule day05-part1
  (export all))

(defrecord state (vowels 0) (double? 'false))

(defun count-nice ()
  (lists:foldl (lambda (string count)
                 (if (nice? string)
                   (+ 1 count)
                   count))
               0 (read-lines "resources/day05-input.txt")))

(defun nice? (string)
  (-nice? string (make-state)))

(defun -nice?
  (['() (match-state vowels vowels double? double?)]
   (andalso (>= vowels 3) double?))
  ([`(,x) state]
   (-nice? '() (check-vowel x state)))
  ([`(,x ,y . ,t) state]
   (andalso (not (lists:member `(,x ,y) '["ab" "cd" "pq" "xy"]))
            (let* ((state1 (check-vowel x state))
                   (state2 (check-double x y state1)))
              (-nice? `(,y . ,t) state2)))))

(defun check-vowel
  ([c (= state (match-state vowels vowels))]
   (if (vowel? c)
     (set-state-vowels state (+ vowels 1))
     state)))

(defun check-double
  ([x y (= state (match-state double? double?))]
   (set-state-double? state (orelse double? (=:= x y)))))

(defun vowel? (c) (lists:member c "aeiou"))

(defun read-lines (filename)
  (let* ((`#(ok ,file) (file:open filename '[read]))
         (lines (read-line file)))
    (file:close file)
    lines))

(defun read-line (file)
  (case (file:read_line file)
    (`#(ok ,data)
     (cons (-- data "\n") (read-line file)))
    ('eof '())))
