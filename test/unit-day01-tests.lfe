(defmodule unit-day01-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest day01-part1-ex1
  (is-equal 0 (day01:part1 "(())"))
  (is-equal 0 (day01:part1 "()()")))

(deftest day01-part1-ex2
  (is-equal 3 (day01:part1 "((("))
  (is-equal 3 (day01:part1 "(()(()(")))

(deftest day01-part1-ex3
  (is-equal 3 (day01:part1 "))(((((")))

(deftest day01-part1-ex4
  (is-equal -1 (day01:part1 "())"))
  (is-equal -1 (day01:part1 "))(")))

(deftest day01-part1-ex5
  (is-equal -3 (day01:part1 ")))"))
  (is-equal -3 (day01:part1 ")())())")))

(deftest day01-part1
  (is-equal 74 (day01:part1)))

(deftest day01-part2-ex1
  (is-equal 1 (day01:part2 ")")))

(deftest day01-part2-ex2
  (is-equal 5 (day01:part2 "()())")))

(deftest day01-part2
  (is-equal 1795 (day01:part2)))
