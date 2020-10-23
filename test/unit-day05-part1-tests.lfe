(defmodule unit-day05-part1-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest ugknbfddgicrmopn
  (is (day05-part1:nice? "ugknbfddgicrmopn")))

(deftest aaa
  (is (day05-part1:nice? "aaa")))

(deftest jchzalrnumimnmhp
  (is-not (day05-part1:nice? "jchzalrnumimnmhp")))

(deftest haegwjzuvuyypxyu
  (is-not (day05-part1:nice? "haegwjzuvuyypxyu")))

(deftest dvszwmarrgswjxmb
  (is-not (day05-part1:nice? "dvszwmarrgswjxmb")))
