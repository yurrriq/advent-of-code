(defmodule unit-day03-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest day03-part1
  (is-equal 2572 (day03:part1)))

(deftest day03-part2
  (is-equal 2631 (day03:part2)))
