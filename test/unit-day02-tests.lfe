(defmodule unit-day02-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest day02-part1
  (is-equal 1588178 (day02:part1)))

(deftest day02-part2
  (is-equal 3783758 (day02:part2)))
