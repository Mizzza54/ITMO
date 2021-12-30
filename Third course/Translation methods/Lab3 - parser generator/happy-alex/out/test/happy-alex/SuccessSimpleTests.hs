module SuccessSimpleTests where

test1 :: String
test1 = "$123$"

expected1 :: String
expected1 = "<math><mo>123</mo></math>"

test2 :: String
test2 = "$abc$"

expected2 :: String
expected2 = "<math><mo>abc</mo></math>"

test3 :: String
test3 = "$=+-*/$"

expected3 :: String
expected3 = "<math><mo>=+-*/</mo></math>"

test4 :: String
test4 = "$42 + x = 13 * y / z - 20$"

expected4 :: String
expected4 = "<math><mo>42</mo><mo>+</mo><mo>x</mo><mo>=</mo><mo>13</mo><mo>*</mo><mo>y</mo><mo>/</mo><mo>z</mo><mo>-</mo><mo>20</mo></math>"

test5 :: String
test5 = "$   123   456   \n 789   abc   cde$"

expected5 :: String
expected5 = "<math><mo>123</mo><mo>456</mo><mo>789</mo><mo>abc</mo><mo>cde</mo></math>"