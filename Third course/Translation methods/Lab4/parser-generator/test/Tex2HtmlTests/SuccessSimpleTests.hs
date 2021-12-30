module Tex2HtmlTests.SuccessSimpleTests where

test1 :: String
test1 = "$123$"

expected1 :: String
expected1 = "<math><mo>1</mo><mo>2</mo><mo>3</mo></math>"

test2 :: String
test2 = "$abc$"

expected2 :: String
expected2 = "<math><mo>a</mo><mo>b</mo><mo>c</mo></math>"

test3 :: String
test3 = "$=+-*/$"

expected3 :: String
expected3 = "<math><mo>=</mo><mo>+</mo><mo>-</mo><mo>*</mo><mo>/</mo></math>"

test4 :: String
test4 = "$42 + x = 13 * y / z - 20$"

expected4 :: String
expected4 = "<math><mo>4</mo><mo>2</mo><mo>+</mo><mo>x</mo><mo>=</mo><mo>1</mo><mo>3</mo><mo>*</mo><mo>y</mo><mo>/</mo><mo>z</mo><mo>-</mo><mo>2</mo><mo>0</mo></math>"

test5 :: String
test5 = "$   123   456   \n 789   abc   cde$"

expected5 :: String
expected5 = "<math><mo>1</mo><mo>2</mo><mo>3</mo><mo>4</mo><mo>5</mo><mo>6</mo><mo>7</mo><mo>8</mo><mo>9</mo><mo>a</mo><mo>b</mo><mo>c</mo><mo>c</mo><mo>d</mo><mo>e</mo></math>"