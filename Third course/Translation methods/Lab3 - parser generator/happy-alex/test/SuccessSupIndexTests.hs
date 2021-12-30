module SuccessSupIndexTests where

test1 :: String
test1 = "$123^{456}$"

expected1 :: String
expected1 = "<math><msup><mo>123</mo><mn><mo>456</mo></mn></msup></math>"

test2 :: String
test2 = "$123^{abc}$"

expected2 :: String
expected2 = "<math><msup><mo>123</mo><mn><mo>abc</mo></mn></msup></math>"

test3 :: String
test3 = "$123^{+-*/=}$"

expected3 :: String
expected3 = "<math><msup><mo>123</mo><mn><mo>+-*/=</mo></mn></msup></math>"

test4 :: String
test4 = "$xyz^{123}$"

expected4 :: String
expected4 = "<math><msup><mo>xyz</mo><mn><mo>123</mo></mn></msup></math>"

test5 :: String
test5 = "$xyz^{abc}$"

expected5 :: String
expected5 = "<math><msup><mo>xyz</mo><mn><mo>abc</mo></mn></msup></math>"

test6 :: String
test6 = "$xyz^{+-*/=}$"

expected6 :: String
expected6 = "<math><msup><mo>xyz</mo><mn><mo>+-*/=</mo></mn></msup></math>"

test7 :: String
test7 = "$+-*/=^{123}$"

expected7 :: String
expected7 = "<math><msup><mo>+-*/=</mo><mn><mo>123</mo></mn></msup></math>"

test8 :: String
test8 = "$+-*/=^{abc}$"

expected8 :: String
expected8 = "<math><msup><mo>+-*/=</mo><mn><mo>abc</mo></mn></msup></math>"

test9 :: String
test9 = "$+-*/=^{+-*/=}$"

expected9 :: String
expected9 = "<math><msup><mo>+-*/=</mo><mn><mo>+-*/=</mo></mn></msup></math>"

test10 :: String
test10 = "$x^{y^{z}}$"

expected10 :: String
expected10 = "<math><msup><mo>x</mo><mn><msup><mo>y</mo><mn><mo>z</mo></mn></msup></mn></msup></math>"