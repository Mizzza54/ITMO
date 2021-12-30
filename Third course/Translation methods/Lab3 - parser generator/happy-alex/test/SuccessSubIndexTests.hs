module SuccessSubIndexTests where

test1 :: String
test1 = "$123_{456}$"

expected1 :: String
expected1 = "<math><msub><mo>123</mo><mn><mo>456</mo></mn></msub></math>"

test2 :: String
test2 = "$123_{abc}$"

expected2 :: String
expected2 = "<math><msub><mo>123</mo><mn><mo>abc</mo></mn></msub></math>"

test3 :: String
test3 = "$123_{+-*/=}$"

expected3 :: String
expected3 = "<math><msub><mo>123</mo><mn><mo>+-*/=</mo></mn></msub></math>"

test4 :: String
test4 = "$xyz_{123}$"

expected4 :: String
expected4 = "<math><msub><mo>xyz</mo><mn><mo>123</mo></mn></msub></math>"

test5 :: String
test5 = "$xyz_{abc}$"

expected5 :: String
expected5 = "<math><msub><mo>xyz</mo><mn><mo>abc</mo></mn></msub></math>"

test6 :: String
test6 = "$xyz_{+-*/=}$"

expected6 :: String
expected6 = "<math><msub><mo>xyz</mo><mn><mo>+-*/=</mo></mn></msub></math>"

test7 :: String
test7 = "$+-*/=_{123}$"

expected7 :: String
expected7 = "<math><msub><mo>+-*/=</mo><mn><mo>123</mo></mn></msub></math>"

test8 :: String
test8 = "$+-*/=_{abc}$"

expected8 :: String
expected8 = "<math><msub><mo>+-*/=</mo><mn><mo>abc</mo></mn></msub></math>"

test9 :: String
test9 = "$+-*/=_{+-*/=}$"

expected9 :: String
expected9 = "<math><msub><mo>+-*/=</mo><mn><mo>+-*/=</mo></mn></msub></math>"

test10 :: String
test10 = "$x_{y_{z}}$"

expected10 :: String
expected10 = "<math><msub><mo>x</mo><mn><msub><mo>y</mo><mn><mo>z</mo></mn></msub></mn></msub></math>"