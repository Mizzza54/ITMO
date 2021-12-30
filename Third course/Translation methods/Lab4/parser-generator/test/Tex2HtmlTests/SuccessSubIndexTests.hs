module Tex2HtmlTests.SuccessSubIndexTests where

test1 :: String
test1 = "$123_{456}$"

expected1 :: String
expected1 = "<math><mo>1</mo><mo>2</mo><msub><mo>3</mo><mn><mo>4</mo><mo>5</mo><mo>6</mo></mn></msub></math>"

test2 :: String
test2 = "$123_{abc}$"

expected2 :: String
expected2 = "<math><mo>1</mo><mo>2</mo><msub><mo>3</mo><mn><mo>a</mo><mo>b</mo><mo>c</mo></mn></msub></math>"

test3 :: String
test3 = "$123_{+-*/=}$"

expected3 :: String
expected3 = "<math><mo>1</mo><mo>2</mo><msub><mo>3</mo><mn><mo>+</mo><mo>-</mo><mo>*</mo><mo>/</mo><mo>=</mo></mn></msub></math>"

test4 :: String
test4 = "$xyz_{123}$"

expected4 :: String
expected4 = "<math><mo>x</mo><mo>y</mo><msub><mo>z</mo><mn><mo>1</mo><mo>2</mo><mo>3</mo></mn></msub></math>"

test5 :: String
test5 = "$xyz_{abc}$"

expected5 :: String
expected5 = "<math><mo>x</mo><mo>y</mo><msub><mo>z</mo><mn><mo>a</mo><mo>b</mo><mo>c</mo></mn></msub></math>"

test6 :: String
test6 = "$xyz_{+-*/=}$"

expected6 :: String
expected6 = "<math><mo>x</mo><mo>y</mo><msub><mo>z</mo><mn><mo>+</mo><mo>-</mo><mo>*</mo><mo>/</mo><mo>=</mo></mn></msub></math>"