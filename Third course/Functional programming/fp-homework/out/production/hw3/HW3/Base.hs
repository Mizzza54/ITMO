module HW3.Base where

-- | function names (e.g. div, sort, length, ...)
data HiFun 
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub

-- | values (numbers, booleans, strings, ...)    
data HiValue 
  = HiValueNumber Rational
  | HiValueFunction HiFun  

-- | expressions (literals, function calls, ...)
data HiExpr 
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]

-- | evaluation errors (invalid arguments, ...)   
data HiError   
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero