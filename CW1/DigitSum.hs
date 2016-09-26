module DigitSum where
    digitSum :: (Integral a) => a -> a
    digitSum a = if a == 0 then 0 else (mod a 10) + digitSum (div a 10)