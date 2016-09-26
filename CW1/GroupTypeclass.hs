module GroupTypeclass where
    class Group a where
        neutral :: a
        negate :: a -> a
        operation :: a -> a -> a

    data SumGroup a = SumGroup { x :: a }
    instance (Num a) => Group (SumGroup a) where
        neutral = SumGroup 0
        negate a = SumGroup {x = 0 - x a}
        operation a b = SumGroup {x = x a + x b}

    data ProductGroup a = ProductGroup { y :: a }
    instance (Fractional a) => Group (ProductGroup a) where
        neutral = ProductGroup 1
        negate a = ProductGroup {y = 1 / (y a)}
        operation a b = ProductGroup {y = y a * y b}





