module Task7 (firstExpression) where

firstExpression :: Bool
firstExpression = let mainArgument :: [([Char] -> [Char], [Char])]
                      mainArgument = [((++) "Dorian ", " Grey")] in
                        let appliableFunction :: (b -> c, b) -> c
                            appliableFunction = uncurry id in
                                let partialMap :: [(b1 -> b2, b1)] -> [b2]
                                    partialMap = map appliableFunction in
                                        let appliedMap :: [[Char]]
                                            appliedMap = partialMap mainArgument in
                                                let compositionToApply :: Foldable t => [t a] -> Bool
                                                    compositionToApply = null . head in
                                                        let result :: Bool
                                                            result = compositionToApply appliedMap in
                                                                result
