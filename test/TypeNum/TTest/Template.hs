
-- | Empty tests template.


undefinedSpec = describe undefined $ do

    describe "is comparable at type-level (TypesEq and TypesOrd)" $ do
        specify "==" $ example pending
        specify ">"  $ example pending
        specify "<"  $ example pending
        specify ">=" $ example pending
        specify "<=" $ example pending

    describe "has natural number operations at type-level (TypesNat)" $ do
        it "provides type-level sum '(+)'"                  $ example pending
        it "provides type-level absolute difference '(/-)'" $ example pending
        it "provides type-level multiplication '(*)'"       $ example pending
        it "provides type-level power '(^)'"                $ example pending

    describe "has integral number operations at type-level (TypesIntegral)" $ do
        it "provides type-level integer division truncated toward zero 'Quot'" $
            example pending
        it "provides type-level integer division truncated toward negative infinity 'Div'" $
            example pending

    describe "has sign operations at type-level (TypeSign)" $ do
        it "provides type-level sign"           $ example pending
        it "provides type-level absolute value" $ example pending
        it "provides type-level unary negation" $ example pending
        it "provides type-level sign to number transformation" $ example pending

    describe "has subtraction operation at type-level (TypesSubtraction)"
        it "provides type-level subtraction (-)" $ example pending

    describe "has rational number operations at type-level (TypesRational)" $
        it "provides type-level rational division (/)" $ example pending
