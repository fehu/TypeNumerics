# Type-level Numerics

### Interfaces

##### TypesEq

* Types equality __==__

##### TypesOrd

* Types ordering __Cmp__
* Compare types:
  * __<__
  * __<=__
  * __>__
  * __>=__

##### TypesNat

* Sum __+__
* Absolute difference __/-__
* Multiplication __*__
* Power __^__
 
##### TypesIntegral

* Simultaneous _Quot_ and _Rem_: __QuotRem__
  * Integer division truncated toward zero __Quot__
  * Integer remainder __Rem__
* Simultaneous _Div_ and _Mod_: __DivMod__
  * Integer division truncated toward negative infinity __Div__
  * Integer modulus __Mod__

##### TypeSign

* Sign of a number __Signum__
* Absolute value __Abs__
* Unary negation __Negate__
* 1, -1 or 0, corresponding to the sign __FromSign__

##### TypesRest

* Subtraction __-__

##### TypesRational

* Rational division __/__


### Implemented

##### Nat
Natural numbers, powered by GHC.TypeLits (Nat).

##### TInt
Integer numbers: positive, zero and negative.

##### PosInt
Non-zero natural numbers.

##### TRational
Rational numbers, as `TInt / PosInt`.



