module Task4
  ( fibHS
  , sqrtHS
  , powHS
  ) where

import Task2 (HScript (..))

sqrtHS :: HScript script => Double -> script Double
sqrtHS =
  sFun1 0 $ \a res ->
  sReadVar1 a $ \eA ->
  sWithVar1 1.0e-10 $ \eps ->
  sWithVar2 0 eA $ \l r ->
    sWhile (sReadVar2 l r $ \eL eR -> eps @< eR - eL)
      ( sReadVar2 l r $ \eL eR ->
        sLetVar ((eL + eR) / 2) $ \m ->
          sIf (a @<= m * m)
            ( r @= m )
            ( l @= m )
      ) #
    sReadVar1 r $ \eR ->
      res @= eR

powHS :: HScript script => Int -> Int -> script Int
powHS =
  sFun2 1 $ \a n res ->
    sIf (n @== 0)
      ( res @= 1 )
      ( sWhile (n @> 0)
          ( sReadVar2 a n $ \eA eN ->
            sWithVar1 (eN `mod` 2 == 1) $ \isNotEven ->
              sIf (isNotEven @== True)
                ( sReadVar1 res $ \eRes ->
                    res @= eRes * eA )
                ( a @= eA ) #
              a @= eA * eA #
              n @= eN `div` 2
          )
      )

fibHS :: HScript script => Int -> script Int
fibHS =
  sFun1 1 $ \a res ->
    sIf (a @== 0)
      ( res @= 0 )
      ( sIf (a @== 1)
          ( res @= 1)
          ( sWithVar1 0 $ \s ->
              sWhile (a @> 1)
                ( sReadVar2 s res $ \eS eRes ->
                    s @= eRes #
                    res @= eS + eRes #
                    sReadVar1 a $ \eA ->
                      a @= eA - 1
                )
          )
      )
