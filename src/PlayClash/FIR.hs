{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module PlayClash.FIR where

import           Clash.Explicit.Testbench
import           Clash.Prelude

createDomain vSystem{vName="SystemLow", vResetPolarity=ActiveLow}

dotp :: SaturatingNum a
     => Vec (n + 1) a
     -> Vec (n + 1) a
     -> a
dotp as bs = fold boundedAdd (zipWith boundedMul as bs)

fir
  :: (Default a, KnownNat n, SaturatingNum a, NFDataX a, HiddenClockResetEnable dom)
  => Vec (n + 1) a -> Signal dom a -> Signal dom a
fir coeffs x_t = y_t
  where
    y_t = dotp coeffs <$> bundle xs
    xs  = window x_t

topEntity ::
     Clock SystemLow
  -> Reset SystemLow
  -> Enable SystemLow
  -> Signal SystemLow (Signed 16)
  -> Signal SystemLow (Signed 16)
topEntity = exposeClockResetEnable (fir (2:>3:>(-2):>8:>Nil))


testBench :: Signal SystemLow Bool
testBench = done
  where
    testInput = stimuliGenerator clk rst (2 :> 3 :> (-2) :> 8 :> Nil)
    expectedOutput = outputVerifier' clk rst (4 :> 12 :> 1 :> 20 :> Nil)
    done = expectedOutput (topEntity clk rst enableGen testInput)
    clk = tbClockGen (not <$> done)
    rst = resetGen
