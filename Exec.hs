{-# LANGUAGE RecordWildCards #-}

module Exec where

import Debug.Trace
import TypesEtc
import Sprockell
import Prelude


-- ============================================================================================
-- execution function

exec addrs instrs state@State{..} (i:is)

	| instrs!!pc==EndProg	=    []
	| otherwise		=    demoOutput addrs instrs state'    :    exec addrs instrs state' is

	where
	  state' = sprockell instrs state




-- ============================================================================================
-- generating demoOutput

demoOutput addrs instrs State{..}	 =  DemoOutput	pc
							(instrs!!pc)
							(map (regbank!!) regaddrs)
							(map (dmem!!) memaddrs)
							sp
							(map (dmem!!) [sp0+1..sp])

						where
						  (regaddrs,memaddrs) = addrs



-- ============================================================================================
-- actual demonstration, used in TestExample.hs

header = " nr: assembly instruction      || regA  regB || data memory          ||   sp: stack"
line   = "-----------------------------------------------------------------------------------"

demo addrs instrs = putStr $ unlines $ (line:) $ (header :) $ (line:) $ map show results
		where
		  results = demoOutput addrs instrs initstate : exec addrs instrs initstate clock


