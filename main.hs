{-# LANGUAGE 
    NoMonomorphismRestriction,
    TypeFamilies #-}

module Main where              

import Animator.Prelude

-- -- Tests
-- foreign import ccall "animator_log_prim" logBool    :: Bool   -> IO ()
-- foreign import ccall "animator_log_prim" logFloat   :: Float  -> IO ()
-- foreign import ccall "animator_log_prim" logDouble  :: Double -> IO ()
-- foreign import ccall "animator_log_prim" logInt     :: Int    -> IO ()
-- foreign import ccall "animator_log_prim" logWord    :: Word   -> IO ()
-- foreign import ccall "animator_log_prim" logWord32  :: Word32 -> IO ()
-- foreign import ccall "animator_log_prim" logString  :: JSString -> IO ()
-- 
-- 
foreign import ccall "animator_fib2" fib2  :: Double -> Double
foreign import ccall "animator_fib3" fib3  :: Double -> Double

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

data Impl = Hs | JsRec | JsIter
logFib Hs n = do
    consoleLog $ "Running fib " ++ show n ++ " in Haskell"  
    consoleLog . show $ fib  n
logFib JsRec n = do
    consoleLog $ "Running fib " ++ show n ++ " in JavaScript"
    consoleLog . show $ fib2 n
logFib JsIter n = do
    consoleLog $ "Running fib " ++ show n ++ " in JavaScript"
    consoleLog . show $ fib3 n
-- 
main = do
    return ()
    logFib Hs 4
    logFib Hs 20
    logFib Hs 22

    logFib JsRec 4
    logFib JsRec 20
    logFib JsRec 22

    logFib JsIter 4
    logFib JsIter 20
    logFib JsIter 22   

    alert "This is a warning"
    consoleLog "This goes in the log"
    documentWrite "This goes in the doc"          

    -- logBool True
    -- logBool True
    -- logBool False
    -- logBool False
    -- logBool False
    -- logBool False
    -- logFloat (1::Float)
    -- logDouble (2::Double)
    -- logInt (3::Int)
    -- logWord (4::Word)
    -- logWord32 (5::Word32)
    -- logString (toJSStr "Hello")

    -- consoleLog "Now"
    -- setTimeout 1000 (consoleLog "Soon")
    -- setTimeout 2000 (consoleLog "Later")
    -- consoleLog $ show $ nthPrime 25000
    -- consoleLog $ "Some int   arithmetic: " ++ show ((100 `div` 3)::Int)
    -- consoleLog $ "Some int   arithmetic: " ++ show ((100 `mod` 3)::Int)
    -- consoleLog $ "Some float arithmetic: " ++ show ((100 / 3)::Double)
    -- consoleLog $ "An int overflow: " ++ show ((maxBound + 1)::Int)
    -- consoleLog $ "An int underflow: " ++ show ((minBound - 1)::Int)
    -- consoleLog $ "An word overflow: " ++ show ((maxBound + 1)::Word)
    -- consoleLog $ "An word underflow: " ++ show ((minBound - 1)::Word)










-- main = do
--     let p = R2 (10, 20)
--     let q = negateV p
--     consoleLog $ show p ++ " " ++ show q
--     consoleLog $ show $ Set.fromList [1,2]
-- 
--     -- documentWrite $ "Animator version is: " ++ show animatorVersion
--     animator_test_processing
--     consoleLog $ "Animator version is: " ++ show animatorVersion

    -- textRequest GET "http://localhost:5566/test.html" [] documentWrite
    -- let s = mkSeed 123781267362761
    -- let [n] = randomRs (0, 10) s
    -- consoleLog $ show (n::Int)
    -- consoleLog $ "Hello Hans!"       
    -- consoleLog $ "2 + 2 * 10 ==> " ++ show (2 + 2 * 10)
    -- consoleLog $ boundsText (undefined::Int)     "Int" 
    -- consoleLog $ boundsText (undefined::Word8)   "Word8" 
    -- consoleLog $ boundsText (undefined::Word16)  "Word16" 
    -- consoleLog $ boundsText (undefined::Word32)  "Word32" 
    -- consoleLog $ boundsText (undefined::Char)    "Char" 
    
