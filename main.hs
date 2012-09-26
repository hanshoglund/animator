{-# LANGUAGE 
    NoMonomorphismRestriction,
    BangPatterns,
    TypeFamilies #-}

module Main where              

import Data.Word
import Data.String

import Control.Monad.ST
import Data.Array.ST
import Data.Array.Base
import Control.Monad
import Data.Bits

import Haste.Prim
import Haste.Ajax
import Haste.DOM
import Haste.JSON

-- Basic reporting
foreign import ccall "animator_log" animator_log     :: JSString -> IO ()
foreign import ccall "animator_write" animator_write :: JSString -> IO ()
foreign import ccall "animator_alert" animator_alert :: JSString -> IO ()
foreign import ccall "animator_processing" animator_processing :: IO JSAny
foreign import ccall "animator_test_processing" animator_test_processing :: IO ()
alert str  = animator_alert (toJSStr $ str)
putLog str = animator_log (toJSStr $ str)
putDoc str = animator_write (toJSStr $ "<code>" ++ str ++ "</code><br/>")


-- Hidden reimports
newtype JSFun a = JSFun (Ptr a)
foreign import ccall jsSetCB      :: Elem -> JSString -> JSFun a -> IO Bool
foreign import ccall jsSetTimeout :: Int -> JSFun a -> IO ()

-- | Turn a computation into a callback that can be passed to a JS
--   function.
mkCallback :: a -> JSFun a
mkCallback = JSFun . toPtr




-- Tests
foreign import ccall "animator_log_prim" logBool    :: Bool   -> IO ()
foreign import ccall "animator_log_prim" logFloat   :: Float  -> IO ()
foreign import ccall "animator_log_prim" logDouble  :: Double -> IO ()
foreign import ccall "animator_log_prim" logInt     :: Int    -> IO ()
foreign import ccall "animator_log_prim" logWord    :: Word   -> IO ()
foreign import ccall "animator_log_prim" logWord32  :: Word32 -> IO ()
foreign import ccall "animator_log_prim" logString  :: JSString -> IO ()


foreign import ccall "animator_fib2" fib2  :: Double -> Double
foreign import ccall "animator_fib3" fib3  :: Double -> Double

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

data Impl = Hs | JsRec | JsIter
putFib Hs n = do
    putLog $ "Running fib " ++ show n ++ " in Haskell"  
    putLog . show $ fib  n
putFib JsRec n = do
    putLog $ "Running fib " ++ show n ++ " in JavaScript"
    putLog . show $ fib2 n
putFib JsIter n = do
    putLog $ "Running fib " ++ show n ++ " in JavaScript"
    putLog . show $ fib3 n

main = do
    putFib Hs 4
    putFib Hs 20
    putFib Hs 22

    putFib JsRec 4
    putFib JsRec 20
    putFib JsRec 22

    putFib JsIter 4
    putFib JsIter 20
    putFib JsIter 22
    -- alert "This is a warning"
    -- putLog "This goes in the log"
    -- putDoc "This goes in the doc"          

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

    -- putLog "Now"
    -- setTimeout 1000 (putLog "Soon")
    -- setTimeout 2000 (putLog "Later")
    -- putLog $ show $ nthPrime 25000
    -- putLog $ "Some int   arithmetic: " ++ show ((100 `div` 3)::Int)
    -- putLog $ "Some int   arithmetic: " ++ show ((100 `mod` 3)::Int)
    -- putLog $ "Some float arithmetic: " ++ show ((100 / 3)::Double)
    -- putLog $ "An int overflow: " ++ show ((maxBound + 1)::Int)
    -- putLog $ "An int underflow: " ++ show ((minBound - 1)::Int)
    -- putLog $ "An word overflow: " ++ show ((maxBound + 1)::Word)
    -- putLog $ "An word underflow: " ++ show ((minBound - 1)::Word)










-- main = do
--     let p = R2 (10, 20)
--     let q = negateV p
--     putLog $ show p ++ " " ++ show q
--     putLog $ show $ Set.fromList [1,2]
-- 
--     -- putDoc $ "Animator version is: " ++ show animatorVersion
--     animator_test_processing
--     putLog $ "Animator version is: " ++ show animatorVersion

    -- textRequest GET "http://localhost:5566/test.html" [] putDoc
    -- let s = mkSeed 123781267362761
    -- let [n] = randomRs (0, 10) s
    -- putLog $ show (n::Int)
    -- putLog $ "Hello Hans!"       
    -- putLog $ "2 + 2 * 10 ==> " ++ show (2 + 2 * 10)
    -- putLog $ boundsText (undefined::Int)     "Int" 
    -- putLog $ boundsText (undefined::Word8)   "Word8" 
    -- putLog $ boundsText (undefined::Word16)  "Word16" 
    -- putLog $ boundsText (undefined::Word32)  "Word32" 
    -- putLog $ boundsText (undefined::Char)    "Char" 
    
