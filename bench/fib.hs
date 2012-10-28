
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

logMap n = 
    let s = sum . map (*3) . map (^2) $ [1..n]
        in consoleLog $ "The sum is " ++ show s ++ " in Haskell"
        

-- 
main = do
    logMap 1000
    
    logFib Hs 4
    logFib Hs 20
    logFib Hs 22
    logFib Hs 30
    logFib Hs 35
    -- 
    logFib JsRec 4
    logFib JsRec 20
    logFib JsRec 22
    logFib JsRec 30
    logFib JsRec 35
    -- 
    logFib JsIter 4
    logFib JsIter 20
    logFib JsIter 22   
    logFib JsIter 30
    logFib JsIter 35
    --