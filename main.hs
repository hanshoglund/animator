
{-# LANGUAGE 
    NoMonomorphismRestriction #-}

module Main where              

import Animator.Animation

import qualified Data.Ord
import qualified Data.Eq    
import qualified Data.Monoid
-- import Data.Functor
import qualified Control.Monad
-- import Control.Monad.Zip
import qualified Control.Applicative
import qualified Control.Arrow
import qualified Data.Foldable
import qualified Data.Traversable

-- import qualified Data.Binary
-- import qualified Control.Exception
-- import qualified Control.Concurrent
-- import qualified Control.Parallel
-- import qualified Control.DeepSeq

import qualified Data.Maybe
import qualified Data.Either
import qualified Data.Function
-- import qualified Data.Fixed

-- import qualified Data.Int  
import Data.Word
import qualified Data.Char
import qualified Data.Ratio
import qualified Data.Complex
import qualified Data.String
-- import qualified Data.Text
-- import qualified Data.Version
-- import qualified Data.Time
-- import qualified Data.Unique
-- import qualified Data.ByteString

-- import qualified Data.Tuple
import qualified Data.List
-- import qualified Data.Sequence
-- import qualified Data.Set
-- import qualified Data.Map
-- import qualified Data.Tree
-- import qualified Data.Graph   

-- import Data.Functor.Compose
-- import Data.Functor.Constant
-- import Data.Functor.Identity
-- import Data.Functor.Product
-- import Data.Functor.Reverse
-- import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Cont
-- import Control.Monad.Trans.Error
-- import Control.Monad.Trans.Identity
-- import Control.Monad.Trans.List
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.Writer
-- import Control.Monad.Trans.State

-- import qualified System.FilePath
-- import qualified System.Directory
-- import qualified System.Environment
-- import qualified System.Exit
-- import qualified System.Process
-- import qualified System.Time
-- import qualified System.TimeOut
-- import qualified System.Random
-- import qualified System.IO
-- import qualified System.Mem
-- import qualified System.Console.GetOpt

import qualified Unsafe.Coerce
import Haste.Prim

-- TODO containers
-- TODO semigroups
-- TODO semigroupoids
-- TODO comonad




bounds :: Bounded a => a -> (a, a)
bounds x = (minBound, maxBound)

boundsText :: (Bounded a, Show a) => a -> String -> String
boundsText x name = let (m, n) = bounds x
    in name ++ " x => " ++ show m ++ " < x < " ++ show n


type Document = JSAny

foreign import ccall "objWrite" write :: Document -> JSString -> IO ()
foreign import ccall "getDocument" getDocument :: IO Document


putStrLn2 :: String -> IO ()
putStrLn2 str = do
    d <- getDocument
    write d (toJSStr $ "<code>" ++ str ++ "</code><br/>\n")



main = do   
    putStrLn2 $ show (1::Integer) 
    putStrLn2 $ "Hello Hans!"       
    putStrLn2 $ "2 + 2 * 10 ==> " ++ show (2 + 2 * 10)
    putStrLn2 $ boundsText (undefined::Int)     "Int" 
    putStrLn2 $ boundsText (undefined::Word8)   "Word8" 
    putStrLn2 $ boundsText (undefined::Word16)  "Word16" 
    putStrLn2 $ boundsText (undefined::Word32)  "Word32" 
    putStrLn2 $ boundsText (undefined::Char)    "Char" 
    putStrLn2 $ "Animator version is: " ++ show animatorVersion
    -- putStrLn $ "Hello " ++ ys ++ "!"
    -- putStrLn $ "The sum from Hs is: " ++ (show $ sum $ map (\x -> x^2) [0..1000])
    
