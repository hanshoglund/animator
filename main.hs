
{-# LANGUAGE 
    NoMonomorphismRestriction,
    TypeFamilies #-}

module Main where              

import Haste.Prim
import Haste.Ajax
import Haste.DOM
import Haste.JSON

import Data.VectorSpace
import qualified Data.Set as Set

import Animator.Random
import Animator.Animation



-- Primitives from Haste compiler
{-
    Boolean (Bool)
    Number (Double, Float, Int, Int32, Word32)
    String (JSString)
    Object
    Array
    Function


data JSString
instance IsString
toJSStr :: String -> JSString
fromJSStr :: String -> JSString

class NumberRep
    ?
instance Double
instance Float
instance Int
instance Int32
instance Word
instance Word32

class Showable
    _show ::
instance Showable Double
instance Showable Float
instance Showable Int
instance Showable Int32
instance Showable Word
instance Showable Word32
instance Showable Integer
instance Showable String
instance Showable JSString
instance Showable Char
instance Showable Bool
instance Showable a => Showable (Maybe a)

data JSON
instance
encode :: JSON -> JSString
decode :: JSON -> JSString


newtype Elem = Elem JSAny
type PropID = String
type ElemID = String

addChild :: Elem -> Elem -> IO ()
addChildBefore :: Elem -> Elem -> Elem -> IO ()
getChildBefore :: Elem -> IO (Maybe Elem)
getLastChild :: Elem -> IO (Maybe Elem)
getChildren :: Elem -> IO [Elem]
setChildren :: Elem -> [Elem] -> IO ()
newElem :: String -> IO Elem
setProp :: Elem -> PropID -> String -> IO ()
getProp :: Elem -> PropID -> IO String
getStyle :: Elem -> PropID -> IO String
setStyle :: Elem -> PropID -> String -> IO ()
elemById :: ElemID -> IO (Maybe Elem)
withElem :: ElemID -> (Elem -> IO a) -> IO a
clearChildren :: Elem -> IO ()
removeChild :: Elem -> Elem -> IO ()



data Method = GET | POST deriving Show
type URL = String
type Key = String
type Val = String
textRequest :: Method -> URL -> [(Key, Val)] -> (String -> IO ()) -> IO ()
jsonRequest :: Method -> URL -> [(Key, Val)] -> (Maybe JSON -> IO ()) -> IO ()
toQueryString :: [(JSString, JSString)] -> JSString


newtype JSFun a = JSFun (Ptr a)
class Callback a where
  constCallback :: IO () -> a
instance Callback (IO ()) where
instance Callback (a -> IO ()) where
data Event
evtName :: IsString s => Event a -> s
setCallback :: Elem -> Event a -> a -> IO Bool
setTimeout :: Int -> IO () -> IO ()
-}









foreign import ccall "animator_log" animator_log :: JSString -> IO ()
foreign import ccall "animator_write" animator_write :: JSString -> IO ()
foreign import ccall "animator_alert" animator_alert :: JSString -> IO ()
foreign import ccall "animator_processing" animator_processing :: IO JSAny
foreign import ccall "animator_test_processing" animator_test_processing :: IO ()

alert :: String -> IO ()
alert str = animator_alert (toJSStr $ str)

putLog :: String -> IO ()
putLog str = animator_log (toJSStr $ str)

putDoc :: String -> IO ()
putDoc str = animator_write (toJSStr $ "<code>" ++ str ++ "</code><br/>")

processing :: IO JSAny
processing = animator_processing

main = do
    let p = R2 (10, 20)
    let q = negateV p
    putLog $ show p ++ " " ++ show q
    putLog $ show $ Set.fromList [1,2]

    -- putDoc $ "Animator version is: " ++ show animatorVersion
    -- animator_test_processing
    putLog $ "Animator version is: " ++ show animatorVersion
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
    
