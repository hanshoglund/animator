
{-# LANGUAGE 
    NoMonomorphismRestriction,
    TypeFamilies #-}

module Main where              

import Data.Word
import Data.String

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

-- data Event a where
--   OnLoad      :: Event (IO ())
--   OnUnload    :: Event (IO ())
--   OnChange    :: Event (IO ())
--   OnFocus     :: Event (IO ())
--   OnBlur      :: Event (IO ())
--   OnMouseMove :: Event (IO ())
--   OnMouseOver :: Event (IO ())
--   OnMouseOut  :: Event (IO ())
--   OnClick     :: Event (Int -> IO ())
--   OnDblClick  :: Event (Int -> IO ())
--   OnMouseDown :: Event (Int -> IO ())
--   OnMouseUp   :: Event (Int -> IO ())
--   OnKeyPress  :: Event (Int -> IO ())
--   OnKeyUp     :: Event (Int -> IO ())
--   OnKeyDown   :: Event (Int -> IO ())
-- 
-- instance Eq (Event a) where
--   a == b = evtName a == (evtName b :: String)
-- 
-- instance Ord (Event a) where
--   compare a b = compare (evtName a) (evtName b :: String)
-- 
-- -- | The name of a given event.
-- evtName :: IsString s => Event a -> s
-- evtName evt =
--   case evt of
--     OnLoad      -> "load"
--     OnUnload    -> "unload"
--     OnClick     -> "click"
--     OnDblClick  -> "dblclick"
--     OnMouseDown -> "mousedown"
--     OnMouseUp   -> "mouseup"
--     OnMouseMove -> "mousemove"
--     OnMouseOver -> "mouseover"
--     OnMouseOut  -> "mouseout"
--     OnKeyPress  -> "keypress"
--     OnKeyUp     -> "keyup"
--     OnKeyDown   -> "keydown"
--     OnChange    -> "change"
--     OnFocus     -> "focus"
--     OnBlur      -> "blur"
-- 
-- -- | Set a callback for the given event.
-- setCallback :: Elem -> Event a -> a -> IO Bool
-- setCallback e evt f =
--   jsSetCB e (evtName evt) (mkCallback $! f)

-- | Wrapper for window.setTimeout; execute the given computation after a delay
--   given in milliseconds.
setTimeout :: Int -> IO () -> IO ()
setTimeout delay cb =
  jsSetTimeout delay (mkCallback $! cb)



-- Tests
foreign import ccall "animator_log_prim" logBool    :: Bool   -> IO ()
foreign import ccall "animator_log_prim" logFloat   :: Float  -> IO ()
foreign import ccall "animator_log_prim" logDouble  :: Double -> IO ()
foreign import ccall "animator_log_prim" logInt     :: Int    -> IO ()
foreign import ccall "animator_log_prim" logWord    :: Word   -> IO ()
foreign import ccall "animator_log_prim" logWord32  :: Word32 -> IO ()
foreign import ccall "animator_log_prim" logString  :: JSString -> IO ()


main = do
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
    
    putLog $ "Some int   arithmetic: " ++ show ((100 `div` 3)::Int)
    putLog $ "Some int   arithmetic: " ++ show ((100 `mod` 3)::Int)
    putLog $ "Some float arithmetic: " ++ show ((100 / 3)::Double)
    putLog $ "An int overflow: " ++ show ((maxBound + 1)::Int)
    putLog $ "An int underflow: " ++ show ((minBound - 1)::Int)
    putLog $ "An word overflow: " ++ show ((maxBound + 1)::Word)
    putLog $ "An word underflow: " ++ show ((minBound - 1)::Word)

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
    
