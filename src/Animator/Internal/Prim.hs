
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

-- | Primitive imports form the Host environment.
module Animator.Internal.Prim
(
JsString,
JsObject,
JsArray,
HJ.JSON
)
where

import qualified Haste.Prim as H
import qualified Haste.JSON as HJ

newtype JsString = JsString { getJSString :: H.JSString }
newtype JsObject = JsObject { getJSObject :: H.JSAny }
newtype JsArray  = JsArray { getJSArray :: H.JSAny }