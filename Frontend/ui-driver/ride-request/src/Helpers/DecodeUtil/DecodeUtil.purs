module DecodeUtil where

import Prelude
import Data.Function.Uncurried
import Data.Maybe (Maybe(..), maybe)
import Foreign.Generic (class Decode, Foreign, decode)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)

foreign import getFromWindow :: Fn3 String (Maybe Foreign) (Foreign -> (Maybe Foreign)) (Maybe Foreign)

foreign import getAnyFromWindow :: forall a. Fn3 String (Maybe a) (a -> (Maybe a)) (Maybe a)

foreign import getFromWindowString :: Fn3 String (Maybe String) (String -> (Maybe String)) (Maybe String)

foreign import setInWindow :: Fn2 String String String

foreign import setAnyInWindow :: forall a. Fn2 String a a

-- JSON Utils
foreign import parseJSON :: forall a. a -> Foreign

foreign import stringifyJSON :: forall a. Fn1 a String

decodeForeignObject :: forall a. Decode a => Foreign -> a -> a
decodeForeignObject object defaultObject = maybe (defaultObject) identity $ decodeForeignObjImpl object

decodeForeignObjImpl :: forall a. Decode a => Foreign -> Maybe a
decodeForeignObjImpl = hush <<< runExcept <<< decode

-- NOTE:: Uncomment this to debug the error case
-- decodeForeignObject :: forall a. Decode a => Foreign -> a -> a
-- decodeForeignObject object defaultObject = maybe (defaultObject) identity $ decodeForeignObjImpl object defaultObject
-- decodeForeignObjImpl object defaultObject =
-- case runExcept $ decode object of
--   Right decodedObj -> Just decodedObj
--   Left err1 -> do
--     let
--       _ = spy "Not able to decode object" $ "Fallbacks to default object for missing Keys" <> (show err1)
--     Just defaultObject
