{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell  #-}
-- {-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Control.Lens
import Control.Arrow                    
import qualified Data.Map               as Map
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom
import Reflex.Dynamic                   (foldDyn)
import Data.Text (pack, unpack, Text)
import qualified Data.ByteString
import Data.FileEmbed
import Data.Functor                     (($>))
import Control.Applicative              ((<*>), (<$>), empty)
import Control.Monad                    (join, (>=>), liftM2)
import Control.Monad.Fix                (MonadFix, mfix)
import Data.Maybe                       (isNothing, catMaybes, isJust ,fromMaybe)
import Data.Semigroup                   (First (..))
import Data.Foldable                    (find)
import Data.Map (Map)
import qualified Data.Map as Map


app :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
app = el "button" $ pure ()

{-



### Count clicks

ghci:

```
:{
ex1 :: GUI a -> GUI (el, a)
ex1 = el' (pack "button") 

ex2 :: GUI (el, ())
ex2 = ex1 $ text (pack "+1")

ex3 :: GUI (Behavior Int)
ex3 = ex2 >>= (fst >>> domEvent Click >>> count) 

ex4 :: GUI ()
ex4 = ex3 >>= (fmap (pack . show) >>> dynText)
:}
```

Run:

```
mainWidget ex4
```

All together:

```
mainWidget $ (el' (pack "button") (text (pack "hi"))) >>= (count . domEvent Click . fst) >>= (dynText . fmap (pack . show))
```

### Count clicks mfix

(Found a bug that mfix doesn't work unless it's a Dynamic -> m Dynamic.)

## Chat

-}

ex6 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
ex6 = (>> pure ()) $
  mfix $ 
    (fmap $ pack . show)                        >>>
    dynText                                     >>>
    el' (pack "button") >=> fst                 >>> 
    domEvent Click                              >>> 
    count

ch1 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
ch1 = el (pack "button") $ text (pack "Send")

ch1' :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Element EventResult (DomBuilderSpace m) t, ())
ch1' = el' (pack "button") $ text (pack "Send")

ch2 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Event t ())
ch2 = ch1' >>= (fst >>> domEvent Click >>> pure)

ch3 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (InputElement EventResult (DomBuilderSpace m) t)
ch3 = inputElement def 

ch4 :: (DomBuilder t (Dynamic t), PostBuild t (Dynamic t), MonadHold t (Dynamic t), MonadFix (Dynamic t)) => Dynamic t Text
ch4 = ch3 >>= value

ch4' :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Dynamic t Text)
ch4' = ch3 >>= (value >>> pure)

ch4'' :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Behavior t Text)
ch4'' = ch3 >>= (value >>> current >>> pure)

ch5 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Event t Text)
ch5 = liftM2 tag ch4'' ch2

ch6 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Dynamic t [Text])
ch6 = ch5 >>= foldDyn (:) []

-- https://github.com/reflex-frp/reflex-todomvc/blob/3facdd7fc1cc585611012c6fef1cafb77a2dfa7a/src/Reflex/TodoMVC.hs#L37-L41
-- | Add a new value to a map; automatically choose an unused key
insertNew_ :: (Enum k, Ord k) => v -> Map k v -> Map k v
insertNew_ v m = case Map.maxViewWithKey m of
  Nothing -> Map.singleton (toEnum 0) v
  Just ((k, _), _) -> Map.insert (succ k) v m

ch6' :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Dynamic t (Map Int Text))
ch6' = ch5 >>= foldDyn insertNew_ Map.empty

ch7 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Dynamic t (Map Int ()))
ch7 = ch6' >>= (flip list (dynText >>> el (pack "div")))


-- tailWind :: Data.ByteString.ByteString
-- tailWind = $(embedFile "tailwind.min.css")

main = mainWidget (ch7 >> pure ())