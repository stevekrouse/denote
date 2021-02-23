{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}
-- {-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Control.Lens
import Control.Arrow                    
import qualified Data.Map               as Map
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom
import Data.Text (pack, unpack, Text)
import qualified Data.ByteString
import Data.FileEmbed
import Data.Functor                     (($>))
import Control.Applicative              ((<*>), (<$>), empty)
import Control.Monad                    (join)
import Control.Monad.Fix                (MonadFix, mfix)
import Data.Maybe                       (isNothing, catMaybes, isJust ,fromMaybe)
import Data.Semigroup                   (First (..))
import Data.Foldable                    (find)


app :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
app = el "button" $ pure ()

{-



### Count clicks

ghci:

```
:{
ex1 = el' (pack "button") 

ex2 = ex1 $ text (pack "+1")

ex3 = ex2 >>= (fst >>> domEvent Click >>> count) 

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

```
{:
ex5 :: String -> m ()
ex5 = pack >>> text >>> el' (pack "button")

ex6 = ex5 >>> (>> pure "hi")
:}
```

*Main Control.Arrow> :t  (pack >>> text >>> el' (pack "button") >>> (>> pure "hi")) >>> mfix

<interactive>:1:20: error:
    • No instance for (DomBuilder t0 ((->) Char))
        arising from a use of ‘el'’
    • In the first argument of ‘(>>>)’, namely ‘el' (pack "button")’
      In the second argument of ‘(>>>)’, namely
        ‘el' (pack "button") >>> (>> pure "hi")’
      In the second argument of ‘(>>>)’, namely
        ‘text >>> el' (pack "button") >>> (>> pure "hi")’
*Main Control.Arrow> :t mfix $ pack >>> text >>> el' (pack "button") >>> (>> pure "hi")
mfix $ pack >>> text >>> el' (pack "button") >>> (>> pure "hi")
  :: (DomBuilder t m, MonadFix m) => m [Char]


```
mainWidget $ (mfix $ pack >>> text >>> el' (pack "button") >>> (>> pure "hi")) >> pure ()
```

## Chat

chat1 = el' 

-}
-- ex6 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
-- ex6 = (mfix $ pack >>> text >>> el' (pack "button") >>> (>> pure "hi")) >> pure ()

ex6 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
ex6 = el "div" $ mdo
  el' (pack "button") $ text (pack t)
  t <- pure "hi"
  pure ()


main = mainWidget ex7