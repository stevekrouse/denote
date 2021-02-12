{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Control.Lens
import Control.Arrow                    ((>>>))
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
import Control.Monad.Fix                (MonadFix)
import Data.Maybe                       (isNothing, catMaybes, isJust ,fromMaybe)
import Data.Semigroup                   (First (..))
import Data.Foldable                    (find)


app :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
app = el "button" $ pure ()

{-

:{

ex1 = el' (pack "button") 

ex2 = ex1 $ text (pack "+1")

ex3 = ex2 >>= (fst >>> domEvent Click >>> count) 

ex4 = ex3 >>= (fmap (pack . show) >>> dynText)

:}

mainWidget ex4


-}


main = mainWidget app