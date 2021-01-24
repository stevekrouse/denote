{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}


module Main where

import Control.Lens
import qualified Data.Map               as Map
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom
import Data.Text (pack, unpack, Text)
import qualified Data.ByteString
import Data.FileEmbed
import           Data.Functor           (($>))
import Control.Applicative ((<*>), (<$>))
import Control.Monad.Fix (MonadFix)




mainCss :: Data.ByteString.ByteString
mainCss = $(embedFile "main.css")

-- data User = User 
--   { email :: Text
--   ,  password :: Text
--   }

data Registrant = Registrant
  { username :: Text
  , email :: Text
  , password :: Text
  } deriving (Show)

register :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Event t Registrant)
register = divClass "auth-page" $ divClass "container page" $ divClass "row" $ divClass "col-md-6 offset-md-3 col-xs-12" $ do
    elClass "h1" "text-xs-center" $ text "Sign up"
    (loginElem, _) <- elClass' "p" "text-xs-center" $
      elAttr "a" ("href" =: "/login") $ text "Already have an account?"
    
    let loginClick = domEvent Click loginElem
    
    elClass "ul" "error-messages" $
      blank -- TODO

    el "form" $ do
      usernameI <- elClass "fieldset" "form-group" $
        inputElement $ def
          & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
            [ ("class","form-control form-control-lg")
            , ("placeholder","Your name")
            ]
      emailI <- elClass "fieldset" "form-group" $
        inputElement $ def
          & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
            [ ("class","form-control form-control-lg")
            , ("placeholder","Email")
            ]
      passI <- elClass "fieldset" "form-group" $
        inputElement $ def
          & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
            [ ("class","form-control form-control-lg")
            , ("placeholder","Password")
            , ("type","password")
            ]
      (submitElem, _) <- elAttr' "button" ("class" =: "btn btn-lg btn-primary pull-xs-right" <> "type" =: "button") $ text "Sign Up"
      let submitE = domEvent Click submitElem
      let user = Registrant 
            <$> usernameI ^. to _inputElement_value 
            <*> emailI ^. to _inputElement_value 
            <*> passI ^. to _inputElement_value

      pure $ user `tagPromptlyDyn` submitE

homePage :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Maybe Registrant -> m ()
homePage loggedInUser = elClass "div" "home-page" $ mdo
  elClass "div" "banner" $
    elClass "div" "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge"


page404 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
page404 = el "div" $ text "404"

router :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => String -> Maybe Registrant -> [Registrant] -> m (Event t Registrant)
router url loggedInUser users
  | url == "denote-conduit.com/"         = homePage loggedInUser $> never
  | url == "denote-conduit.com/register" = register
  | otherwise                            = page404 $> never

browser :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Dynamic t [ Registrant ]-> m (Event t Registrant)
browser users = divClass "browser" $ mdo
  urlElem <- inputElement $ def -- todo make this full width
    & inputElementConfig_initialValue .~ "denote-conduit.com/register"
  let urlB = unpack <$> _inputElement_value urlElem
  newUserNested <- dyn $ router <$> urlB <*> loggedInUser <*> users
  newUser <- switchHold never newUserNested
  loggedInUser <- holdDyn Nothing $ fmap Just newUser -- TODO logout
  pure newUser

app :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
app = divClass "universe" $ mdo
  newUser <- browser users
  newUser1 <- browser users
  users <- foldDyn (:) [] $ leftmost [newUser, newUser1] -- TODO UUID and switch to map? & leftmost is not quite right

  dynText $ fmap (pack . show) users

  pure ()

main = mainWidgetWithCss mainCss app
