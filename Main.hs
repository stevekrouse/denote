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
import Control.Applicative              ((<*>), (<$>), empty)
import Control.Monad.Fix                (MonadFix)
import Data.Maybe                       (isNothing)

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
    loginNav <- elClass "p" "text-xs-center" $ -- TODO get loginNav back up to toplevel
      aClass baseURL "" $ text "Already have an account?"
        
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

aClass :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => String -> String -> m () -> m (Event t String)
aClass route klass contents = do
  (el, _) <- elAttr' "a" (Map.fromList [("class", pack klass), ("href", "#")])
    contents
  pure $ route <$ domEvent Click el

-- >>> :t domEvent
-- domEvent
--   :: HasDomEvent t target eventName =>
--      EventName eventName
--      -> target -> Event t (DomEventType target eventName)
--

navbar :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => String -> Maybe Registrant -> m (Event t String)
navbar url loggedInUser = do
  elClass "nav" "navbar navbar-light" $
    elClass "div" "container" $ do
      logoNav <- aClass baseURL "navbar-brand" $ text "conduit" 
      navs <- elClass "ul" "nav navbar-nav pull-xs-right" $ do
        homeNav <- navItem baseURL $ text "Home"
        menu loggedInUser
      pure $ leftmost [ logoNav, navs ]
  where
    menu (Just Registrant {username = username}) = do
      editorNav <- navItem "denote-conduit.com/editor" $ do
        elClass "i" "ion-compose" blank
        text " "
        text "New Post"
      settingsNav <- navItem "denote-conduit.com/settings" $ do
        elClass "i" "ion-gear-a" blank
        text " "
        text "Settings"
      profileNav <- navItem ("denote-conduit.com/profile/" ++ unpack username) $ text username
      pure $ leftmost [editorNav, settingsNav, profileNav]
    menu Nothing                                 = do
      loginNav <- navItem loginURL    $ text "Sign in"
      registerNav <- navItem registerURL $ text "Sign up"
      pure $ leftmost [loginNav, registerNav]
      
    navItem :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => String -> m () -> m (Event t String)
    navItem route contents = elClass "li" "nav-item" $ do
      aClass route ("nav-link" ++ if url == route then " active" else "") contents

router :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => String -> Maybe Registrant -> [Registrant] -> m (Event t Registrant)
router url loggedInUser users
  | url == baseURL                                   = homePage loggedInUser $> never
  | url == registerURL && isNothing loggedInUser = register
  | otherwise                                                      = homePage loggedInUser $> never

browser :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Dynamic t [ Registrant ] -> m (Event t Registrant)
browser users = divClass "browser" $ mdo
  urlElem <- inputElement $ def
    & inputElementConfig_setValue .~ (pack <$> leftmost [navs, baseURL <$ newUser])
    & inputElementConfig_initialValue .~ pack registerURL
    & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList [("class","url-bar")]
  let urlB = unpack <$> _inputElement_value urlElem
  navsNested <- dyn $ navbar <$> urlB <*> loggedInUser
  navs <- switchHold never navsNested
  newUserNested <- dyn $ router <$> urlB <*> loggedInUser <*> users
  newUser <- switchHold never newUserNested
  loggedInUser <- holdDyn Nothing $ fmap Just newUser -- TODO logout
  pure newUser

baseURL :: String
baseURL = "denote-conduit.com/"

registerURL :: String
registerURL = baseURL ++ "register"

loginURL :: String
loginURL = "denote-conduit.com/login"

app :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
app = divClass "universe" $ mdo
  newUser <- browser users
  newUser1 <- browser users
  users <- foldDyn (:) [] $ leftmost [newUser, newUser1] -- TODO UUID and switch to map? & leftmost is not quite right

  dynText $ fmap (pack . show) users

  pure ()

main = mainWidgetWithCss mainCss app
