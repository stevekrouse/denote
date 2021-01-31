{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Monad                    (join)
import Control.Monad.Fix                (MonadFix)
import Data.Maybe                       (isNothing, catMaybes, isJust ,fromMaybe)
import Data.Semigroup                   (First (..))
import Data.Foldable                    (find)

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

emptyRegistrant = Registrant "" "" ""

validate :: Registrant -> Bool -> Bool -> [ String ]
validate Registrant { username = username, email = email, password = password } usernameAlreadyTaken_ emailAlreadyTaken_ =
    catMaybes [
      if usernameAlreadyTaken_ then Just "username has already been taken" else Nothing,
      if emailAlreadyTaken_ then Just "email has already been taken" else Nothing,
      if length (unpack password) < 8 then Just "password is too short (minimum is 8 characters)" else Nothing,
      if null $ unpack username then Just "username can't be blank" else Nothing,
      if null $ unpack email then Just "email can't be blank" else Nothing,
      if null $ unpack password then Just "password can't be blank" else Nothing
    ]

-- >>> validate (Just emptyRegistrant) False False
-- ["password is too short (minimum is 8 characters)","username can't be blank","email can't be blank","password can't be blank"]

register :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First String) m) => (Text -> Dynamic t Bool) -> (Text -> Dynamic t Bool) -> m (Event t Registrant)
register usernameAlreadyTaken emailAlreadyTaken = divClass "auth-page" $ divClass "container page" $ divClass "row" $ divClass "col-md-6 offset-md-3 col-xs-12" $ mdo
    elClass "h1" "text-xs-center" $ text "Sign up"
    loginNav <- elClass "p" "text-xs-center" $ -- TODO get loginNav back up to toplevel
      aClass baseURL "" $ text "Already have an account?"

    elClass "ul" "error-messages" $
      simpleList errors (el "li" . dynText . fmap pack) 

    newUserSubmitted <- el "form" $ do
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

      pure $ current user <@ submitE
    
    let (someErrors,goodUser) =
          fanEither
          . pushAlways (\registrant -> do
                        nameTaken <- sample . current . usernameAlreadyTaken . username $ registrant
                        emailTaken <- sample . current . emailAlreadyTaken . email $ registrant
                        let errors = validate registrant nameTaken emailTaken
                        pure (if null errors then Right registrant else Left errors))
          $ newUserSubmitted
    errors <- holdDyn [] someErrors
    pure goodUser

    

homePage :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Maybe Registrant -> m ()
homePage loggedInUser = elClass "div" "home-page" $ mdo
  elClass "div" "banner" $
    elClass "div" "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge"

aClass :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First String) m) => String -> String -> m () -> m ()
aClass route klass contents = do
  (el, _) <- elAttr' "a" (Map.fromList [("class", pack klass), ("href", "#")])
    contents
  tellEvent $ First route <$ domEvent Click el

-- >>> :t domEvent
-- domEvent
--   :: HasDomEvent t target eventName =>
--      EventName eventName
--      -> target -> Event t (DomEventType target eventName)
--

navbar :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First String) m) => String -> Maybe Registrant -> m ()
navbar url loggedInUser = do
  elClass "nav" "navbar navbar-light" $
    elClass "div" "container" $ do
      aClass baseURL "navbar-brand" $ text "conduit"
      elClass "ul" "nav navbar-nav pull-xs-right" $ do
        navItem baseURL $ text "Home"
        menu loggedInUser
  where
    menu (Just Registrant {username = username}) = do
      navItem "denote-conduit.com/editor" $ do
        elClass "i" "ion-compose" blank
        text " "
        text "New Post"
      navItem "denote-conduit.com/settings" $ do
        elClass "i" "ion-gear-a" blank
        text " "
        text "Settings"
      navItem ("denote-conduit.com/profile/" ++ unpack username) $ text username
    menu Nothing                                 = do
      navItem loginURL    $ text "Sign in"
      navItem registerURL $ text "Sign up"

    navItem :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First String) m) => String -> m () -> m ()
    navItem route contents = elClass "li" "nav-item" $ do
      aClass route ("nav-link" ++ if url == route then " active" else "") contents

router :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First String) m) => String -> Maybe Registrant -> (Text -> Dynamic t Bool) -> (Text -> Dynamic t Bool) -> m (Event t Registrant)
router url loggedInUser usernameAlreadyTaken emailAlreadyTaken
  | url == baseURL                               = homePage loggedInUser $> never
  | url == registerURL && isNothing loggedInUser = register usernameAlreadyTaken emailAlreadyTaken
  | otherwise                                    = homePage loggedInUser $> never

browser :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => (Text -> Dynamic t Bool) -> (Text -> Dynamic t Bool) -> m (Event t Registrant)
browser usernameAlreadyTaken emailAlreadyTaken = mdo
  (newUser, urlBarUpdates) <- runEventWriterT $ mdo
    divClass "browser" $ mdo
      urlElem <- inputElement $ def
        & inputElementConfig_setValue .~ (pack . getFirst <$> urlBarUpdates)
        & inputElementConfig_initialValue .~ pack registerURL
        & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList [("class","url-bar")]
      let urlB = unpack <$> _inputElement_value urlElem
      dyn $ navbar <$> urlB <*> loggedInUser
      newUserNested <- dyn $ router <$> urlB <*> loggedInUser <*> constDyn usernameAlreadyTaken <*> constDyn emailAlreadyTaken
      newUser <- switchHold never newUserNested
      loggedInUser <- holdDyn Nothing $ fmap Just newUser -- TODO logout
      pure newUser
  pure newUser

baseURL :: String
baseURL = "denote-conduit.com/"

registerURL :: String
registerURL = baseURL ++ "register"

loginURL :: String
loginURL = "denote-conduit.com/login"

app :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
app = divClass "universe" $ mdo
  newUser <- browser usernameAlreadyTaken emailAlreadyTaken
  newUser1 <- browser usernameAlreadyTaken emailAlreadyTaken
  users <- foldDyn (:) [] $ leftmost [newUser, newUser1] -- TODO UUID and switch to map? & leftmost is not quite right

  let usernameAlreadyTaken username_ = isJust . find (== username_) . map username <$> users
  let emailAlreadyTaken email_ = isJust . find (== email_) . map email <$> users

  dynText $ fmap (pack . show) users

  pure ()

main = mainWidgetWithCss mainCss app
