module Main where

import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import           Control.Applicative                  ((<|>))
import           Control.Monad                        ((>=>))
import           System.Environment                   (getEnv)
import Data.Int (Int32)

import qualified Telegram.Bot.API                     as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

data Model = Model
  { todoItems :: [TodoItem]
  , allowedUsers :: [Int32]
  } deriving (Show)

type TodoItem = Text

addItem :: TodoItem -> Model -> Model
addItem item model = model
  { todoItems = item : todoItems model }

showItems :: [TodoItem] -> Text
showItems = Text.unlines

userList :: IO [Int32]
userList = (fmap read) <$> (words <$> getEnv "ALLOWED_USERS")

allowedCheck :: Model -> Telegram.User -> Bool
allowedCheck model user = elem uid (allowedUsers model)
  where uid = extractID (Telegram.userId user)
        extractID (Telegram.UserId i) = i

data Action
  = DoNothing
  | TestID
  | AddItem Text
  | ShowItems
  deriving (Show)

testID :: Model -> UpdateParser Bool
testID model =
  let user = (Telegram.updateMessage >=> Telegram.messageFrom)
    in UpdateParser $ fmap (allowedCheck model) . user

guardID :: Model -> UpdateParser Bool
guardID model = do
  t <- testID model
  if True == t
  then fail "User not allowed"
  else pure t

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate model = parseUpdate
   $  TestID    <$  (guardID model)
  <|> ShowItems <$  command "show"
  <|> AddItem   <$> text

handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
  case action of
    DoNothing -> pure model
    TestID -> model <# do
      replyText $ "YOU ARE NOT ALLOWED TO INTERACT WITH THE BOT!"
      pure DoNothing
    AddItem title -> addItem title model <# do
      replyText "Got it."
      pure DoNothing
    ShowItems -> model <# do
      replyText (showItems (todoItems model))
      pure DoNothing

inititalModel :: IO Model
inititalModel = do
  users <- userList
  pure Model {
    todoItems = []
  , allowedUsers = users
  }

initBot :: IO (BotApp Model Action)
initBot = do
  model <- inititalModel
  pure BotApp
    { botInitialModel = model
    , botAction = flip handleUpdate
    , botHandler = handleAction
    , botJobs = []
    }

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  bot <- initBot
  startBot_ (traceBotDefault bot) env

main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
