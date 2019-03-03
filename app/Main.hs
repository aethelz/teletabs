module Main where

import           Data.Text                            (Text, pack)
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
  , allowedUsers :: [Telegram.User]
  } deriving (Show)

type TodoItem = Text

addItem :: TodoItem -> Model -> Model
addItem item model = model
  { todoItems = item : todoItems model }

showItems :: [TodoItem] -> Text
showItems = Text.unlines

userList :: IO [Int32]
userList = (fmap read) <$> (words <$> getEnv "ALLOWED_USERS")

allowedCheck :: Telegram.User -> Bool
allowedCheck user = flip elem [00000000, 99999999] uid
-- allowedCheck user = elem uid <$> userList
  where uid = extractID (Telegram.userId user)
        extractID (Telegram.UserId i) = i

data Action
  = DoNothing
  | FailID
  | AddItem Text
  | ShowItems
  deriving (Show)

testID :: UpdateParser Bool
testID =
  let user = (Telegram.updateMessage >=> Telegram.messageFrom)
    in UpdateParser $ fmap allowedCheck . user

guardID :: UpdateParser Bool
guardID = do
  t <- testID
  if True == t
  then fail "User not allowed"
  else pure t

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model [] []
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _model = parseUpdate
   $  FailID    <$  guardID
  <|> ShowItems <$  command "show"
  <|> AddItem   <$> text

handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
  case action of
    DoNothing -> pure model
    FailID -> model <# do
      replyText $ "YOU ARE NOT ALLOWED TO INTERACT WITH THE BOT!"
      pure DoNothing
    AddItem title -> addItem title model <# do
      replyText "Got it."
      pure DoNothing
    ShowItems -> model <# do
      replyText (showItems (todoItems model))
      pure DoNothing

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
