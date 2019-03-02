module Main where

import           Data.Text (Text)
import qualified Telegram.Bot.API as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

data Model = Model
  { todoItems :: [TodoItem]
  } deriving (Show)

type TodoItem = Text

addItem :: TodoItem -> Model -> Model
addItem item model = model
  { todoItems = item : todoItems model }

data Action
  = DoNothing
  | Echo Text
  | AddItem Text
  deriving (Show)

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model []
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _model = parseUpdate
  $ AddItem <$> text
  -- (Echo <$> text)

handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
  case action of
    DoNothing -> pure model
    Echo msg -> model <# do
      replyText msg
      pure DoNothing
    AddItem title -> addItem title model <# do
      replyText "Got it."
      pure DoNothing

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
