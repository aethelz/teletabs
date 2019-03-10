module Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Char                      ( intToDigit )
import           Data.Maybe                     ( fromJust, isJust )
import           Data.List                      ( delete )
import           Text.Read                      ( readMaybe )
import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( (>=>) )
import           System.Environment             ( getEnv )
import           Data.Int                       ( Int32 )

import qualified Telegram.Bot.API              as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser


data Model = Model
  { todoItems    :: [TodoItem]
  , allowedUsers :: [Int32]
  , convos       :: [Telegram.ChatId]
  } deriving (Show)

type TodoItem = Text

addConvo :: Telegram.ChatId -> Model -> Model
addConvo chatId model = model { convos = chatId : convos model }

addItem :: TodoItem -> Model -> Model
addItem item model = model { todoItems = todoItems model ++ [item] }

clearItems :: Model -> Model
clearItems model = model { todoItems = [] }

removeItem :: Text -> Model -> Either Text Model
removeItem input model
  | idx == Nothing              = Left "Not a Number"
  | 1 > fromJust idx            = Left "Incorrect Number"
  | length items < fromJust idx = Left "No such task"
  | otherwise = Right model { todoItems = delItem (fromJust idx - 1) items }
 where
  items = todoItems model
  idx   = (readMaybe . Text.unpack) input
  delItem im xs = delete (xs !! im) xs

showItems :: [TodoItem] -> Text
showItems = Text.unlines . zipWith (\a b -> intToText a <> ". " <> b) [1 ..]
  where intToText = Text.singleton . intToDigit

userList :: IO [Int32]
userList = (fmap read) <$> (words <$> getEnv "ALLOWED_USERS")

allowedCheck :: Model -> Telegram.User -> Bool
allowedCheck model user = elem uid (allowedUsers model)
 where
  uid = extractID (Telegram.userId user)
  extractID (Telegram.UserId i) = i

data Action
  = DoNothing
  | TestID
  | InitConvo
  | AddItem TodoItem
  | AddConvo Telegram.ChatId
  | RemoveItem TodoItem
  | ClearItems
  | NotifyOthers Telegram.ChatId
  | ShowItems
  deriving (Show)

guardID :: Model -> UpdateParser Bool
guardID model = do
  t <- testID model
  if True == t then fail "User not allowed" else pure t
 where
  testID model' = UpdateParser $ fmap (allowedCheck model') . user
  user = (Telegram.updateMessage >=> Telegram.messageFrom)

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate model =
  parseUpdate
    $  TestID     <$  guardID model
   <|> InitConvo  <$  command "start"
   <|> ShowItems  <$  command "show"
   <|> RemoveItem <$> command "rm"
   <|> ClearItems <$  command "clear"
   <|> AddItem    <$> text

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  DoNothing -> pure model
  TestID    -> model <# do
    replyText $ "YOU ARE NOT ALLOWED TO INTERACT WITH THE BOT!"
    pure DoNothing
  InitConvo -> model <# do
    yourId <- currentChatId
    if isJust yourId
      then pure $ AddConvo $ fromJust yourId
      else pure DoNothing
  AddItem title -> addItem title model <# do
    replyText "Item added"
    yourId <- currentChatId
    if isJust yourId
      then pure $ NotifyOthers $ fromJust yourId
      else pure DoNothing
  NotifyOthers current -> model <# do
    mapM_ (flip replyTo "List was updated")
      $ map Telegram.SomeChatId
      $ filter (/= current)
      $ convos model
    pure DoNothing
  RemoveItem i -> case removeItem i model of
    Left err -> model <# do
      replyText err
      pure DoNothing
    Right newModel -> newModel <# do
      replyText "Item removed"
      pure ShowItems
  ClearItems -> clearItems model <# do
    replyText "List Cleared!"
    pure DoNothing
  AddConvo convoId -> addConvo convoId model <# do
      replyText $ "Your ID added"
      pure DoNothing
  ShowItems -> model <# do
    let todoList = todoItems model
    if null todoList
      then replyText "No items to show!"
      else replyText (showItems todoList)
    pure DoNothing

inititalModel :: IO Model
inititalModel = do
  users <- userList
  pure Model
    { todoItems    = []
    , allowedUsers = users
    , convos       = []
    }

initBot :: IO (BotApp Model Action)
initBot = do
  model <- inititalModel
  pure BotApp
    { botInitialModel = model
    , botAction       = flip handleUpdate
    , botHandler      = handleAction
    , botJobs         = []
    }

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  bot <- initBot
  startBot_ (traceBotDefault bot) env

main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
