
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Tombot.Funcs (funcs) where

-- {{{ Imports

import Tombot.Types
import Tombot.Utils
import Tombot.Parser

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.State

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Language.Haskell.Interpreter as H

import System.Random (randomRIO)

import Text.Regex

-- }}}


funcs :: Map Text Func
funcs = M.fromList [ (">", echo)
                   , ("del", del)
                   , ("raw", raw)
                   , ("set", set)
                   , ("sed", sed3)
                   , ("^", history)
                   , ("eval", eval)
                   , ("help", help)
                   , ("ra", random)
                   , ("wiki", wiki)
                   , ("funcs", list)
                   , ("greet", greet)
                   , ("us", userlist)
                   , ("cutify", cutify)
                   , ("reload", reload)
                   , ("britify", britify)
                   ]


{-
-- FIXME String -> Text
-- | Low level anime releases function, instead returning a list of strings.
anime' :: Text -> Mind [Text]
anime' str = do
    let (str', filters) = foldr' sepFilter ("", []) $ words str
        burl = "http://www.nyaa.eu/?page=search&cats=1_37&filter=2&term="
    html <- liftIO $ httpGetString (burl ++ urlEncode' str')
    let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "td" Nothing Nothing
        attrs = [Attr (QName "class" Nothing Nothing) "tlistname"]
        elems = findElementsAttrs qname attrs elem'
        animes = map (strip . elemsText) elems
        animes' = filter (\x -> not . or $ map (`isInfixOf` x) filters) animes
        -- FIXME make this read the first word of str and use that
        amount = 10 --if length args > 1 then (args !! 1) `safeRead` 10 else 10
    verbosity <- asks (verbosityC . getConfig)
    liftIO . when (verbosity > 1) $ do
        putStrLn $ "Filters: %(f); str': %(s)" % [("f" :: String, join ", " filters), ("s", str')]
    return $ take amount animes'

-- FIXME String -> Text
-- | Anime releases function.
anime :: Text -> Mind Text
anime str = do
    animes <- anime' str
    let animes' = colorize animes
    return $ T.intercalate ", " animes'
-}

-- | Replace words with British slang equivalents.
britify :: Func
britify str = do
    tmvar <- gets $ keepConfigTMVar
    bs <- liftIO $ do
        dir <- atomically $ confDir . fst <$> readTMVar tmvar
        ml <- readConfig $ dir <> "britify"
        return $ maybe [] id ml
    return $ wordReplace str bs

-- TODO
cutify :: Func
cutify str = do
    tmvar <- gets $ keepConfigTMVar
    bs <- liftIO $ do
        dir <- atomically $ confDir . fst <$> readTMVar tmvar
        ml <- readConfig $ dir <> "cutify"
        return $ maybe [] id ml
    return $ wordReplace str bs

-- TODO
del :: Func
del str = return ""

echo :: Func
echo = return

-- TODO get `funcs' from Config
list :: Func
list _ = return $ T.unwords $ M.keys funcs

-- TODO
eval :: Func
eval _ = return ""

-- TODO reconnect on Handle error
--      - Make a function that does this for us.
-- TODO check your privileges
glob :: Func
glob str = whenStat (>= AdminStat) $ do
    tmvar <- gets $ keepConfigTMVar
    mhs <- fmap snd $ liftIO $ atomically $ readTMVar tmvar
    void . forkMi $ forM_ (M.elems mhs) $ \h -> do
        e <- liftIO $ do
            n <- randomRIO (3, 6)
            e <- E.try $ T.hPutStrLn h str
            threadDelay (10^6 * n)
            return e
        either onerror return e
    return ""
  where
    onerror :: SomeException -> Mind ()
    onerror e = erro e

greet :: Func
greet str = liftIO $ randomRIO (0, len - 1) >>= return . (greetings !!)
  where
    greetings = [ "Hi～! :3"
                , "Heya everyone!"
                , "Hiiii!"
                , "How's it going? c:"
                , "It's me! Hi " <> str <> " :3c"
                ]
    len = length greetings

-- TODO read from file
help :: Func
help str = return ""

-- TODO filter
history :: Func
history str = do
    tmvar <- gets keepConfigTMVar
    host <- servHost <$> gets keepServ
    edest <- currDest <$> gets keepCurrent
    path <- fmap (confLogPath . fst) $ liftIO $ atomically $ readTMVar tmvar
    let dest = T.unpack $ either userNick chanName edest
        path' = path <> host <> " " <> dest
        (tn, str') = T.break (== ' ') $ T.stripStart str
        mn = readMay $ T.unpack tn :: Maybe Int
        n = maybe 1 id mn
        string = maybe str (const str') mn
    ts <- reverse . T.lines <$> liftIO (T.readFile path')
    let ts' = flip filter ts $ flip any (T.words str') . flip T.isInfixOf
        mt = ts' `atMay` n
    return $ maybe "" id mt
  where
    fil x y | T.null x = True
            | T.null y = False
            | T.head x == '-' = T.tail x /= y
            | otherwise = x == y

-- TODO
-- XXX Admin or above required
-- | `store' adds a new func to the Map and runs the contents through `eval'.
store :: Func
store str = return ""

-- | Pick a random choice or number.
random :: Func
random str
    | isDigits $ T.strip str = do
        let mi :: Maybe Integer
            mi = maybeRead $ T.unpack str
        flip (maybe $ return mempty) mi $ \i -> do
            n <- liftIO $ randomRIO (0, i)
            return . T.pack $ show n
    | otherwise = do
        let choices = T.split (== '|') str
            len = length choices
        n <- liftIO $ randomRIO (0, len - 1)
        if len > 0
            then return $ choices !! n
            else return mempty
  where
    isDigits = T.all (`elem` ['0' .. '9'])
    maybeRead = fmap fst . listToMaybe . reads

raw :: Func
raw str = whenStat (>= AdminStat) $ write str >> return ""

-- TODO what should the argument be
-- FIXME
reload :: Func
reload _ = whenStat (>= AdminStat) $ do
    confpath <- confPath . fst <$> readConfHands
    ec <- loadModules [confpath] ["Config"] "config" (H.as :: Config)
    let e = flip fmap ec $ \config -> do
        verb $ confVerbosity config
        verb $ confLogging config
        verb $ confPath config
        verb $ confModules config
        verb $ map fst $ M.toList $ confFuncs config
        mapConfHands $ first (const config)
        return ""
    either (\x -> warn x >> return "") id e
  where
    loadModules ps ms x as = liftIO $ H.runInterpreter $ do
        H.loadModules ps
        H.setTopLevelModules ms
        H.interpret x as

-- TODO Char x should only be a specific range of chars. No alphanumeric or '\'
-- | Regex replace function.
--
-- > .sed s\/apples\/Google\/i I love apples!
sed3 :: Func
sed3 str = do
    mwhen (T.length str > 1) $ do
        let c = str `T.index` 1
            m = A.maybeResult . flip A.feed "" $ A.parse (parser c) str
        flip (maybe $ pure "") m $ \x@(mat, rep, ins, str') -> do
            liftIO $ verb x
            let regex = mkRegexWithOpts mat False ins
            e <- liftIO $ E.try (pure $! subRegex regex str' rep)
            case (e :: Either E.SomeException String) of
                Right a -> pure $ T.pack a
                Left e -> pure ""
  where
    parser :: Char -> Parser (String, String, Bool, String)
    parser x = do
        A.char 's'
        A.char x
        (mat, mc) <- manyTillKeep A.anyChar $ escape x
        (rep, rc) <- manyTillKeep A.anyChar $ escape x
        ins <- (== 'i') <$> A.try (A.char 'i' <|> pure 'z')
        A.space
        str <- T.unpack <$> A.takeText
        pure (mat <> [mc], rep <> [rc], ins, str)
    escape x = A.try $ A.notChar '\\' >>= \c -> A.char x >> return c

-- TODO
-- TODO save settings to a file
-- | `set' lets the user change settings, such as the `UserStat' of a user.
set :: Func
set str = return ""

userlist :: Func
userlist _ = whenStat (>= OpStat) $ do
    edest <- gets $ currDest . keepCurrent
    users <- gets $ M.elems . servUsers . keepServ
    return $ either userNick (chanNicks users) edest
  where
    chanNicks us c = let nicks = filter (M.member (chanName c) . userChans) us
                     in T.unwords $ map userNick nicks

-- TODO
wiki :: Func
wiki str = return ""

