
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Tombot.Funcs (funcs) where

-- {{{ Imports

import Tombot.Types
import Tombot.Utils

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
                   , ("sed", sed3)
                   , ("ra", random)
                   , ("us", userlist)
                   , ("reload", reload)
                   , ("funcs", list)
                   , ("help", help)
                   , ("^", history)
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

echo :: Func
echo = return

list :: Func
list _ = return $ T.intercalate ", " $ M.keys funcs

-- TODO reconnect on Handle error
-- TODO check your privileges
glob :: Func
glob str = do
    tmvar <- gets $ keepConfigTMVar
    mhs <- fmap snd $ liftIO $ atomically $ readTMVar tmvar
    forM_ (M.elems mhs) $ \h -> do
        e <- liftIO $ do
            n <- randomRIO (3, 6)
            threadDelay (10^6 * n)
            E.try $ T.hPutStrLn h str
        either onerror return e
    return ""
  where
    onerror :: SomeException -> Mind ()
    onerror e = erro e

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
    ts <- reverse . T.lines <$> liftIO (T.readFile path)
    let ts' = flip filter ts $ flip any (T.words str) . flip T.isInfixOf
        mt = ts' `atMay` n
    return $ maybe "" id mt

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

-- TODO check your privileges
raw :: Func
raw str = write str >> return ""

-- TODO what should the argument be
reload :: Func
reload _ = do
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
-- > .sed s\/apple\/banana\/i I love Apple!
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

userlist :: Func
userlist _ = gets $ T.unwords . M.keys . currUsers . keepCurrent

