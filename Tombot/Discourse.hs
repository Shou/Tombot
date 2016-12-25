
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections,
             BangPatterns
#-}


module Tombot.Discourse where

import Control.BoolLike ((>&>), (<&<))
import Control.Applicative ((<$>), (<|>))
import Control.Lens ((^.), Field1(..), over)
import Control.Monad

import Data.Aeson
import Data.Aeson.Types (parse, parseMaybe, Parser)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as A
import Data.Either (rights)
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import qualified Data.Map as Map
import Data.Monoid ((<>), mempty)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Network.Wreq

import Safe

import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)


board = "https://www.newflood.net/"
user = "Shou"
key = discKey . stripSpace . unsafePerformIO $ readFile "irc/api"
  where
    discKey = Map.lookup "discourse-newflood" . read
    stripSpace = dropWhile (`elem` [' ', '\n']) . takeWhile (/= '\n')

latest = "latest.json"

topic :: (Integral int, Show int, IsString string, Monoid string)
      => int -> string
topic n = "/t/" <> (fromString $ show n) <> ".json"

posts :: (Integral int, Show int, IsString string, Monoid string)
      => int -> string
posts n = "/posts/" <> (fromString $ show n) <> ".json"

url path = board <> path <> "?api_key=" <> key <> "&api_username=" <> user


till' :: (MonadPlus m) => m a -> m b -> m ([a], b)
till' p end = mapFst ($ []) <$> scan (id, undefined)
  where scan (fp, e) = liftM (fp,) end `mplus` do !a <- p; scan (fp . (a:), e)
        mapFst f (x, y) = (f x, y)

-- | Strips arbitrary tags surrounded by @o@ and @c@ preserving any
-- text in between the opening tag and closing tag, except for
-- whatever matches @p@, which is then treated like a block element and
-- replaced by newlines, useful for e.g. <code> or [quote].
stripTags :: Char -> Char -> (Text -> Bool) -> A.Parser Text
stripTags o c p = do
    (pre, tag) <- over _1 T.pack <$> till' A.anyChar tagOpen
    (mins, mmid) <- (,) <$> maybeTags <*> fmap Just (tillTagClose tag)
    mend <- maybeTags

    let mQuote = if p tag then Nothing else Just ()

    return . T.concat $ catMaybes
           [ Just pre, mins <* mQuote
           , (mmid <* mQuote) <|> Just "\n", mend
           ]
  where
    tagOpen = do
        A.char o
        t <- A.takeWhile (`notElem` ['=', c, ' '])
        A.skipWhile (/= c)
        A.char c
        return t
    tagClose t = A.char o >> "/" *> A.string t >> A.char c
    tillTagClose t = T.pack <$> A.manyTill' A.anyChar (tagClose t)
    maybeTags = fmap Just (stripTags o c p) <|> return Nothing

stripBBCode = liftM2 (<>) (stripTags '[' ']' (== "quote")) A.takeText
-- TODO add HTML block elements to annihilate, e.g. <pre>
-- FIXME remove unary tags like <img>
stripHTML = liftM2 (<>) (stripTags '<' '>' p) A.takeText
  where
    p = flip elem ["code"]

line = A.takeWhile1 (/= '\n')

-- FIXME TODO lazy implementation
stripMarkdown :: A.Parser (Either Text Text)
stripMarkdown = do
    -- Either designates whether we keep the result or discard it, later.
    A.choice [h1, h2, hn, link, quote, unorderedList, orderedList]
  where
    h1 = fmap Right $ line <* "\n" <* A.many1 (A.char '=')
    h2 = fmap Right $ line <* "\n" <* A.many1 (A.char '-')
    hn = let hashes = A.string `map` reverse (scanl (flip T.cons) "#" "#####")
         in fmap Right $ A.choice hashes *> A.skipSpace *> line
    link = fmap Right $ "[" *> A.takeWhile1 (/= ']') <* "(" <* A.skipWhile (/= ')') <* ")"
    -- TODO parse lazy, 80col quoteblocks, for all above
    quote = fmap (Left . T.unlines) . A.many1 $ "\n" *> A.skipSpace *> line
    unorderedList = do
        let lp = "\n" *> A.skipSpace *> A.choice (map A.char ('+':"*-")) *> line
        Left . T.unlines <$> A.many1 lp
    -- TODO requires recursion: List has to start with "1." not "N."
    orderedList = do
        let lp = "\n" *> A.skipSpace  *> A.digit *> "." *> A.skipSpace *> line
        Left . T.unlines <$> A.many1 lp


-- FIXME remove use of `fromJust`
-- | Get latest Discourse thread IDs
getThreadIds :: IO (Vector Int)
getThreadIds = do
    r <- fmap (^. responseBody) . get $ url latest

    let mtids = do
            json <- decode r

            flip parseMaybe json $ \obj -> do
                (tobj :: Object) <- obj .: "topic_list"
                (tops :: Array) <- tobj .: "topics"

                let f :: Value -> Int
                    f = fromJust . parseMaybe id . withObject "" (.: "id")

                return $ V.map f tops

        tids = maybe V.empty id mtids

    return tids

getPostIds tid = do
    r <- fmap (^. responseBody) . get . url $ topic tid

    let mpids = do
            json <- decode r

            flip parseMaybe json $ \obj -> do
                (pstream :: Object) <- obj .: "post_stream"
                (stream :: Array) <- pstream .: "stream"

                let f :: Value -> Int
                    f = fromJust . parseMaybe id . parseJSON

                return $ V.map f stream

        pids = maybe V.empty id mpids

    return pids

getPost pid = do
    r <- fmap (^. responseBody) . get . url $ posts pid

    let mpost = do
            json <- decode r

            flip parseMaybe json $ \obj -> do
                (raw :: Text) <- obj .: "raw"

                return raw

        post = maybe mempty id mpost

    return post

getThread tid = do
    pids <- getPostIds tid

    V.forM pids getPost

run n = do
    rawp <- getPost n

    let eps = rights $ [A.parseOnly stripBBCode rawp]

    forM_ eps T.putStrLn


main :: IO ()
main = do
    -- XXX listToMaybe only takes the head
    margs <- fmap readMay . listToMaybe <$> getArgs

    mint <- maybe (readMay <$> getLine) return margs

    maybe warnNoArg run mint
  where
    warnNoArg = putStrLn "No argument Int provided."

