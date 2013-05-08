
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Tombot.Parser where

-- {{{ Imports
import Tombot.Types
import Tombot.Utils

import Control.Applicative
import Control.Monad

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- }}}

-- {{{ Data
data KawaiiLang = Func Text Text KawaiiLang
                | Oper Text KawaiiLang
                | Parens KawaiiLang KawaiiLang
                | Kempty
                deriving (Show)

instance Monoid KawaiiLang where
    mempty = Kempty
    mappend a b = let f (Func t0 t1 Kempty) = Func t0 t1 b
                      f (Func t0 t1 kl) = Func t0 t1 (kl `mappend` b)
                      f (Oper t Kempty) = Oper t b
                      f (Oper t kl) = Oper t (kl `mappend` b)
                      f (Parens kl0 Kempty) = Parens kl0 b
                      f (Parens kl0 kl) = Parens kl0 (kl `mappend` b)
                      f Kempty = b
                  in f a

-- | map a function that modifies the `Text' of the last `Func' in the
-- `KawaiiLang'.
kmap :: (Text -> Text) -> KawaiiLang -> KawaiiLang
kmap f (Func t0 t1 kl) = Func t0 (f t1) kl
kmap f (Parens kl0 kl1) = kLast kl0
  where
    kLast kl@(Func t0 t1 Kempty) = Func t0 (f t1) Kempty
    kLast (Func t0 t1 kl) = Func t0 t1 $ kLast kl
    kLast kl@(Oper t Kempty) = kl
    kLast (Oper t kl) = Oper t $ kLast kl
    kLast (Parens kl0 Kempty) = Parens (kLast kl0) Kempty
    kLast (Parens kl0 kl1) = Parens kl0 (kLast kl1)
    kLast x = x
kmap f _ = Kempty

-- | head of a KawaiiLang tree
khead :: KawaiiLang -> (KawaiiLang, KawaiiLang)
khead Kempty = (mempty, mempty)
khead (Func t0 t1 kl) = (Func t0 t1 mempty, kl)
khead (Oper t kl) = (Oper t mempty, kl)
khead (Parens kl0 kl1) = (Parens kl0 mempty, kl1)

{-
instance Show KawaiiLang where
    show Kempty = ""
    show (Func t0 t1 kl) = unwords [T.unpack t0, show t1, show kl]
    show (Oper t kl) = unwords [T.unpack t, show kl]
    show (Parens kl0 kl1) = "( " ++ show kl0 ++ ")" ++ " " ++ show kl1
-}

-- }}}

-- TODO
-- - A way to send `cmds' and `funcs'.

-- FIXME
-- - See TODO

-- XXX

-- {{{ Bot

botparser :: [Char] -> [Text] -> Parser KawaiiLang
botparser ps fs = skipPrefix ps >> limbs fs

-- TODO typeOf fs
-- | Match against parts of a KawaiiLang command, where Funcs, Opers and Parens
-- make up the parts.
limbs :: [Text] -> Parser KawaiiLang
limbs fs = foldr mappend Kempty <$> A.manyTill anyLimb (A.try A.endOfInput)
  where
    anyLimb = do
        noSpaces (fullCmd fs)
        <|>
        noSpaces oper
        <|>
        noSpaces (inParens fs)

-- | Ignore spaces before and after, while also making sure it's not at the end
-- of the input on the last space skipper.
noSpaces :: Parser a -> Parser a
noSpaces f = do
    ignoreSpaces
    v <- f
    ignoreSpaces
    return v

-- | Match against any of the strings in `cmds'.
cmd :: [Text] -> Parser Text
cmd cmds = A.choice (zipWith (flip ($)) cmds $ cycle [A.string])

skipPrefix :: [Char] -> Parser ()
skipPrefix cs = A.skip (`elem` cs)

oper :: Parser KawaiiLang
oper = Oper <$> A.choice strs <*> pure Kempty
  where
    strs = [ A.string "++"
           , A.string "->"
           , A.string "<-"
           , A.string "<>"
           ]

inParens :: [Text] -> Parser KawaiiLang
inParens fs = do
    (cs, op) <- "(" A..*> manyTillKeep A.anyChar end
    let cs' = T.pack cs
        kls = A.parse (limbs fs) cs'
    return $ Parens (resultToKL kls) (either (const Kempty) id op)
  where
    end = A.try $ A.string ")" >> ignoreSpaces >> (padOper <|> kawaiiEndOfInput)
    kawaiiEndOfInput = Left <$> A.endOfInput
    padOper = Right <$> oper
    resultToKL :: A.IResult Text KawaiiLang -> KawaiiLang
    resultToKL x = case x of
        A.Partial f -> resultToKL $ f ""
        A.Fail _ _ _ -> Kempty
        A.Done _ r -> r

args :: Parser (Text, KawaiiLang)
args = do
    (t, op) <- manyTillKeep A.anyChar $ A.try operOrEnd
    return (T.pack t, either (const Kempty) id op)
  where
    operOrEnd = fmap Right oper <|> fmap Left A.endOfInput

ignoreSpaces = A.skipWhile (== ' ')

-- TODO typeOf fs
-- | Match against a full function such as `"> I am (very) hungry."`.
fullCmd :: [Text] -> Parser KawaiiLang
fullCmd fs = do
    c <- cmd fs
    A.try A.skipSpace
    (m, o) <- args
    return $ Func c m o

-- XXX will be Mind Text later on
-- TODO clean up and split this function
-- | Compile `KawaiiLang' into `IO Text'.
compile :: Map Text Func -> KawaiiLang -> Mind Text
compile funcs = klToText mempty
  where
    klToText :: Text -> KawaiiLang -> Mind Text
    -- Append
    klToText old (Oper "++" kl) = klToText old kl
    -- Pipe
    klToText old (Oper "->" kl) = do
        let f = (`T.append` old)
            kl' = kmap f kl
        klToText mempty kl'
    klToText old (Oper "<>" kl) = do
        if T.null $ T.strip old
        then klToText mempty kl
        else return old
    -- Parens
    klToText old (Parens kl0 kl1) = do
        t <- klToText mempty kl0
        klToText t kl1
    -- Funcs and applicative
    klToText old (Func name args kl) = do
        let (hd, tl) = khead kl
        -- "Paren-stripper"
        if kwhen hd then do
            t <- klToText mempty tl
            klToText mempty $ Func name (T.append args t) Kempty
        -- Regular Func
        else do
            flip (maybe $ return mempty) (M.lookup name funcs) $ \f -> do
                t <- f args
                klToText (T.append old t) kl
      where
        kwhen (Oper "$$" _) = True
        kwhen _ = False
    klToText old Kempty = pure old

-- }}}

-- {{{ IRC
ircparser = A.choice [ nick, mode, quit, ircJoin, part, topic, invite, kick
                     , privmsg, notice, ping, ircError, numeric
                     ]

user = do
    A.char ':'
    nick <- A.takeWhile (/= '!')
    A.char '!'
    name <- A.takeWhile (/= '@')
    A.char '@'
    host <- A.takeWhile (/= ' ')
    A.space
    return $ (nick, name, host)

nick = do
    (nick, name, host) <- user
    A.string "NICK"
    A.space
    A.char ':'
    text <- A.takeText
    return $ Nick nick name host text

-- XXX we can clearly see a pattern here. Use your head to minimise the code.
-- - multiArg == "#chan0,#chan1,.."
--      - This applies to MODEs as well:
--        `MODE #channel +vvvv Shou Dark Aria lunar'

mode = do
    (nick, name, host) <- user
    A.string "MODE"
    A.space
    chan <- A.takeWhile (/= ' ')
    A.space
    mode <- A.takeWhile (/= ' ')
    A.space
    text <- Just <$> A.takeText
    return $ Mode nick name host chan mode text

-- XXX might be incorrect
quit = do
    (nick, name, host) <- user
    A.string "QUIT"
    A.space
    A.char ':'
    text <- A.takeText
    return $ Quit nick name host text

ircJoin = do
    (nick, name, host) <- user
    A.string "JOIN"
    A.space
    A.char ':'
    chan <- A.takeText
    return $ Join nick name host chan

part = do
    (nick, name, host) <- user
    A.string "PART"
    A.space
    chan <- A.takeWhile (/= ' ')
    A.space
    A.char ':'
    text <- A.takeText
    return $ Part nick name host chan text

topic = do
    (nick, name, host) <- user
    A.string "TOPIC"
    A.space
    chan <- A.takeWhile (/= ' ')
    A.space
    A.char ':'
    text <- A.takeText
    return $ Topic nick name host chan text

invite = do
    (nick, name, host) <- user
    A.string "INVITE"
    A.space
    rec <- A.takeWhile (/= ' ')
    A.space
    A.char ':'
    chan <- A.takeText
    return $ Invite nick name host rec chan

-- XXX might be incorrect
kick = do
    (nick, name, host) <- user
    A.string "KICK"
    A.space
    chans <- A.takeWhile (/= ' ')
    A.space
    nicks <- A.takeWhile (/= ' ')
    A.space
    A.char ':'
    text <- A.takeText
    return $ Kick nick name host chans nicks text

privmsg = do
    (nick, name, host) <- user
    A.string "PRIVMSG"
    A.space
    dest <- A.takeWhile (/= ' ')
    A.space
    A.char ':'
    text <- A.takeText
    return $ Privmsg nick name host dest text

notice = do
    (nick, name, host) <- user
    A.string "NOTICE"
    A.space
    dest <- A.takeWhile (/= ' ')
    A.space
    A.char ':'
    text <- A.takeText
    return $ Notice nick name host dest text

-- FIXME i have no idea what a KILL looks like
kill :: Parser IRC
kill = return $ Kill "" ""

-- "PING :irc.rizon.us"
ping = do
    A.string "PING"
    A.string " :"
    server <- A.takeText
    return $ Ping server

ircError :: Parser IRC
ircError = do
    A.string "ERROR"
    A.string " :"
    text <- A.takeText
    return $ Error text

numeric = do
    A.char ':'
    A.takeWhile (/= ' ')
    A.space
    num <- A.takeWhile1 (`elem` ['0' .. '9'])
    A.space
    A.takeWhile (/= ' ')
    A.skipWhile (`elem` "*=@ ")
    c <- fmap T.stripEnd <$> cmd
    text <- (A.try $ colonText) <|> pure ""
    return $ Numeric num c text
  where
    colonText = A.skipWhile (/= ':') >> A.char ':' >> A.takeText
    cmd = (fmap Just . A.try $ A.takeWhile1 (/= ':')) <|> pure Nothing
-- }}}

