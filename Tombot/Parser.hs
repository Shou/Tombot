
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, TupleSections,
             ScopedTypeVariables, StandaloneDeriving, TypeApplications,
             PartialTypeSignatures
#-}

module Tombot.Parser (
  botparse
, botparser
, ircparser
, compile
, trySpaced
) where

-- {{{ Imports
import Tombot.Types
import Tombot.IRC.Types
import Tombot.Utils

import Control.Applicative
import Control.Error
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.CaseInsensitive as CI
import Data.Char as Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Debug.Trace

-- }}}

-- {{{ Data
data KawaiiLang = Func Text Text KawaiiLang
                | Oper Text KawaiiLang
                | Parens KawaiiLang KawaiiLang
                | Kempty
                deriving (Eq, Read, Show)

instance Monoid KawaiiLang where
    mempty = Kempty
    mappend a b = let f (Func t0 t1 Kempty) = Func t0 t1 b
                      f (Func t0 t1 kl) = Func t0 t1 (kl <> b)
                      f (Oper t Kempty) = Oper t b
                      f (Oper t kl) = Oper t (kl <> b)
                      f (Parens kl0 Kempty) = Parens kl0 b
                      f (Parens kl0 kl) = Parens kl0 (kl <> b)
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

-- }}}

-- TODO
-- - Operator escaping functions.
--      - Should closing parens be escaped too?
--      - How can we use these together with other functions?
--          - : about $$ funcs -> sed s/ /|/ -> ra -> eval
-- - Instead of operating escaping, we just have functions that take everything
--   on the right as parameters, irregardless of what it is (op, func, ...)
--      - Only within parens if present

-- FIXME

-- XXX

-- {{{ New hot parser

-- | Syntax
data Syntax = Let LHS
            | Fun Text Text
            | Opr Text

deriving instance Show Syntax


skipSpaces = Atto.skipWhile (== ' ')

skipSpaces1 = Atto.space >> skipSpaces >> pure ()

around wrap parser = wrap *> parser <* wrap


hotParser = Atto.many' parserSyntax

parserSyntax :: Atto.Parser _
parserSyntax = Atto.choice $ map (around skipSpaces)
             [ parserLet
             , parserOpr
             , parserFun
             ]

data LHS = LHSConstant Text
         | LHSFunction Text [Text]
         | LHSOperator Text Text Text
         deriving (Show)

parserConstLHS :: Parser LHS
parserConstLHS = LHSConstant <$> Atto.takeWhile1 Char.isAlphaNum

parserFuncLHS :: Parser LHS
parserFuncLHS = do
    name <- Atto.takeWhile Char.isAlphaNum
    vars <- Atto.many1' $ skipSpaces1 >> Atto.takeWhile1 Char.isAlphaNum

    return $ LHSFunction name vars

parserOperLHS :: Parser LHS
parserOperLHS = do
    leftVar <- Atto.takeWhile Char.isAlphaNum
    skipSpaces1
    operator <- Atto.takeWhile isOperator
    skipSpaces1
    rightVar <- Atto.takeWhile Char.isAlphaNum

    return $ LHSOperator leftVar operator rightVar

parserLet :: Atto.Parser _
parserLet = do
    Atto.string "let"
    skipSpaces1

    a <- Atto.choice [parserOperLHS, parserFuncLHS, parserConstLHS]

    skipSpaces1
    Atto.string "="
    skipSpaces1

    return $ Let a

parserFun = do
    name <- Atto.takeWhile1 Char.isAlphaNum

    return $ Fun name ""

isOperator :: Char -> Bool
isOperator c = or @[] [Char.isSymbol c, Char.isPunctuation c]

parserOpr = Opr <$> Atto.takeWhile1 isOperator


-- | Operator name, fixity, precedence
data Operator = Operator Text (Either () ()) Int

-- }}}

-- {{{ Bot

botparser :: [Char] -> [Text] -> Parser KawaiiLang
botparser ps fs = skipPrefix ps >> limbs fs

-- | Match against parts of a KawaiiLang command, where Funcs, Opers and Parens
-- make up the parts.
limbs :: [Text] -> Parser KawaiiLang
limbs fs = foldr mappend Kempty <$> Atto.manyTill anyLimb (Atto.try Atto.endOfInput)
  where
    anyLimb = do
        trySpaced (fullCmd fs)
        <|>
        trySpaced oper
        <|>
        trySpaced (inParens fs)

-- | Ignore spaces before and after, while also making sure it's not at the end
-- of the input on the last space skipper.
trySpaced :: Parser a -> Parser a
trySpaced f = do
    Atto.try Atto.skipSpace
    v <- f
    Atto.try Atto.skipSpace
    return v

-- | Match against any of the strings in `cmds'.
cmd :: [Text] -> Parser Text
cmd cmds = Atto.choice (zipWith (flip ($)) scmds $ cycle [fcmd])
  where
    -- sort by length so longer function names are prioritised
    scmds = sortBy (comparing $ (* (-1)) . Text.length) cmds
    fcmd x = do
        c <- Atto.asciiCI x
        if Text.all isLetter c then Atto.skipSpace else Atto.try Atto.skipSpace
        return $ Text.toLower c

skipPrefix :: [Char] -> Parser ()
skipPrefix cs = Atto.skip (`elem` cs)

oper :: Parser KawaiiLang
oper = Oper <$> Atto.choice strs <*> pure Kempty
  where
    strs = [ Atto.string "++"
           , Atto.string "->"
           , Atto.string "<-"
           , Atto.string "<>"
           , Atto.string "><"
           , Atto.string ">>"
           , Atto.string "+>"
           , Atto.string "=="
           , Atto.string "/="
           , Atto.string "=>"
           , Atto.string "/>"
           ]

inParens :: [Text] -> Parser KawaiiLang
inParens fs = do
    (cs, op) <- "(" *> manyTillKeep Atto.anyChar end
    let cs' = Text.pack cs
        kls = Atto.parse (limbs fs) cs'
    return $ Parens (resultToKL kls) (either (const Kempty) id op)
  where
    end = Atto.try $ Atto.string ")" >> ignoreSpaces >> (padOper <|> kawaiiEndOfInput)
    kawaiiEndOfInput = Left <$> Atto.endOfInput
    padOper = Right <$> oper
    resultToKL :: Atto.IResult Text KawaiiLang -> KawaiiLang
    resultToKL x = case x of
        Atto.Partial f -> resultToKL $ f ""
        Atto.Fail _ _ _ -> Kempty
        Atto.Done _ r -> r

args :: Parser (Text, KawaiiLang)
args = do
    (t, op) <- manyTillKeep Atto.anyChar $ Atto.try operOrEnd
    return (Text.pack t, either (const Kempty) id op)
  where
    operOrEnd = fmap Right oper <|> fmap Left Atto.endOfInput

ignoreSpaces = Atto.skipWhile (== ' ')

-- | Match against a full function such as `"> I am (very) hungry."`.
fullCmd :: [Text] -> Parser KawaiiLang
fullCmd fs = do
    c <- cmd fs
    (m, o) <- if c `elem` lefts
              then (, Kempty) <$> Atto.takeText
              else args
    return $ Func c m o
  where
    lefts :: [Text]
    lefts = ["eval", "event", "help", "let", "on", "re"]

-- FIXME check Funk Stat
-- TODO clean up and split this function
-- | Compile `KawaiiLang' into `IO Text'.
compile :: forall s. Map Text (Funk s) -> KawaiiLang -> Mind s Text
compile funcs = funky . klToText mempty
  where
    klToText :: Text -> KawaiiLang -> Funky s Text
    -- Append
    klToText old (Oper "++" kl) = klToText old kl
    -- Pipe
    klToText old (Oper "->" kl) = klToText mempty $ kmap (<> old) kl
    -- Or
    klToText old (Oper "<>" kl) = do
        if Text.null $ Text.strip old
        then klToText mempty kl
        else return old
    -- And right
    klToText old (Oper "><" kl) = do
        if Text.null $ Text.strip old
        then return mempty
        else klToText mempty kl >>= \t -> if Text.null $ Text.strip t
                                          then return mempty
                                          else return t
    -- And
    klToText old (Oper "+>" kl) = do
        if Text.null $ Text.strip old
        then return mempty
        else klToText mempty kl >>= \t -> if Text.null $ Text.strip t
                                          then return mempty
                                          else return $ old <> t
    -- Bind
    klToText old (Oper ">>" kl) = klToText mempty kl
    -- Equal
    klToText old (Oper "==" kl) = do
        t <- klToText mempty kl
        if old == t then return "True" else return ""
    -- Not equal
    klToText old (Oper "/=" kl) = do
        t <- klToText mempty kl
        if old /= t then return "True" else return ""
    -- Equal, return right
    klToText old (Oper "=>" kl) = do
        t <- klToText mempty kl
        if old == t then return t else return ""
    -- Not equal, return right
    klToText old (Oper "/>" kl) = do
        t <- klToText mempty kl
        if old /= t then return t else return ""
    -- Parens
    klToText old (Parens kl0 kl1) = klToText mempty kl0 >>= flip klToText kl1
    -- Funcs and applicative
    klToText old (Func name args kl) = do
        let (hd, tl) = khead kl
        -- "Paren-stripper"
        if kwhen hd then do
            t <- klToText mempty tl
            klToText old $ Func name (args <> t) Kempty
        -- Regular Func
        else do
            flip (maybe $ return mempty) (Map.lookup name funcs) $ \f -> do
                fu <- get
                if stFunkRecs fu < stFunkMax fu then do
                    t <- lift $ mwhenPrivTrans (funkStat f) $ do
                        funkFunc f args
                    put $ StFunk (stFunkRecs fu + 1) (stFunkMax fu)
                    klToText (old <> t) kl
                else return "pls no recursive"
      where
        kwhen (Oper "<-" _) = True
        kwhen _ = False
    klToText old Kempty = pure old

-- |
botparse :: Map Text (Funk s) -> Text -> Mind s KawaiiLang
botparse funcs t = fmap (either id id) . decide $ do
    d <- either _userId _chanId . _currDestination <$> lift see
    chans <- _servChannels . _currServer <$> lift see

    let mchan = Map.lookup d chans

    unless (isJust mchan) $ lift (warn $ "No channel " <> d) >> throwError Kempty

    let chan = fromJust mchan
        parser = botparser (_chanPrefix chan) (Map.keys funcs)
        mkl = Atto.maybeResult . flip Atto.feed "" $ Atto.parse parser t

    return $ maybe Kempty id mkl

-- }}}

-- {{{ IRC
ircparser = Atto.choice [ nick, mode, quit, ircJoin, part, topic, invite, kick
                     , privmsg, notice, ping, ircError, numeric, cap
                     ]

user = do
    Atto.char ':'
    nick <- Atto.takeWhile (/= '!')
    Atto.char '!'
    name <- Atto.takeWhile (/= '@')
    Atto.char '@'
    host <- Atto.takeWhile (/= ' ')
    Atto.space
    return $ (nick, name, host)

network = do
    Atto.char ':'
    server <- Atto.takeWhile (/= ' ')
    Atto.space
    return (server, server, server)

-- XXX test this
nick = do
    (nick, name, host) <- user
    Atto.string "NICK"
    Atto.space
    Atto.try $ Atto.char ':'
    text <- Atto.takeText
    return $ Nick nick name host text

-- XXX we can clearly see a pattern here. Use your head to minimise the code.
-- - multiArg == "#chan0,#chan1,.."
--      - This applies to MODEs as well:
--        `MODE #channel +vvvv Shou Dark Aria lunar'

mode = do
    (nick, name, host) <- user <|> network
    Atto.string "MODE"
    Atto.space
    chan <- Atto.takeWhile (/= ' ')
    Atto.space
    mode <- Text.unpack <$> Atto.takeWhile (/= ' ')
    Atto.space
    text <- Just <$> Atto.takeText
    return $ Mode nick name host chan mode text

-- XXX might be incorrect
quit = do
    (nick, name, host) <- user
    Atto.string "QUIT"
    Atto.space
    Atto.char ':'
    text <- Atto.takeText
    return $ Quit nick name host text

ircJoin = do
    (nick, name, host) <- user
    Atto.string "JOIN"
    Atto.space
    Atto.char ':'
    chan <- Atto.takeText
    return $ Join nick name host chan

part = do
    (nick, name, host) <- user
    Atto.string "PART"
    Atto.space
    chan <- Atto.takeWhile (/= ' ')
    Atto.try $ do
        Atto.space
        Atto.char ':'
    text <- Atto.takeText
    return $ Part nick name host chan text

topic = do
    (nick, name, host) <- user
    Atto.string "TOPIC"
    Atto.space
    chan <- Atto.takeWhile (/= ' ')
    Atto.space
    Atto.char ':'
    text <- Atto.takeText
    return $ Topic nick name host chan text

invite = do
    (nick, name, host) <- user
    Atto.string "INVITE"
    Atto.space
    rec <- Atto.takeWhile (/= ' ')
    Atto.space
    Atto.char ':'
    chan <- Atto.takeText
    return $ Invite nick name host rec chan

-- XXX might be incorrect
kick = do
    (nick, name, host) <- user <|> network
    Atto.string "KICK"
    Atto.space
    chans <- Atto.takeWhile (/= ' ')
    Atto.space
    nicks <- Atto.takeWhile (/= ' ')
    Atto.space
    Atto.char ':'
    text <- Atto.takeText
    return $ Kick nick name host chans nicks text

privmsg = do
    (nick, name, host) <- user <|> network
    Atto.string "PRIVMSG"
    Atto.space
    dest <- Atto.takeWhile (/= ' ')
    Atto.space
    Atto.char ':'
    text <- Atto.takeText
    return $ Privmsg nick name host dest text

notice = do
    (nick, name, host) <- user <|> network
    Atto.string "NOTICE"
    Atto.space
    dest <- Atto.takeWhile (/= ' ')
    Atto.space
    Atto.char ':'
    text <- Atto.takeText
    return $ Notice nick name host dest text

-- FIXME i have no idea what a KILL looks like
kill :: Parser IrcAST
kill = return $ Kill "" ""

-- "PING :irc.rizon.us"
ping = do
    Atto.string "PING"
    Atto.string " :"
    server <- Atto.takeText
    return $ Ping server

ircError :: Parser IrcAST
ircError = do
    Atto.string "ERROR"
    Atto.string " :"
    text <- Atto.takeText
    return $ Error text

numeric = do
    Atto.char ':'
    Atto.takeWhile (/= ' ')
    Atto.space
    num <- Atto.takeWhile1 (`elem` (['0' .. '9'] :: [Char]))
    Atto.space
    Atto.takeWhile (/= ' ')
    Atto.skipWhile (`elem` (['*', '=', '@'] :: [Char]))
    c <- fmap Text.stripEnd <$> cmd
    text <- (Atto.try $ colonText) <|> pure ""
    return $ Numeric num c text
  where
    colonText = Atto.skipWhile (/= ':') >> Atto.char ':' >> Atto.takeText
    cmd = (fmap Just . Atto.try $ Atto.takeWhile1 (/= ':')) <|> pure Nothing

-- ":irc.rizon.no CAP * ACK :multi-prefix"
-- ":irc.rizon.no CAP Tombot ACK :multi-prefix"
-- http://www.leeh.co.uk/draft-mitchell-irc-capabilities-02.html
cap = do
    Atto.try $ do
        Atto.char ':'
        Atto.skipWhile (/= ' ')
        Atto.space
    Atto.string "CAP"
    nick <- Atto.takeWhile1 (/= ' ')
    Atto.space
    Atto.skipWhile (/= ' ')
    Atto.space
    subcap <- Atto.choice subcaps
    text <- Atto.takeText
    return $ Cap subcap text
  where
    subcaps = ["LS", "LIST", "REQ", "ACK", "NAK", "CLEAR", "END"]

-- }}}

