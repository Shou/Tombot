Tombot
======

Tombot is a general textual bot with interfaces to IRC and Discord. It has its
own small composable interpreted language.

    :say Hey, ++ space ++ nick ++ say !
    :(random 10 == say 5 >< say It's five!) <> say It's not five.

Interfaces to various APIs.

    :wiki Haskell programming language
    :anime Jojo's Bizarre Adventure

User-definable functions.

    :let utctime :formattime <- unixtime ++ space ++ >0 %c

Timed events and reactive events.

    :every 3600 :airing
    :sleep 100 >> say Hello!
    :on /^what time is it/i :utctime

## Install

You can install Tombot with `[http://haskellstack.org/](stack)`.

    stack install

## Functions

`This section is outdated`

A line using her functions need to be prefixed with `:` by default.

* `help`

* `!`
Search DuckDuckGo using a !bang tag.<br>
`! <bang> <string>` `:!i Haskell`

* `>`
Displays a message to the channel, or user if it is a private message.<br>
`> <string>` `:> I-it's not like I'm sending this because I want to or a-anything, idiot!`

* `<`
Displays a message to the user.<br>
`< <string>` `:< Psst, Tombot! Please print some secret messages.`

* `^`
Searches for a message in the bot's chatlog.<br>
`^ [<index>] [<string>]` `:^ 3 banana -apple`

* `b`
Ban a user, or several. `op`<br>
`b <nick> ...` `:b Moss John`

* `k`
Kick a user, or several. `op`<br>
`k <nick> ...` `:k Frank Tom`

* `m`
Modify the channel mode. `op`<br>
`m [+|-]<mode> <args>` `:m -b Moss!*@*`

* `v`
Voice a user, or several. `op`<br>
`v <nick> ...` `:v Chen Rin`

* `an`
Search for anime releases and optionally choose how many results to print.<br>
`an [<number>] [<string>]` `:an 10 potato`

* `ai`
Displays currently airing anime.<br>
`ai [<string>]` `:ai`

* `in`
Show whether a regex matches a string.<br>
`in <regex> <string>` `:in /banana/ <- ra banana|cake`

* `ma`
Search for manga releases and optionally choose how many results to print.<br>
`ma [<number>] [<string>]` `:ma 5 banana no nana`

* `me`
Perform an action (`/me`) with the bot.<br>
`me <string>`

* `on`
Run some function(s) on match.<br>
`on <regex> <kawaiilang>` `:on /what should i do/i :nick ++ ra Go to bed|Do your work|Work out`

* `ra`
Displays a random number or string.<br>
`ra <integer>, <string> | <string> ...` `:ra Suwako|Momiji|Youmu`

* `re`
Add a reminder for someone; printed on join.<br>
`re <nick> [<string> | <kawaiilang>]` `re ChinaGal :> Won't you take me where you go?`

* `us`
Userlist printing function.<br>

* `len`
Length of a string.<br>
`len <string>` `:us -> len`

* `let`
Define a new function by composing old ones.<br>
`let <string> <kawaiilang>` `:let randomuser :us -> sed s/ /|/ -> ra`

* `raw`
Control the bot directly; raw IRC message. `admin`<br>
`raw <string>` `:raw PRIVMSG #example :Hi!`

* `sed`
A regex replace function.<br>
`sed s/<match>/<replacement>/[i] <string>` `:> I love bananas! -> sed s/banana/apple/`

* `bots`
Confirms that it is a bot.<br>

* `eval`
Evaluate Kawaii Language code.<br>
`eval <kawaiilang>` `:eval :> Hi!`

* `help`
Display help about a function.
`help <string>` `:help functions`

* `host`
Display the hostname of the user.

* `http`

* `isup`
Displays the status of a website.<br>
`isup <url>` `:isup haskell.org`

* `join`
Join a channel.<br>
`join <string>` `:join #example`

* `kill`
Kill a bot event.
`kill <string>` `kill event`

* `name`
Display the user's name.

* `nick`
Display the user's nick.

* `part`
Leave the current channel.

* `quit`
Make the bot quit the IRC server. `admin`

* `show`
Display data held by the bot.<br>
`show [Config | Chan <string> | User <string>]` `show User John`

* `stat`
Display or change a user's stat. `admin*`<br>
`stat <nick> [<stat>]` `:stat Offender Banned`

* `tell`
Tell a user something the next time they talk.<br>
`tell <nick> <string>` `:tell James Check your left pocket!`

* `verb`

* `wiki`
Displays the top paragraph of a Wikipedia article.<br>
`wiki <string>` `:wiki haskell programming language`

* `cjoin`
Display or change the ChanJoin value of the current channel. `op`<br>
`cjoin [True | False]`

* `event`
Add a new bot event. `admin`<br>
`event <string> <kawaiilang>` `:event airing :sleep 1800 >> ai`

* `funcs`
Display or change the channel's function list. `op`<br>
`funcs [Whitelist [<string> ...] | Blacklist [<string> ...]]` `funcs Blacklist`

* `kanji`
Display the definitions of a kanji.<br>
`kanji <string>` `:kanji 氷`

* `nicks`
Display or change the bot's list of nick it uses on connect. `op`<br>
`nicks <string> ...` `:nicks Tomboy Otenba`

* `sleep`
Delay by n seconds. Useful when composing with other functions.<br>
`sleep <number>` `:sleep 30 >> us -> sed s/ /|/ -> ra -> b `

* `title`
Display the title of a website.<br>
`title <url>` `:title https://example.love/`

* `topic`
Display, append to, strip from, or set the channel's topic. `op`<br>
`topic [+<string | -<string> | <string]` `:topic + | We're finally moving to Freenode!`

* `cajoin`
Display or change the channel's ChanAutoJoin value. `op`<br>
`cajoin [True | False]`

* `prefix`
Display or change the bot's KawaiiLang prefix characters. `op`<br>
`prefix [<char> ...]` `prefix .:!`

* `romaji`
Convert Japanese syllabaries to Romaji.<br>
`romaji <string>` `:romaji あなたを食べたい`

* `connect`

* `restart`
Restart the bot's process. `root`

* `reverse`
Print the words in reverse order.<br>

\* Only when given an argument.

## Operators
* `->`
Pipe, it appends the output of the function on the left into the function on the right.<br>
`.> Hi! -> ra Bye!|` - Pipe into next command, before it runs.

* `<-`
The opposite of `->`.<br>
`:ra Oslo is the worst.|The best: <- ra Tokyo|Oslo|Madrid` - Pipe into previous command, before it runs.

* `++`
Add, it appends the string of the output on the right to the output on the left.<br>
`.ai ++ .an` - 

* `>>`
Execute the first function, disregard the output and continue.<br>
`:tell Fogun Did you watch Gargantia yet? >> tell lunar Hi!` - Disregard `tell`'s output because it outputs nothing.

* `<>`
Or; return whatever function's result isn't empty. `Lazy`<br>
`:ra You're safe for now| <> b John` - Only ban John when `ra` returns an empty value.

* `><`
And; return the right function's output only if both functions return something. `Lazy`<br>
`:ra You're dead, kid!| >< b John` - Only ban John when `ra` returns `You're dead kid!`

* `+>`
And append; return both functions' outputs appended only if both return something. `Lazy`<br>
`:> Title: +> title http://github.com/` - only output and append when the bot successfully fetches Github's website title.

## Function config files

Some functions have files associated with them. These files are stored in the
config directory as defined in `Config.hs`. These files are:

* `help`
This is the help file, and it is included with the bot, you just need to move
it to the directory.

* `tell`
This is the file that is filled with tells from the `tell` function, which are
messages printed when a user shows activity.

* `respond`
This is the file that is filled with ons from the `on` function, which are
regex matches and the Kawaiilang to run on match.

* `letfuncs`

