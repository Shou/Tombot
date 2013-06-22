Tombot
======

This is the successor to [KawaiiBot-hs](https://github.com/Shou-/KawaiiBot-hs).
Tombot, a mix of "tomboy" and "bot", is an IRC bot that does useless things, such as grabbing information about anime and manga, having its own small composable language and much more.

See `Config.example.hs` for options.

## Install

To install, run `cabal install` in the root of her directory.
This assumes you have <a href=http://haskell.org/>Haskell Platform</a>
installed; if not, go install it.

## Kawaiilang

The bot comes with its own small language that can be used for various things.
The language is supposed to be composable through the use of operators.
An example of the syntax: `:(ra True| >< gay Hi!) <> gay Bye!` where `ra` and
`gay` are functions, the subsequent text are their arguments and `><` and `<>`
are operators. Parentheses are also supported to enclose functions, and are
useful because operators are left associative.

## Functions

A line using her functions need to be prefixed with `.` or `:` by default.

Along with these functions a user may define new functions using `let`, and you
can use `help` to see what those functions are.

* `!`
Search DuckDuckGo using a !bang tag.<br>
Syntax: `! <bang> <string>`
Example: `:!i Haskell`

* `>`
Prints a message to the channel, or user if it is a private message.<br>
Syntax: `> <string>`
Example: `:> I-it's not like I'm sending this because I want to or a-anything, idiot!`

* `<`
Prints a message to the user.<br>
Syntax: `< <string>`
Example: `:< Psst, Tombot! Please print some kawaii messages.`

* `^`
Searches for a message in the bot's chatlog.<br>
Syntax: `^ [<index>] [<string>]`
Example: `:^ 3 banana -apple`

* `b`
Ban a user, or several. `OP`<br>
Syntax: `b <nick> ...`
Example: `:b Moss John`

* `k`
Kick a user, or several. `OP`<br>
Syntax: `k <nick> ...`
Example: `:k Frank Tom`

* `m`
Modify the channel mode. `OP`<br>
Syntax: `m [+|-]<mode> <args>`
Example: `:m -b Moss!*@*`

* `v`
Voice a user, or several. `OP`
Syntax: `v <nick> ...`
Example: `:v Chen Rin`

* `an`
Search for anime releases and optionally choose how many results to print.<br>
Syntax: `an [<number>] [<string>]`
Example: `:an 10 potato`

* `ai`
Prints currently airing anime.<br>
Syntax: `ai [<string>]`
Example: `:ai`

* `in`
Show whether a regex matches a string.<br>
Syntax: `in <regex> <string>`
Example: `:in /banana/ <- ra banana|cake`

* `ma`
Search for manga releases and optionally choose how many results to print.<br>
Syntax: `ma [<number>] [<string>]`
Example: `:ma 5 banana no nana`

* `me`

* `on`
Run some function(s) on match.<br>
Syntax: `on <regex> <kawaiilang>`
Example: `:on /what should i do/i :nick ++ ra Go to bed|Do your work|Work out`

* `ra`
Prints a random number or string.<br>
Syntax: `ra <integer>, <string> | <string> ...`
Example: `:ra Suwako|Momiji|Youmu`

* `re`
Add a reminder for someone; printed on join.<br>
Syntax: `re <nick> <string>`
Example: `re ChinaGal Won't you take me where you go?`

* `us`
Userlist printing function.<br>
Syntax: `us`

* `gay`
Colour a string with the rainbow.<br>
Syntax: `gay <string>`
Example: `:gay Lesbian Gay Bi Trans`

* `len`
Length of a string.<br>
Syntax: `len <string>`
Example: `:us -> len`

* `let`
Define a new function by composing old ones.<br>
Syntax: `let <string> <kawaiilang>`
Example: `:let randomuser :us -> sed s/ /|/ -> ra`

* `raw`
Control the bot directly; raw IRC message.<br>
Syntax: `raw <string>`
Example: `:raw PRIVMSG #example :Hi!`

* `sed`
A regex replace function.<br>
Syntax: `sed s/<match>/<replacement>/[i] <string>`
Example: `:> I love bananas! -> sed s/banana/apple/`

* `bots`
Confirms that it is a bot.<br>

* `eval`
Evaluate Kawaii Language code.<br>
Syntax: `eval <kawaiilang>`
Example: `:eval :> Hi!`

* `help`

* `host`

* `http`

* `isup`
Prints the status of a website.<br>
Syntax: `isup <url>`
Example: `:isup haskell.org`

* `join`

* `kill`

* `name`

* `nick`

* `part`

* `quit`

* `show`

* `stat`

* `tell`

* `verb`

* `wiki`
Prints the top paragraph of a Wikipedia article.<br>
Syntax: `wiki <string>`
Example: `:wiki haskell programming language`

* `cjoin`

* `event`

* `funcs`

* `greet`

* `nicks`

* `sleep`

* `title`

* `topic`

* `cajoin`

* `cutify`

* `prefix`

* `britify`

* `connect`

## Operators
* `->`
Pipe, it appends the output of the function on the left into the function on the right.<br>
Example: `.> Hi! -> gay`

* `<-`
The opposite of `->`.<br>
Example: `:> This is a cool city: ++ gay <- ra Tokyo|Oslo|Madrid`

* `++`
Add, it appends the string of the output on the right to the output on the left.<br>
Example: `.lewd ++ .lewd`

* `>>`
Execute the first function, disregard the output and continue.<br>
Example: `:tell Fogun Did you watch Gargantia yet? >> tell lunar Hi!`

* `<>`
Or; return whatever function's result isn't empty. `Lazy`<br>
Example: `:ra You're safe for now| <> b John`

* `><`
And; return the right function's output only if both functions return something. `Lazy`<br>
Example: `:ra You're dead, kid!| >< b John`

* `+>`
And append; return both functions' outputs appended only if both return something. `Lazy`<br>
Example: `:`

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

* `britify`

* `cutify`


⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨⑨
<br>
♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡♡

