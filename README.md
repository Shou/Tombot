Tombot
======

This is the successor to [KawaiiBot-hs](https://github.com/Shou-/KawaiiBot-hs).
Tombot, a mix of "tomboy" and "bot", is an IRC bot that does useless things, such as grabbing information about anime and manga, having its own small composable language and much more.

See `Config.example.hs` for options.

To install, run `cabal install` in the root of her directory.

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

* `raw`
Control the bot directly; raw IRC message.<br>
Syntax: `raw <string>`
Example: `:raw PRIVMSG #example :Hi!`

* `sed`
A regex replace function.<br>
Syntax: `sed s/<match>/<replacement>/[i] <string>`
Example: `:> I love bananas! -> sed s/banana/apple/`

* `wiki`
Prints the top paragraph of a Wikipedia article.<br>
Syntax: `wiki <string>`
Example: `:wiki haskell programming language`

* `isup`
Prints the status of a website.<br>
Syntax: `isup <url>`
Example: `:isup haskell.org`

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

⑨
<br>
♡

