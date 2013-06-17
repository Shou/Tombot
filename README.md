Tombot
======

This is the successor to [KawaiiBot-hs](https://github.com/Shou-/KawaiiBot-hs).
Tombot, a mix of "tomboy" and "bot", is a cute IRC bot that does useless things, such as grabbing information about anime and manga, having its own small language and much more.

See `Config.example.hs` for options.

To install, run `cabal install` in the root of her directory.

## Functions

A line using her functions need to be prefixed with `.` or `!` by default.

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
Syntax: `^ [index] [<string>]`
Example: `:^ 3 banana -apple`

* `us`
Userlist printing function.<br>
Syntax: `:us`

* `sed`
A regex replace function.<br>
Syntax: `sed s/<match>/<replacement>/[i] <string>`
Example: `:> I love bananas! -> sed s/banana/apple/`

* `ai`
Prints currently airing anime.<br>
Syntax: `ai [<string>]`
Example: `:ai`

* `an`
Prints recent anime releases.<br>
Syntax: `an [<string>]`
Example: `:an yuruyuri -HorribleSubs`

* `ma`
Prints recent manga releases.<br>
Syntax: `ma [amount] [<string>]`
Example: `:ma 5 banana no nana`

* `ra`
Prints a random number or string.<br>
Syntax: `ra <integer>, <string> | <string> ...`
Example: `:ra Suwako|Momiji|Youmu`

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
Example: `.lewd -> .>`

* `$$`
Application operator, it appends the output of the function on the right into the function on the left. The opposite of `->`.<br>
Example: `.we $$ .ra Tokyo|Oslo|Madrid`

* `++`
Add, it appends the string of the output on the right to the output on the left.<br>
Example: `.lewd ++ .lewd`

* `()`
Not an operator, but you can use parentheses to isolate a set of functions.

⑨
♡

