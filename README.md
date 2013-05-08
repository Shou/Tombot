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
It takes a string as an argument.<br>
Example: `.> I-it's not like I'm sending this because I want to or a-anything, idiot!`

* `<`
Prints a message to the user.<br>
It acts the same way as `.>`.<br>
Example: `.< Psst, Tombot! Please print some kawaii messages.`

* `^`
Searches for a message in the bot's chatlog.<br>
It takes two optional arguments. The index of the list of matches, and a search string. If no search string is given, it matches against everything.<br>
Example: `.^ banana -apple`

* `us`
Userlist printing function.<br>
Takes no arguments.<br>
Example: `.us`

* `sed`
A regex replace function.<br>
It takes two arguments, the regex matching and replacing string and a string.<br>
Example: `.> I love bananas! -> sed s/banana/apple/`

* `ai`
Prints currently airing anime.<br>
It takes one argument; the anime to search for.<br>
Example: `.ai`

* `an`
Prints recent anime releases.<br>
It takes one argument; the anime to search for.<br>
Example: `.an yuruyuri -HorribleSubs`

* `ma`
Prints recent manga releases.<br>
It takes one argument; the manga to search for.<br>
Example: `.ma banana no nana`

* `ra`
Prints a random number or string.<br>
Takes one integer as an argument, or several strings separated by `|` (pipe).<br>
Example: `.ra Suwako|Momiji|Youmu`

* `lewd`
Prints a random string from the file as defined in `Config.hs`.<br>
Takes arguments!<br>
Example: `.lewd ++ .lewd ++ .lewd ++ .lewd`

* `sage`
Sages a person in the channel.<br>
Takes one argument, a nick.<br>
Example: `.sage Shou`

* `wiki`
Prints the top paragraph of a Wikipedia article.<br>
Takes one argument, the article title.<br>
Example: `.wiki haskell programming language`

* `isup`
Prints the status of a website.<br>
Takes one argument, an URL.<br>
Example: `.isup haskell.org`

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

