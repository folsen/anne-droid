Anne Droid - the helpful little bot that could
===

Slack enables integration in a couple of different ways, there's
webhooks, both outgoing and ingoing, the outgoing ones send data from
Slack on a specific trigger word and allow response by responding to the
outgoing HTTP POST request. The incoming webhooks allow you to send data
into slack without any prompt from Slack itself to do so (useful for
services that monitor something else or just send regular updates into
slack or something). There is a slack API that is used for acting on
behalf of a single user, i.e. there is a per-user authentication token
and it allows you to send messages as that user etc. Finally there are
`Slash commands` that seem to be somewhere in between webhooks and a full
API integration, i'm not exactly sure what makes them differ from
webhooks.

This bot has been set up to handle integration with webhooks, for
starters it handles outgoing webhooks but it is trivial to make it
handle incoming webhooks as well.


Building
---

This is a standard Haskell application, nothing fancy. I will assume
that you have GHC and Cabal installed. There are "relatively" few
dependencies to the project, I've kept them low within the bounds of
what is convenient.

To install the project you should

    cabal sandbox init
    cabal install -j

You can also start a REPL with

    cabal repl

That will launch into the Main module, which also has the Web module
imported so you can for instance do something like

    *Main> let wc = WebConfig "development" 3000
    *Main> web wc
    Setting phasers to stun... (port 3000) (ctrl-c to quit)

to get a web server running in the REPL so that you can easily `ctrl-c`
out of it and `:r` to reload our code.

Contributing
---

The main purpose of this bot is of course to have commands that are
available to fellow slack chatters! We want to have as many commands as
possible and as useful commands as possible.

I will walk through the process of adding a simple command and hopefully that
will demonstrate pretty clearly how to add more commands, then I will let your
imagination take it further.

Let's say we want to add a command similar to `dogetipbot`s tipping
function. The command looks something like `tip @user 100`. There are
three parts to this command, the command itself, `tip` who it's going to
and the amount. So we'll see how we get all these three parts into our
command handler later.

The first thing you want to do is go to `src/Lumibot/Commands.hs`, this
is where all the command work will take place.

Go find the definition of `Command`, as of this writing it looks like
this:

    data Command = Help
                 | CommandNotFound
                 deriving (Show)

The Help command displays a helpful message with all the commands
available. The `CommandNotFound` command is just a default command that we
give to the parser when we didn't actually find any useful command to
parse into.

Let's add the `Tip` command.

    data Command = Help
                 | Tip String Int
                 | CommandNotFound
                 deriving (Show)

Here I added the `Tip` type, which takes two parameters, some `String` and
an `Int`. The `String` here is meant to be the username, we could have
created a username type as well somewhere else to make this clear but I
thought that was a bit excessive for this case. The `Int` will store our
tip amount.

Now that I have my `Tip` command I want to write a parser for that command so
I can turn it from `String` into an actual Haskell data representation. So I add a
parser function that looks something like this.

    parseTipCommand :: CommandParser
    parseTipCommand = do
      string "tip"
      space
      char '@'
      uname  <- manyTill anyChar space
      amount <- manyTill digit eof
      return $ Tip uname (read amount)

What this does is take the string `"tip"`, a space, the `@` character (so far so
simple), then we start getting into some of the actual Parsec functions (more on
those in a bit). `manyTill` takes the character defined in the first argument
until the character in the second argument, the character in the second argument
is parsed as well but not returned, so it wont be in our `uname` variable and it
won't be available in the input anymore for the rest of the parser. So in this
case after `@` we take any character until a space. _After_ that space, we take
all digits until the end of the input. Then finally we return out new `Tip` with
the arguments for the username and the tip amount supplied. Now you'll notice
there's a `read` on the tip amount. One could write a parser for `Int`s, or one
could do the cheap things which is what this is. What `read` does is take a
string and try to coerce it into the data type expected (`Int` in this case).
This can fail dramatically and crash our program, but since we're guaranteed by
the parser (unless there are bugs) that what we were given in the `amount` are
only digits, we can be sure that `read` won't fail since the parser would have
failed before it.

Now that we have our parser, we can add it to the list of parsers
available to the application in `commandParser` which looks something
like this.

    commandParser :: CommandParser
    commandParser = choice
      [ parseHelpCommand
      ]

So we simply add our `parseTipCommand` to the list.

    commandParser :: CommandParser
    commandParser = choice
      [ parseHelpCommand
      , parseTipCommand
      ]

(**Break**) How do I write a parser?

We're using the Parsec library to write our parsers, it's a very popular
but also very big and powerful library to write parsers in. It can do
pretty much anything but it can also be slightly daunting. Parsec is
what we use to parse Ludwig, so if you learn Parsec here, you'll know
how to write the Parser for Ludwig as well.

The main package site is [here](http://hackage.haskell.org/package/parsec). But
by default in `Commands.hs` we only include `Text.Parsec` and
`Text.Parsec.Char`. Those modules provide pretty much anything we need. I
recommend looking at those, and for those feeling ambitous there are some useful
things in the `Text.Parsec.Combinator` module as well. Links below.

* [Text.Parsec](http://hackage.haskell.org/package/parsec-3.1.7/docs/Text-Parsec.html)
* [Text.Parsec.Char](http://hackage.haskell.org/package/parsec-3.1.7/docs/Text-Parsec-Char.html)
* [Text.Parsec.Combinator](http://hackage.haskell.org/package/parsec-3.1.7/docs/Text-Parsec-Combinator.html)


(**Break over!**)

The final step to getting out command to work is to handle it! We want to actually
do something in our command right?

Any parsed command is automatically sent to the `handler` function.
`handler` is designed to be a total function for which we pattern-match
all the possible arguments. The argument in question is a `SlackRequst`,
which has two parameters, of which only one is variable, and that is the
`Command`. So we want to pattern-match for all possible `SlackRequest`s,
which means pattern-matching for all possible `Command`s.

To handle our tipping command, we simply add another line to the
`handler` function that looks something like this.

    handler (SlackRequest (Tip uname amount) SlackData {..}) = return . SuccessResponse $
      "Wow such generous, @" <> userName <> " tipped " <> show amount <> " to @" <> uname

As you can see, the pattern-match part of it is this

    SlackRequest (Tip uname amount) SlackData {..}

We write out our whole `Tip` structure (because that's what the
pattern is that we want to match) and the odd-looking thing for the
`SlackData` is the `RecordWildCards` "GHC add-on" in play. We use it
quite extensively in the Ludwig codebase so I thought I'd use it here as
well even though it's not really necessary for writing idiotmatic
Haskell. What it does is automatically inject all the record names of
`SlackData` as variables in our function, so without explicitly
declaring a variable called `userName`, we have it in the function
because `SlackData` has a record named `userName`.

Now we can do whatever we want in our function with the `SlackData` and
with our `Command`, including our arguments for who got the tip and the
tip amount.

In this case I just give back a text-string saying who tipped whom how
much, but IRL you'd probably want to do stuff like call out to the
dogecoin client and make actual transfers of dogecoin.

This is the whole process (_phew_). To recap, this is what you need to
do.

1. Add your command to the `Command` data type.
1. Write a `CommandParser` function for your command.
1. Add your new `CommandParser` to the list in the `commandParser` function.
1. Write a `handler` for your `Command`.


TODO
---

There's a wealth of potential projects in this application, but one
obvious thing to do right now is to add some testing. One could add
[QuickCheck](http://hackage.haskell.org/package/QuickCheck) tests to
test all the commands automatically to make sure there's no combination
of commands or words that could break the bot. Or one could do unit
tests with something like [HSpec](http://hackage.haskell.org/package/hspec)
to make sure that the command you just programmed actually behaves like
you expect it to.

Documentation on the Slack API
---

The documentation for the various ways to interact with Slack is lacking
at best. But here are some things I found that might be relevant.

What to be expected from an outgoing webhook from Slack.

    token=WbfcgFz37ILxmQ7NqXQ6Mjcx
    team_id=T0001
    channel_id=C2147483705
    channel_name=test
    timestamp=1355517523.000005
    user_id=U2147483697
    user_name=Steve
    text=googlebot: What is the air-speed velocity of an unladen swallow?
    trigger_word=googlebot:

What you can output to the response (I think):

https://api.slack.com/methods/chat.postMessage
