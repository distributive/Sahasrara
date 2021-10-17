# Writing Your First Plugin!

In this tutorial, we will be writing an overengineered Ping plugin to show how you can write your own plugins. We'll go over basic plugins, taking in arguments, IO operations and database operations.

## My First Ping

We are going to first focus on a basic Ping plugin, which responds to any message `.ping` with the message "pong". Amazing.

To create a plugin, you need to make a Haskell file in the `Plugins` directory. There are two parts that you need to write: any `Command`s you actually want your bot to implement, and the resulting `Plugin` object that combines your implementation.

A `Command` is a mapping from a name to code that takes in the rest of the message and performs some action. The former is simply a string that will be checked for at the start of a message (after the command prefix - we'll use `.` in this tutorial), so for this example "ping" will suffice. Here is the implementation so far:

```haskell
ping :: Command
ping = Command "ping" _
```

If you were to compile this now, Haskell will point out the hole (`_`) and say that it needs to be of type `Parser (Message -> DatabaseDiscord ())`. There's a fair amount going on in that type, so let's discuss each part.

`Parser` refers to a parser that works with `Text`. We use the `megaparsec` library, which provides a variety of helpful functions for writing parsers of your own. These can be specified in do-notation, so allow for clear sets of steps describing your parser. For example, the below parser reads a number (consisting of some digits), then a space, then another number.

```haskell
twoNumbers :: Parser (Int, Int)
twoNumbers = do
    num1 <- number
    space
    num2 <- number
    return (num1, num2)
```

At the end of the parser, we use `return` to give back the two numbers found - hence giving us a `Parser (Int, Int)` since we can run the parser to get a value of type `(Int, Int)` out. Note that `number :: Parser Int` is defined in `Tablebot.Plugin.Parser`, so you would have to import that if you wanted to use the above parser.

Later on we'll be seeing how you can avoid writing these parsers altogether using _smart commands_. As such, we won't discuss writing parsers in more detail in this tutorial - if you're interested then there are a good number of tutorials about `megaparsec` and its parent `parsec` online.

Our command parser requires us to return a function of type `Message -> DatabaseDiscord ()`. `Message` is a type provided by the library `discord-haskell`, which contains a variety of information about the message that the command was contained in. You can find all of the data within it documented [in the `discord-haskell` documentation](https://hackage.haskell.org/package/discord-haskell-1.8.8/docs/Discord-Internal-Types-Channel.html#t:Message), but for now we will just be using the input `Message` with some of the helper functions defined in this library.

Finally, `DatabaseDiscord ()` is the type of a _monadic action_ which provides us a few bits of functionality - namely Discord operations (such as sending messages), database operations and IO operations. We'll cover each of these as we build our more complex plugin. In general, you do not need to worry about the specifics of the underlying implementation (for those familiar with these kinds of libraries, `DatabaseDiscord` contains a stack of monads providing each bit of functionality) and instead stick with the library functions discussed in this tutorial.

With all of that out of the way, we can finally get to implementing our Ping plugin. We use `noArguments :: (Message -> DatabaseDiscord ()) -> Parser (Message -> DatabaseDiscord ())` here, which provides us a parser for free that accepts commands with no arguments. We then need to actually respond - this is done using `sendMessage :: Message -> Text -> DatabaseDiscord ()`, which sends a message in the same channel as the input `Message`, with content `Text`:

```haskell
ping :: Command
ping = Command "ping" (noArguments $ \m -> do
    sendMessage m "pong")
```

Now we just need to make a `Plugin`. This is done using `plug` (the default empty plugin) and a record update to add your commands in. To add commands to a plugin, simply update the `commands` field of the record as follows.

```haskell
pingPlugin :: Plugin
pingPlugin = plug {commands = [ping]}
```

## Echo

"But wait!" I hear you cry. "Echo isn't ping! You lied to us!"

"Yeah fair enough, but at least this is kind of like ping and will teach you how to use smart commands." I respond, sleepily.

"Touché."

The echo command takes in a single argument (some text), which is repeated back to the user. Thrilling, I know. We could define this with a parser that takes all text up to the end of the line, but instead we are going to use _smart commands_. Smart commands allow you to define a command via a more complex function type, which a parser is automatically generated for. For example, if we define a command that takes in a `Text` argument, then it will consume the first word of the message that it sees (after the command invocation of `.echo`) and provide that as input to your function.

Smart commands are invocated through `parseComm :: PComm ty => ty -> Parser (Message -> DatabaseDiscord ())`, so slot in nicely to our existing command infrastructure. As such, a first pass of `echo` could look like this.

```haskell
echo :: Command
echo = Command "echo" (parseComm echoHelp)
    where echoHelp :: Text -> Message -> DatabaseDiscord ()
          echoHelp t m = sendMessage m t
```

As you can see, we didn't have to even mention parsers here - we defined the sort of command that we wanted, and `parseComm` built the parser for us. However, the parser for `Text` only parses a single word, which may not be the entire input. As such, we provide a collection of new types that have slightly different parsers. These are instances of `CanParse` in `Tablebot.Plugin.SmartCommand` so can be seen in that documentation, but we also list the new ones here for convenience.

* `newtype Quoted a = Qu a` parses input within quote marks, giving a value of type `a`.
* `data Exactly (s :: Symbol) = Ex` parses exactly the string provided by `s` - e.g. `Exactly "add"` parses the string "add".
* `newtype RestOfInput a = ROI a` parses the entire rest of the input, giving a value of type `a`.
* `newtype WithError (err :: Symbol) x = WErr x` throws a custom error `err` if the interior parser fails. For example, `WithError "Can't find add" (Exactly "add")` generates a parser that errors with "Can't find add" if the word "add" is not present.

`RestOfInput` is the correct type here, so we can modify our previous example to make a correct `echo` plugin:

```haskell
echo :: Command
echo = Command "echo" (parseComm echoHelp)
    where echoHelp :: RestOfInput Text -> Message -> DatabaseDiscord ()
          echoHelp (ROI t) m = sendMessage m t
```

As a bonus, we also map common types to helpful parsers.

* `[a]` parses `a` zero or more times and gives us the list of results that it got.
* `Maybe a` optionally parses `a`, returning `Just x` if successful, else `Nothing`.
* `Either a b` tries to parse `a` (giving `Left x`), and if that fails tries to parse `b` instead (giving `Right y`).
* `(a, b)` parses `a` and then `b`. This is the same behaviour as having multiple function arguments (so `(a, b) -> Message -> DatabaseDiscord ()` is equivalent to `a -> b -> Message -> DatabaseDiscord ()`) but can be used inside other types (so `[(a, b)] -> ...` makes sense while `[a -> b] -> ...` doesn't).

With this, we get a fairly expressive parsing language built from the types of the commands that we want to run - thus allowing you to avoid writing parsers. If you'd like to see a particularly complex example, look at `Tablebot.Plugins.Quote`.

## Ping with Time

Now, I know what you're thinking - ping is good, but what if it also told you the time? I know, so exciting. Well this is easy enough, we have functions that get us the time right, let's use one of those... oh

```haskell
getSystemTime :: IO SystemTime
```

We need to be able to do things in IO. Fortunately, as eluded to earlier, `DatabaseDiscord` provides this functionality - but we should briefly look at the actual definition of `DatabaseDiscord` to justify this further.

```haskell
type DatabaseDiscord = SqlPersistT DiscordHandler
type DiscordHandler = ReaderT DiscordHandle IO
```

This is called a _monad transformer stack_, which means that we get to combine the functionality of different monads by stacking them up. `DatabaseDiscord` gives us database operations through `SqlPersistT` (from `esquelito`, more on that later), discord operations through `DiscordHandler` and IO operations through `IO` at the bottom of the stack. The challenge is accessing each part of the stack. Rather than going on a long discussion about what a monad transformer is, here are the important facts you need to know when working within `DatabaseDiscord`:

* Database operations (those with types like `SqlPersistT a`) can be run as normal.
* Discord operations (those with types like `DiscordHandler a`) can be run using `lift` - e.g. `lift . restCall ...` lets you make direct Discord API requests.
* IO operations can be run using `liftIO` from `Control.Monad.IO.Class`, or `lift . lift` if you really don't want to import a package.

With this in mind, we have an easy solution to our timing problem.

```haskell
ping' :: Command
ping' = Command "ping" (parseComm pingWithTime)
    where pingWithTime :: Message -> DatabaseDiscord ()
          pingWithTime m = do
              now <- liftIO $ systemToUTCTime <$> getSystemTime
              sendMessage $ "pong (" ++ show now ++ ")"
```

Great!

## Ping with Database

Okay so hear me out, what if `ping` also told you how many pings it had received so far from a given user. Good, you're already used to questionable extensions to this command, let's go.

First, we need a database schema. This is written using `persistent`. Rather than giving an entire `persistent` tutorial here, I'll provide the schema and go with a learn by example approach. Check out some of the other plugins, or other tutorials on `persistent`, if you need more help.

```haskell
share
  [mkPersist sqlSettings, mkMigrate "pingMigration"]
  [persistLowerCase|
PingCount
  Primary reminderUid Word64
  counter Int
  deriving Show
|]
```

We now have to link this migration into the rest of the system. This is done by adding it to the plugin through the migrations field. Note the `mkMigrate "pingMigration"` in the above example - we have to use that same name in the plugin definition.

```haskell
pingPlugin :: Plugin
pingPlugin = plug {commands = [ping'], migrations = [pingMigration]}
```

With that in place, we now need commands that work with this database. We use `esquelito`, which allows us to write in an SQL-like language. Its [documentation](https://hackage.haskell.org/package/esqueleto-3.5.3.0/docs/Database-Esqueleto.html) has a variety of great examples which you should investigate if you're going to perform database operations! For now, let's look at the implementation of this command. First, we attempt to get the existing user info.

```haskell
ping'' = Command "ping" (parseComm pingDB)
    where pingDB :: Message -> DatabaseDiscord ()
          pingDB m = do
              let uid = userId $ messageAuthor m
              user <- select $ from $ \p -> do
                  where_ (p ^. PingCountReminderUid ==. val uid)
                  return p
              ...
```

Note that the table we are selecting from is inferred by use of `^. PingCountReminderUid`, which gets the ReminderUid field of the PingCount table from p.

This query will either get us a single record in a list, or the empty list. We use this information to create the record we would like to be present in the database - either a new record if this record was not present, or a record with incremented count otherwise. We finally then use `repsert`, which replaces or inserts the given data.

```haskell
              ...
              let record' = case user of
                  [] -> PingCount uid 1
                  (x : _) -> (\(PingCount uid' count) -> PingCount uid' (count+1)) $ entityVal x
              repsert uid record'
              ...
```

Finally, we need to provide functionality for reporting this information back to the user, which is a game of reading the record we just wrote to the database.

```haskell
              ...
              sendMessage m (show $ count record')
```

Hooray!

## Adding Help

Finally, all good plugins should have help information. This is achieved by creating `HelpPage`s. Here is a direct quote of Quote's help pages:

```haskell
showQuoteHelp :: HelpPage
showQuoteHelp = HelpPage "show" "show a quote by number" "**Show Quote**\nShows a quote by id\n\n*Usage:* `quote show <id>`" []

addQuoteHelp :: HelpPage
addQuoteHelp = HelpPage "add" "add a new quote" "**Add Quote**\nAdds a quote\n\n*Usage:* `quote add \"quote\" - author`" []

quoteHelp :: HelpPage
quoteHelp = HelpPage "quote" "store and retrieve quotes" "**Quotes**\nAllows storing and retrieving quotes" [showQuoteHelp, addQuoteHelp]
```

A `HelpPage` consists of a short command name, a short summary, a longer summary and a list of subcommands. These are fairly self-explanatory. Note that you can use the `[r| |]` quoter for multiline strings, by importing `Text.RawString.QQ`.

These are then added to the plugin like everything else - through a field in the `Plugin`. You only need to add the highest parent - any other help pages referenced by any included page will be included as well. Here is Quote's plugin definition as an example.

```haskell
quotePlugin :: Plugin
quotePlugin = plug {commands = [quote], migrations = [quoteMigration], helpPages = [quoteHelp]}
```
