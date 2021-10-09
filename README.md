# tablebot
[![CI](https://github.com/WarwickTabletop/tablebot/actions/workflows/main.yml/badge.svg)](https://github.com/WarwickTabletop/tablebot/actions/workflows/main.yml)

An extendable Discord bot framework written on top of `discord-haskell`

Create a `.env` file containing the following keys. Consult `.env.example` if you're unsure how this should be formatted!

* `DISCORD_TOKEN` (mandatory) - the Discord token for your bot. Go to the [Discord Developer Portal](https://discord.com/developers/applications), create an application representing your bot, then create a bot user and copy its token.
* `PREFIX` (optional, defaults to `!`) - the prefix for each bot command. For example, if you set it to `$`, then you would call `$ping` to ping the bot.
* `SQLITE_FILENAME` (mandatory) - a name for your SQLite database, for example `database.db`.
* `CATAPI_TOKEN` (optional) - the api token to get cat pictures. Go to [The Cat API](https://thecatapi.com/) to create an account and get a token so you can enjoy cats.

## Ormolu

You must use ormolu, which can be installed via stack:

`stack install ormolu`

Then you can run it on every file via

`ormolu --mode inplace $(git ls-files '*.hs')`

You can see full documentation on the [Ormolo repo](https://github.com/tweag/ormolu#usage).