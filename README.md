# tablebot

[![CI](https://github.com/WarwickTabletop/tablebot/actions/workflows/main.yml/badge.svg)](https://github.com/WarwickTabletop/tablebot/actions/workflows/main.yml)

An extendable Discord bot framework written on top of [`discord-haskell`](https://github.com/aquarial/discord-haskell).

If you're new to this project, or completely new to git, and Haskell, you might be interested in looking at the [Setup from Scratch](CONTRIBUTING.md#setup-from-scratch) section in the [contributor's guide](CONTRIBUTING.md). If you want tutorials on making your first plugin or how exceptions work, checkout the tutorials in the [tutorials](tutorials) folder.

## Environment file setup

Create a `.env` file containing the following keys. Consult `.env.example` if you're unsure how this should be formatted! Please note that the `.env` file must have a newline at the end of it - i.e. the last line should be blank.

* `DEBUG` (mandatory) - whether the bot should run in debug mode. This bypasses all permission checks, and prints
  certain log message that would otherwise be suppressed
* `DISCORD_TOKEN` (mandatory) - the Discord token for your bot. Go to
  the [Discord Developer Portal](https://discord.com/developers/applications), create an application representing your
  bot, then create a bot user and copy its token.
* `PREFIX` (optional, defaults to `!`) - the prefix for each bot command. For example, if you set it to `$`, then you
  would call `$ping` to ping the bot.
* `SQLITE_FILENAME` (mandatory) - a name for your SQLite database, for example `database.db`.
* `CATAPI_TOKEN` (optional) - the api token to get cat pictures. Go to [The Cat API](https://thecatapi.com/) to create
  an account and get a token so you can enjoy cats.
* `EXEC_GROUP` (optional) - the group ID assigned to exec members.
* `MODERATOR_GROUP` (optional) - the group ID assigned to moderator members.
* `SUPERUSER_GROUP` (optional) - the group ID assigned to the superuser. Strongly recommended

The three Group settings are optional, but without them any commands that require elevated permissions will not be able
to be called when DEBUG is false. Users with the superuser group are able to run every command (including some dangerous
ones), so caution should be used when setting these up.

If you have any difficulties setting it up, see the [contributor's guide](CONTRIBUTING.md) for a walkthrough.
