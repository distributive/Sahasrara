# Sahasrara

An extendable Discord bot framework written on top of [`WarwickTabletop's Tablebot`](https://github.com/WarwickTabletop/tablebot), built on top of [`discord-haskell`](https://github.com/aquarial/discord-haskell). Note that a lot of commands were stripped from the original version of this bot, but the underlying code remains. If you find code that seems to be unused, it probably is.

## Functionality

Card searching:

Displaying card art:

Displaying card flavour text:

Searching cards using NetrunnerDB syntax:

Displaying the banlist history of cards:

Displaying current and past banlists:

Displaying Netrunner rules and timing structures:

### Upcoming

Some features that are not yet present but I would like to add soon(ish):

- Card aliases
- More comprehensive rules
- Update the search command (it's a little bit buggy and a little bit out of date)

## Contributing

If you're interested in getting involved in contributing to this bot, and are completely new to git and/or Haskell, you might be interested in looking at the [Setup from Scratch](CONTRIBUTING.md#setup-from-scratch) section in the [contributor's guide](CONTRIBUTING.md). If you want tutorials on making your first plugin or how exceptions work, checkout the tutorials in the [tutorials](tutorials) folder.

## Environment file setup

Create a `.env` file containing the following keys. Consult `.env.example` if you're unsure how this should be formatted! Please note that the `.env` file must have a newline at the end of it - i.e. the last line should be blank.

- `DEBUG` (mandatory) - whether the bot should run in debug mode. This bypasses all permission checks, and prints
  certain log message that would otherwise be suppressed.
- `DISCORD_TOKEN` (mandatory) - the Discord token for your bot. Go to
  the [Discord Developer Portal](https://discord.com/developers/applications), create an application representing your
  bot, then create a bot user and copy its token.
- `PREFIX` (optional, defaults to `!`) - the prefix for each bot command. For example, if you set it to `$`, then you
  would call `$ping` to ping the bot.
- `SQLITE_FILENAME` (mandatory) - a name for your SQLite database, for example `database.db`.
- `MODERATOR_GROUP` (optional) - the group ID assigned to moderator members.
- `SUPERUSER_GROUP` (optional) - the group ID assigned to the superuser.
- `ALLOW_GIT_UPDATE` (mandatory) - whether superusers can update the bot from git through Discord.

The two Group settings are optional, but without them any commands that require elevated permissions will not be able
to be called when DEBUG is false. Users with the superuser group are able to run every command (including some dangerous
ones), so caution should be used when setting these up.

If you have any difficulties setting it up, see the [contributor's guide](CONTRIBUTING.md) for a walkthrough.
