# tablebot
[![CI](https://github.com/WarwickTabletop/tablebot/actions/workflows/main.yml/badge.svg)](https://github.com/WarwickTabletop/tablebot/actions/workflows/main.yml)

An extendable Discord bot framework written on top of `discord-haskell`

## Setup from Scratch

If at any point something doesn't work, restart your computer first and try again, and then ask Benji (or someone else) for guidance.

1. git, wsl, and vscode
  1. github
    1. Create a GitHub account
    2. Go to <https://github.com/WarwickTabletop/tablebot>
    3. Click fork on the repo (should be top right) (this creates your own version of the repo)
    4. Take note of the url that your forked repo is on
  2. wsl and git
    1. Install wsl by going to <https://docs.microsoft.com/en-us/windows/wsl/install>, and make sure it's in the right click context menu of folders
    2. Navigate to an empty folder on your computer that you want to do your programming from (the project folder)
    3. Shift-right click in the project folder, and click "open linux shell here"
    4. Type `git clone <your repo url>` into the terminal
    5. The folder should be filled with a bunch of files and folders
  3. vscode
    1. Install vscode from <https://code.visualstudio.com/>
    2. Install this <https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack>
    3. From a terminal opened in the project folder, type `code .`
    4. There should soon be a window which has all the folders and files open on the left hand side
2. haskell beginnings
  1. in any linux terminal window, type `curl -sSL https://get.haskellstack.org/ | sh`, allowing sudo access and providing passwords as needed
  2. in the linux terminal window which was opened from the project folder (or the terminal that is open in the vscode window) run `stack build`, and then wait until it's done
  3. this will take a long time
  4. make some tea, or maybe some coffee or hot chocolate
  5. if it didn't work, reopen all terminal windows and try again. if that doesn't work, restart your computer and try again
  6. install this <https://marketplace.visualstudio.com/items?itemName=haskell.haskell>
  7. open a file and marvel at the colours, and the fact you can hover over things and see values and stuff
3. discord
  1. create a file in the top level of the project folder called `.env`, based on the template in `.env.example`
  2. follow the instructions in <https://github.com/WarwickTabletop/tablebot#readme> to fill in the `.env`. make sure to get a `DISCORD_TOKEN` and a `SQLITE_FILENAME` (which can be named anything, but use something like `database.db`)
  3. to run the bot, type `stack run` into the terminal, and the bot will start to run
  4. make sure to invite the bot to a server so you can test it out!

congratulations, you now know the very basics needed to set up your own tablebot!

To learn more about git, you should look up a tutorial or look at this video: <https://youtu.be/HOIC804Berc>



## Environment file setup

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

You can see full documentation on the [Ormolu repo](https://github.com/tweag/ormolu#usage).