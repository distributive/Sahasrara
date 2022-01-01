# Contributing

Welcome to the [University of Warwick Tabletop and Roleplaying Society](https://warwicktabletop.co.uk)'s Discord server bot! You can join our Discord server [here](https://warwicktabletop.co.uk/discord)! You can access the channel for developer discussion by giving yourself the @computer_person role.

Please feel free to contribute to the code in whatever way you are able. We're more than happy to accept new code, bug fixes, documentation, issues, and anything else you think will improve the bot! If you do start work on an issue, please first let us know in the issue's thread or in our Discord server to avoid competing pull requests.

## Code of Conduct

Our society has a [Code of Conduct](https://www.warwicktabletop.co.uk/page/codeofconduct/). We expect it to be upheld by contributors in all our online spaces the same way we'd expect it to be upheld by attendees of our events.

## Good Practice Recommendations

Here are some good practice tips for git and this project.

* Whenever a new feature is being worked on, create a new branch on your forked repo
* When a feature is ready to be merged into the bot, make a pull request from the feature branch to the main repo
* Before making a pull request, make sure your branch is up to date with main (and that it compiles and complies with `ormolu` - see below for details) so that it can be merged without hassle
* Write comments! This project will be maintained by many people, and it can be difficult to work out what others' code does
* To communicate with the maintainers, please join the [Tabletop Discord server](https://warwicktabletop.co.uk/discord) and give yourself the @computer_person role to join the developer channel
* If you need help finding a function to do a particular task, you can search on [Hoogle](https://hoogle.haskell.org/). The two libraries that deal with parsing are `Text.Megaparsec` and `Control.Monad.Combinators`, and `Discord` is the package that deals with Discord itself. You can filter by package if that helps to find certain functions

You can check out the [README](README.md) for a brief overview on how to set up a local bot for testing. If you've never done something like this before, see the bottom of this document for a walkthrough.

## Ormolu

To maintain consistent formatting you must use Ormolu, which can be installed via stack:

`stack install ormolu`

Then you can run it on every file via:

`ormolu --mode inplace $(git ls-files '*.hs')`

You can see full documentation on the [Ormolu repo](https://github.com/tweag/ormolu#usage).

### Running Ormolu automatically with git

You may also wish to set up Ormolu to run when you stage a file (get ready to commit it) - this can be done using `.gitattributes` and `.gitconfig` as follows.

1. We need to define what runs on commit, which is done via a filter. Add the following to `~/.gitconfig`:

    ```gitconfig
    [filter "haskell-linting"]
        clean = ormolu
        smudge = cat
    ```

    This globally defines a filter that you can call from individual git respositories.

2. Now we need to tell this git repository that it should run this filter. Simply add the following to `.gitattributes`. Note that this has been added to `.gitignore` to avoid people who don't want this filter being forced to use it.

    ```gitattributes
    *.hs filter=haskell-linting
    ```

That's it! (With thanks to Sam Coy for explaining this process)

## What can I work on?

We're happy to accept any contribution, big or small. You can find our list of issues [here](https://github.com/WarwickTabletop/tablebot/issues). If you think of a feature you'd like added or a bug in the current implementation please do create a new ticket! There's no obligation to implement the issue. If you don't have any ideas but do want to get involved with programming you can check the issues page for new features and fixes to work on. If you're not too familiar with Haskell or our codebase, look out for the "good first issue" label. We put this on issues that we think would be good for newcomers to the language/project to get started on.

If you have trouble at any time, please do ask for help in an issue thread or on our Discord. You can also check out the [tutorials](tutorials) in the repository and pre-existing solutions in the code for guidance.

## Setup from Scratch

If at any point something doesn't work, restart your computer first and try it again. If the problem persists please feel free to ask for help in the [Discord server](https://www.warwicktabletop.co.uk/discord/). Sections are marked depending on what OS they rely on, if any.

1. git, wsl, and vscode setup
    1. github
        1. Create a GitHub account
        2. Go to <https://github.com/WarwickTabletop/tablebot>
        3. Click fork on the repo (should be top right) (this creates your own version of the repo)
        4. Take note of the url that your forked repo is on
    2. wsl and git (Windows)
        1. Install wsl by going to <https://docs.microsoft.com/en-us/windows/wsl/install>, and make sure it's in the right click context menu of folders
        2. Navigate to an empty folder on your computer that you want to do your programming from (the project folder)
        3. Shift-right click in the project folder, and click "open linux shell here"
        4. Type `git clone <your repo url>` into the terminal
        5. The folder should be filled with a bunch of files and folders
    3. terminal and git (Linux)
        1. Navigate to an empty folder on your computer that you want to do your programming from (the project folder)
        2. Shift-right click in the project folder and press "open in terminal"
        3. Type `git clone <your repo url>` into the terminal
        4. The folder should be filled with a bunch of files and folders
    4. vscode
        1. Install vscode from <https://code.visualstudio.com/>
        2. Install this <https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack>
        3. From a terminal opened in the project folder, type `code .`
        4. There should soon be a window which has all the folders and files open on the left hand side
2. Haskell setup
    1. In any linux terminal window (wsl or the linux terminal itself), type `curl -sSL https://get.haskellstack.org/ | sh`, allowing sudo access and providing passwords as needed
    2. In the linux terminal window opened from the project folder (or the terminal open in the vscode window) run `stack build`, and then wait until it's done
    3. This will take a long time
    4. Make some tea, or maybe some coffee or hot chocolate
    5. If it didn't work, reopen all terminal windows and try again. if that doesn't work, restart your computer and try again
    6. Install this <https://marketplace.visualstudio.com/items?itemName=haskell.haskell>
    7. Open a file and marvel at the colours, and the fact you can hover over things and see values and stuff
3. Discord and Environment variables
    1. Create a file in the top level of the project folder called `.env`, based on the template in `.env.example`
    2. Follow the instructions in [Environment File Setup](README.md#environment-file-setup) to fill in the `.env`. Make sure to get a `DISCORD_TOKEN` and a `SQLITE_FILENAME` (which can be named anything, but use something like `database.db`)
    3. To run the bot, type `stack run` into the terminal, and the bot will start to run
    4. Make sure to invite the bot to a server so you can test it out!

Congratulations, you now know the very basics needed to set up your own tablebot!

To learn more about git, you should look up a tutorial or watch this video: <https://youtu.be/HOIC804Berc>
