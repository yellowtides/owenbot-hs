<div align="center">
<img alt="Owenbot Logo" src="https://i.imgur.com/oDFn4Ep.png" />

</div>

# owenbot-hs 

A neat Discord bot written in Haskell. Use with caution.

## Features

- Serves over 500 students in the Informatics 2020-2021 Discord server at the University of Edinburgh
- Prefix: `:` (colon), inspired by Haskell GHCi
- Various commands to help studies such as `:thm`, or `:syllogisms`
- Various enjoyable utility functionality such as a built-in owo-ifier
- Admin commands to configure some settings at runtime
- Has a modular and extensible codebase that abstracts away boilerplate

### Cabal

Install and build all dependencies, and run the bot all with the command: `cabal run owenbot-exe`.

Run tests with `cabal test`.

### Stack

Build with `stack install`. This compiles and installs the bot in one go.

Run with `owenbot-exe`.

Run tests with `stack test`.

## Running Locally

Place a text file called `.token.txt` containing your bot's Discord token in `~/.config/owen` (or `%APP_DATA%/owen` on Windows).

Execute with `owenbot-exe`

### Docker

There is a `Dockerfile` included in this repo that basically runs the Stack build/exec process.
However, it has not been used so the functionality cannot be vouched for.

## Documentation | Commands

Try sending `:helpme` to the bot!

Generate Haddock documentation:
`stack haddock . --haddock-hyperlink-source`
