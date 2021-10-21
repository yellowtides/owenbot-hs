<div align="center">
<img alt="Owenbot Logo" src="https://i.imgur.com/oDFn4Ep.png" />

</div>

# owenbot-hs
![](https://tokei.rs/b1/github/yellowtides/owenbot-hs)

A neat Discord bot written in Haskell. Use with caution.

## Features

- Serves over 500 students in the Informatics 2020-2021 Discord server at the University of Edinburgh
- Prefix: `:` (colon), inspired by Haskell GHCi
- Various commands to help studies such as `:thm`, or `:syllogisms`
- Various enjoyable utility functionality such as a built-in owo-ifier
- Admin commands to configure some settings at runtime
- Has a modular and extensible codebase that abstracts away boilerplate

## Running/Developing Locally

Place a "config.json" file in `~/.config/owen` (or `%APP_DATA%/owen` on Windows). Format is as given in "Config.hs".

All source code is formatted using [brittany](https://hackage.haskell.org/package/brittany).

```
cd src
find . -type f -name "*.hs" -exec brittany --write-mode=inplace --config-file ../brittany.yaml {} \;
```

### Cabal

Install and build all dependencies, and run the bot all with the command: `cabal run owenbot-exe`.

Run tests with `cabal test`.

### Stack

Build with `stack install`. This compiles and installs the bot in one go. Run with `owenbot-exe`.

Alternatively, build and run with `stack run owenbot-exe`.

Run tests with `stack test`.

### Nix

If you have nix installed,
running `stack install --nix`
will automatically ensure you get an environment with the correct
system-level dependencies (namely zlib).


### Docker

There is a `Dockerfile` included in this repo that basically runs the Stack build/exec process.
However, it has not been used so the functionality cannot be vouched for.

## Documentation | Commands

Try sending `:helpme` to the bot!

Generate Haddock documentation:
`stack haddock . --haddock-hyperlink-source`
