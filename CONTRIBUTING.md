# Becoming an OwenDev

Thanks for showing interest in contributing owo! 🎉

We accept mostly everyone as contributors,
no matter what connection you have with the project or the existing devs.
However we do require that you have some prior Haskell knowledge,
as the bot uses some fairly advanced concepts
(if you took inf1a, you're mostly there).
No rigorous "production"-level bureaucracy is in place, don't worry.

The following are some guidelines (not rules!) which may help you pave the way to becoming an OwenDev :)
Please don't hesitate to ask any kind of questions! 💜

## Feature Request, Bug Report

Please use [GitHub Issues](https://github.com/yellowtides/owenbot-hs),
or ping `@OwenDev` on the Informatics 2020 Discord server.
If you express interest (no screening, no requirement, only passion),
we'll add you to the role too.

## Language

We exclusively use Haskell for our codebase,
so you must have some level of familiarity with the language.
We're happy to explain the more convoluted parts of our codebase to new contributors,
although in some (only a few!) cases this will require some background theory.

If you want to contribute, but don't have any Haskell experience,
we recommend going through [Learn You A Haskell](http://learnyouahaskell.com/),
after which you'll easily be able to contribute to our codebase.

## Style Guide

We're looking into an automatic formatter/linter that fits everyone's requirements,
but for now, the following is fine:
- **Indentation**: 4 spaces, not tabs!
- **do-notation**: Unless super short, strongly encouraged for legibility
- **Documentation**: Haddock-style (see existing ones and improvise!)

## Testing!

Testing a Discord bot is quite difficult
because there are so many side-effects
(unlike the exclusively pure functions we did in inf1A).
If a helper function is abstract enough to be tested,
you're encouraged to create a Spec file
(see existing ones for inspiration),
and test with `stack/cabal test`.
