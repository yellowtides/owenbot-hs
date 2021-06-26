# Becoming an OwenDev

Thanks for showing interest in contributing owo! 🎉

We accept everyone as contributors, no matter how experienced/inexperienced you are with Haskell, and no matter what connection you have with the existing devs. This bot exists primarily as a learning resource, and no rigorous "production"-level bureaucracy is in place, don't worry.

The following are some guidelines (not rules!) which may help you pave the way to becoming an OwenDev :) Please don't hesitate to ask any kind of questions! 💜

## Feature Request, Bug Report

Please use [GitHub Issues](https://github.com/yellowtides/owenbot-hs), or ping `@OwenDev` on the Informatics 2020 Discord server.
If you express interest (no screening, no requirement, only passion), we'll add you to the role too.

## Coding!

We exclusively use Haskell for our codebase, so being familiar with the language may help. But don't worry if you aren't!

## Style Guide

We're looking into an automatic formatter/linter that fits everyone's requirements, but for now, the following is fine:
- **Indentation**: 4 spaces, not tabs!
- **do-notation**: Unless super short, strongly encouraged for legibility
- **Documentation**: Haddock-style (see existing ones and improvise!)

## Testing!

Testing a Discord bot is quite difficult because there are so many side-effects (unlike the QuickCheck things we did in Inf1A). If a helper function is abstract enough to be tested, you're encouraged to create a Spec file (see existing ones for inspiration), and test with `stack/cabal test`.
