se-nocker
=========

# Etymology

a pun on "reflexive nock" since reflexive French verbs have the forms like
"se ----er", e.g. "se raser" means "to shave oneself".

# Set-up

```
cabal configure --ghcjs
./reflex-platform/work-on ghcjs ./. --run "ghcid -c'cabal repl'"
```

using current submodule scheme:

```
./reflex-platform/work-on ./ghcjs-packages.nix ./.
```
