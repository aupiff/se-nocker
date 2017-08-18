se-nocker
=========

# Etymology

A pun on "reflexive nock" since reflexive French verbs have the forms like
"se ----er", e.g. "se raser" means "to shave oneself".

# Set-up

```
./reflex-platform/work-on ./ghcjs-packages.nix ./.
cabal configure --ghcjs
cabal build
```

Compiled javascript and `index.html` can be found in `dist/build/rn/rn.jsexe/` after a successful `cabal build`.
