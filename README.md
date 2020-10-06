Reference implementation of the hashsplit spec. WIP.

This hews as closely to the spec as possible, the goal being to have
an "executable specification" so we can be confident that the spec says
what we think it says, and so we have something to test production
implementations against.

The implementation is in Haskell, which was chosen as this makes it
easier to transliterate a declarative mathematical description into
code.

You probably do not want to actually use this implementation; among
other things, it is slow, because it omits optimizations that real
implementations will want to perform. If you are looking for a Haskell
implementation for production use, see
<https://github.com/hashsplit/haskell-hashsplit>.
