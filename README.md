# porter-stemmer

Implementation of stemmer as per M.F.Porter's [algorithm](http://tartarus.org/~martin/PorterStemmer/def.txt).
This implementation does not take any deviations from the actual algorithm, but this not a direct translation of the ANSI C implementation.

# Usage

On a REPL, use `stem-it` procedure to obtain the stem of a string.
    
    (stem-it "generalization")

NOTE: ensure that string is in lower-case before passing to `stem-it`.

## License

Copyright (C) 2012 Basavanagowda Kanur

Distributed under the Eclipse Public License, the same as Clojure.
