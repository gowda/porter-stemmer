# porter-stemmer

Implementation of stemmer as per M.F.Porter's [algorithm](http://tartarus.org/~martin/PorterStemmer/def.txt).

# Caveats

Actual algorithm treats a 'y' preceded by a consonant as a vowel. This program does not treat 'y' so.

# Usage

On a REPL, use `stem-it` procedure to obtain the stem of a string.
    (stem-it "generalization")

NOTE: ensure that string is in lower-case before passing to `stem-it`.

## License

Copyright (C) 2012 Basavanagowda Kanur

Distributed under the Eclipse Public License, the same as Clojure.
