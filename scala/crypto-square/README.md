# Crypto Square

Implement the classic method for composing secret messages called a square code.

Given an English text, output the encoded version of that text.

First, the input is normalized: the spaces and punctuation are removed
from the English text and the message is downcased.

Then, the normalized characters are broken into rows.  These rows can be
regarded as forming a rectangle when printed with intervening newlines.

For example, the sentence

> If man was meant to stay on the ground, god would have given us roots.

is normalized to:

> ifmanwasmeanttostayonthegroundgodwouldhavegivenusroots

The plaintext should be organized in to a rectangle.  The size of the
rectangle (`r x c`) should be decided by the length of the message,
such that `c >= r` and `c - r <= 1`, where `c` is the number of columns
and `r` is the number of rows.

Our normalized text is 54 characters long, dictating a rectangle with
`c = 8` and `r = 7`:

```plain
ifmanwas
meanttos
tayonthe
groundgo
dwouldha
vegivenu
sroots
```

The coded message is obtained by reading down the columns going left to
right.

The message above is coded as:

```plain
imtgdvsfearwermayoogoanouuiontnnlvtwttddesaohghnsseoau
```

Output the encoded text in chunks.  Phrases that fill perfect squares
`(r X r)` should be output in `r`-length chunks separated by spaces.
Imperfect squares will have `n` empty spaces.  Those spaces should be distributed evenly across the last `n` rows.

```plain
imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn sseoau
```

Notice that were we to stack these, we could visually decode the
cyphertext back in to the original message:

```plain
imtgdvs
fearwer
mayoogo
anouuio
ntnnlvt
wttddes
aohghn
sseoau
```

The Scala exercises assume an SBT project scheme. The exercise solution source
should be placed within the exercise directory/src/main/scala. The exercise
unit tests can be found within the exercise directory/src/test/scala.

To run the tests simply run the command `sbt test` in the exercise directory.

For more detailed info about the Scala track see the [help
page](http://exercism.io/languages/scala).

## Source

J Dalbey's Programming Practice problems [http://users.csc.calpoly.edu/~jdalbey/103/Projects/ProgrammingPractice.html](http://users.csc.calpoly.edu/~jdalbey/103/Projects/ProgrammingPractice.html)

## Submitting Incomplete Problems
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

