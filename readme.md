# What is it?
A Scala version of git@github.com:swlaschin/DomainModelingMadeFunctional.git
Some more flavors will come. Plan is to use idiomatic Scala version as opposed to F# in Scala.
Next thing to come is to use `ZLayer` instead of curried functions DI.

JSON won't be implemented.

## Type annotations
Scala requires more of them than F#. It has pros and cons.
I, as a programmer, had problem with inferring type `OrderQuantity.create` `productCode` parameter and is other places.
Parameter name is usually enough for me.
Another value I see in type ascriptions is that F# requires more comments.

## 'let' vs 'val' and 'def'
Another difference is that F# uses `let` and that's it.
In Scala I need two things: `val` and `def`. The second one usually when I need type params.

## curried syntax
I use it in most places here, I don't like it. Scala is not Haskell nor F#.
Which one looks better for non-programmer? I don't know. I don't know any non-programmer :-)

## 'Result'
I added `Result` like in F# original version, however I kept error on left side.
I didn't add `AsyncResult` - too much hassle. I'm using `IO` from `ZIO`.
Somehow I couldn't make type aliases to be visible everywhere (however I didn't try hard), that's a pity.

## Dotty
At some point I'll try to rewrite with opaque types and sum types...

## Files
I kept files almost like in F# original. Awkward in Scala I think. I merged SimpleTypes and CompoundTypes in package object.

## End result
I don't think it's a code that domain expert would event try to comprehend.
However for me it's much better than what I usually produce.

