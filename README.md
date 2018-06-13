# same-same: ignore the differences between a and Identity a.

[![Build Status](https://api.travis-ci.org/isovector/same-same.svg?branch=master)](https://travis-ci.org/isovector/same-same) | [Hackage][hackage]

[hackage]: https://hackage.haskell.org/package/same-same


## Dedication

> The ego is only an illusion, but a very influential one. Letting the
> ego-illusion become your identity can prevent you from knowing your true self.
> Ego, the false idea of believing that you are what you have or what you do, is
> a backwards way of assessing and living life.
>
> Wayne Dyer


## Synopsis

I think [higher-kinded data][hkd] is a pretty cool guy. He reuses datatypes and
he isn't afraid of anything. But working with HKD isn't everything it's cracked
up to be -- it makes deriving instances hard, requires janky type families, and
in general doesn't fill you with any sense of joy.

[hkd]: http://reasonablypolymorphic.com/blog/higher-kinded-data/

Enter `same-same`: a compiler plugin that provides proofs of `a ~ Identity a`
and makes working with HKD a little less shit. Armed with this proof, we're able
to get rid of the HKD type family, and thus regain our derived instances.


## Unsoundness

This plugin is a teensy little bit **completely unsound** and when used for
evil, is capable of producing `forall a b. a ~ b` proofs. Fortunately, you need
to be actively trying to accomplish such a feat, and so you're probably going to
be a-ok.

Modules that haven't loaded the plugin are not affected by this "feature."


## Example

```haskell
{-# OPTIONS_GHC -fplugin=Data.Functor.Identity.Plugin #-}

module Test where

import Data.Functor.Identity

data X f = X
  { foo :: f Int
  }

getFoo :: X Identity -> Int
getFoo = foo
```


## Contact

Please reports bugs and missing features at the [GitHub bugtracker][issues]. This is
also where you can find the [source code][source].

`same-same` was written by [Sandy Maguire][me] and is licensed under a
permissive MIT [license][lic].

[me]: http://reasonablypolymorphic.me
[lic]: https://github.com/isovector/same-same/blob/LICENSE
[issues]: https://github.com/isovector/same-same/issues
[source]: https://github.com/isovector/same-same

