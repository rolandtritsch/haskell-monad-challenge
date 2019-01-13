# The code for/behind [the monad challenges](http://mightybyte.github.io/monad-challenges)

To make this work you need to ...

* install git and clone this repo
* install [stack]() and run `stack build` and `stack test`
* you can then run individual examples with `stack exec example1`, ...

You can also compile/run this with [eta](https://eta-lang.org/) ...

* install [etlas](https://github.com/typelead/etlas)
* run `etlas test`
* run `etlas run example01

Note: You still need to install stack first and need to run `stack build --dry-run` to create the `cabal` file from `package.yaml` (or you need to install/run `hpack` to create the `cabal` file).