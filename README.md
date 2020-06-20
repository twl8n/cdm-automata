#### 

Sketch:

Automata for each of the four marketplace roles plus one to play the role of the environment. Framework to
support multiples of consumer, producer, and forecaster. The actual code that will resolve forecasts and
trades is used by the automata with the result being a simulated market based on the actual Coordisc code and
algos.

The "framework" can be a fifth automata?


#### Usage

The fastest launch is with babashka (aka bb), which is a Clojure interpreter.

https://github.com/borkdude/babashka

```bash
bb -cp src --main coordisc.core
```

This software sort of uses Leiningen. Leiningen installs its own private copy of Clojure (or something).

https://leiningen.org/#install

```bash
lein run
```

Alternatively install Clojure, and run coordisc via Clojure. 

https://clojure.org/guides/getting_started


```bash
clojure -m coordisc.core
```


#### todo CDM

* Add ideal clearing price to saved-history to make results easier to interpret
x create functions for bots: good-price and bad-price.
x add usage notes for lein, clojure, and bb.
* migrate away from lein to tools.deps.
* add more unit tests for forecast functions. Need to confirm behavior of every function.
* verify that rounding is always accounted for explicitly
* create one or more automata modules suitable to various simulations
* maybe create a front end to allow humans to interact with automata (bots).
* price 995 to 995 costs zero and succeeds? Maybe it should, but something needs to never do a zero move price change.


#### License TBD

Copyright © 2020 Noah Healy


