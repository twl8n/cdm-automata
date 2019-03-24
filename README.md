#### cdm automata

Tech: SQLite, Clojure

Sketch:

Automata for each of the four marketplace roles plus one to play the role of the environment. Framework to
support multiples of consumer, producer, and forecaster.

The "framework" can be a fifth automata.

#### todo machine

* demo4 fails, perhaps because will-not-dashboard doesn't exist.
Maybe if table has any fail during parsing, then exit.

* upgrade state node vectors to a map for the sake of debugging, and
get rid of nth and assoc with seq indexes. The indexes are very bug prone.

* x test.clj change to hash of seq of seq where key is edge name and seqs are states for that edge.

* x test.clj munge strings to functions (or symbols) at parse time
or leave them as strings and munge-eval at runtime

* x port test.clj to core.clj

* x get it all working, run demos

* + make traverse return a map with fres, final state, etc. so the calling code can always determine what happened

* fres depleted when not in wait is an error, and the machine should halt or something.

* x sub-table returns only the current edges

* save or return the final state when we halted (wait). Return this to the top level.

* ? recursing on traverse needs to send the new edge and jumpstack, but the table global will be used by the
next call to sub-table

* x execution halts on wait, or running out of anything (edges, states, etc.)

* x add traverse needs to return when st (sub-table ...) is consumed.

* sanity check, warn if next-state-edgte (column 4) is not empty when func is wait.

* sanity check can't reach the 3rd line:

(The test of the second line is always true, so that will always fire.

| login          | if-logged-in |                          | pages           |
| login          |              | draw-login               | login-wait      |
| login          |              | wait                     | login           |

* exit kills the repl, need something (like an exception) to exit clj, but leave the repl intact.

(System/exit 0)

#### usage

(def logged-in-state true)
(demo)

Retur continues. Hit ^D several times to cancel. (demo) is in a read-line loop.

#### notes

(defn foo [xx] (str "this is foo " xx))
(def bar (eval (read-string "{:func foo}")))
((:func bar) 22)

(def bar {:func (eval 'foo)})

(def bar {:func (eval (symbol "foo"))})

x

Workflow state machine ported to Clojure

;; turn this into a function, run on the seq of maps that is the state table from read-state-file
;;   (map (fn [x] (if (not (= "" (x :test))) (assoc x :testx (eval (read-string (x :test)))) x)) table)


#### github mirror push 

When cloning a repo, github docs recommend:

$ git push --mirror https://github.com/exampleuser/new-repository.git

The command above does not use ssh, and will prompt for a userid and password. 

but you almost certainly want:

> git push --mirror git@github.com:exampleuser/new-repository.git

This command will use your nomral github ssh credentials.

#### License

Copyright © 2017 Tom Laudeman


