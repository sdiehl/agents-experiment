agents
======

Toy language to simulate beer consumption, derived after too many beers with Stefan.

Usage
-----

```bash
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal configure
```

```bash
$ cabal run
Linking dist/build/agents/agents ...
                         _       
                        | |      
   __ _  __ _  ___ _ __ | |_ ___ 
  / _` |/ _` |/ _ \ '_ \| __/ __|
 | (_| | (_| |  __/ | | | |_\__ \
  \__,_|\__, |\___|_| |_|\__|___/
         __/ |  Version 0.0                 
        |___/   Type :help for help               

Agents> :help
:run          Run the loaded program
:reload       Run the active file
:show         Dump the AST of active file
:load <file>  Load a program from file

Agents> :load simple.world
Loading simple.world

Agents> :run
[Tick] Start 1
[Entity] station
[Cond] Gte (Var "timer") (Const 10) ==> Fl
[Action] INC: timer 1
[Entity] tourist
[Action] DEC: health 1
[Cond] Eq (Var "health") (Const 0) ==> Fl
[Cond] And (Lt (Var "health") (Const 5)) (Not (Var "thirsty")) ==> Tr
[Action] SET Tag "thirsty"
[Cond] And (Gt (Var "health") (Const 5)) (Var "thirsty") ==> Fl
[Cond] Var "beer" ==> Fl
[Tick] Finished 1
...

Agents> :show
[ ObjectDecl
    Object
      { _name = "restaurant"
      , _measure = [ Measure { _mname = "beer" , _mval = 100 } ]
      , _tags = fromList []
      , _actuators = []
      }
, ObjectDecl
    Object
      { _name = "station"
      , _measure = [ Measure { _mname = "timer" , _mval = 1 } ]
      , _tags = fromList []
      , _actuators =
          [ Actuator
              (If (Gte (Var "timer") (Const 10)))
              [ Create "tourist" , Zero "timer" ]
          , Actuator Always [ Inc "timer" 1 ]
          ]
      }
...

Agents> ^D
Goodbye.
```
