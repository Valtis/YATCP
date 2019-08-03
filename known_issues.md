# Known issues

* Error printer does not handle tabs correctly; diagnostic arrows can be misplaced. 
    - Fix candidate: Always convert tabs to spaces before printing lines with errors/notes. 

* Constant folding & propagation does not handle all patterns. Sometimes obvious optimization possibilities are missed  
    - More investigation needed to find all cases where constant is not properly propagated and folded.

* !, && and || are not implemented for booleans

* Redundant boolean comparsions are not implemented (boolean\_var == true/false)

* Cannot return constant true/false from functions

* Cannot use boolean-returing functions in while/if statements.

* Current string table implementation is not compatible with rayon/parallelism. Need to make String sharing thread-safe
