# Known issues

* Constant folding & propagation does not handle all patterns. Sometimes obvious optimization possibilities are missed  
    - More investigation needed to find all cases where constant is not properly propagated and folded.

* Current string table implementation is not compatible with rayon/parallelism. Need to make String sharing thread-safe

* Optimizer has not been updated for TAC changes, generally produces broken code

