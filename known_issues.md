# Known issues

* Constant folding & propagation does not handle all patterns. Sometimes obvious optimization possibilities are missed  
    - More investigation needed to find all cases where constant is not properly propagated and folded.

* Current string table implementation is not compatible with rayon/parallelism. Need to make String sharing thread-safe
* Optimizer has not been updated for TAC changes, generally produces broken code
    * Disabled

* Generated assembly is very inefficient. Plan is to address during optimization rework

* Tech debt in various places:
   * semcheck is getting too large - need to definitely split constant folding into separate pass
   * Stack allocator also deals with converting code from three-address to two-address form where needed
   * ByteCode register representation is closely tied to X86 arch - need to generalize in order to support other architectures

* Known bugs are inside bugs/ directory


