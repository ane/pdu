pdu
===

Parallel experiment of the unix "du -sh" command.

It works by taking a directory listing of some directory and then recursively 
running itself in parallel on all subdirectories, and pretty printing the output 
(hence 'sh').

The point of the experiment that in Haskell parallellization can be turned off 
by using just `mapM` instead of `MP.mapM`.
