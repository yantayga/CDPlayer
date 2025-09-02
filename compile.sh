ghc -no-keep-hi-files -no-keep-o-files -Wall -Wextra -O4 Editor/Main.hs -o editor
strip editor
ghc -no-keep-hi-files -no-keep-o-files -rtsopts -prof -fprof-auto -ticky -fdistinct-constructor-tables -O4 Editor/Main.hs -o editor-prof

#./Test +RTS -l-augT -hc
#eventlog2html Test.eventlog
