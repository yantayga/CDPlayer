lib:
	g++ -Wall -Wpedantic -shared -fPIC -std=c++23 CoNLLU/CoNLLU.cpp `pkg-config --libs --cflags icu-uc icu-io` -o libconllu.so

test:
	g++ -Wall -Wpedantic -std=c++23 CoNLLU/Test.cpp -lconllu -L. -o Test

all: lib test

editor:
	ghc -no-keep-hi-files -no-keep-o-files -Wall -Wextra -O4 Editor/Main.hs -o editor
	strip editor

editor-prof:
	ghc -no-keep-hi-files -no-keep-o-files -rtsopts -prof -fprof-auto -ticky -fdistinct-constructor-tables -O4 Editor/Main.hs -o editor-prof
	./editor-prof +RTS -l-augT -hc
	eventlog2html editor-prof.eventlog

hlint:
	hlint -v --report --with-group=generalise-for-conciseness --with-group=extra --with-group=teaching .
	open report.html