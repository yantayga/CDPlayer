lib-conllu: Support/*
	g++ -Wall -Wpedantic -shared -fPIC -std=c++23 Support/CoNLLU/CoNLLU.cpp  Support/CoNLLU/CoNLLUSentence.cpp Support/CoNLLU/Statistics.cpp Support/Serialize/Serialize.cpp Support/CoNLLU/HMM.cpp Support/HMM/Matrix.cpp Support/HMM/HMM.cpp Support/Support.c `pkg-config --libs --cflags icu-uc icu-io` -lgzstream -lz -L. -o libsupport.so -g -O4

test-conllu: lib-conllu Tests/Test.c
	gcc -Wall -Wpedantic Tests/Test.c -lsupport -L. -o test-conllu -O4

test-conllu-hs: lib-conllu Tests/Main.hs
	ghc -no-keep-hi-files -no-keep-o-files -Wall -Wextra -lconllu -L. -O4 Tests/Main.hs -o test-conllu-hs

all-conllu: lib-conllu test-conllu test-conllu-hs

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