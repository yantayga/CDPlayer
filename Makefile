lib-hmm: HMM/*
	g++ -Wall -Wpedantic -shared -fPIC -std=c++23 HMM/Matrix.cpp HMM/HMM.cpp -o libhmm.so  -g -pg

lib-conllu: lib-hmm CoNLLU/CoNLLU.cpp CoNLLU/CoNLLUSentence.cpp CoNLLU/Statistics.cpp CoNLLU/CoNLLUci.c CoNLLU/Serialize.cpp CoNLLU/HMM.cpp
	g++ -Wall -Wpedantic -shared -fPIC -std=c++23 CoNLLU/CoNLLU.cpp  CoNLLU/CoNLLUSentence.cpp CoNLLU/Statistics.cpp CoNLLU/CoNLLUci.c CoNLLU/Serialize.cpp CoNLLU/HMM.cpp `pkg-config --libs --cflags icu-uc icu-io` -lgzstream -lz -lhmm -L. -o libconllu.so  -g -pg

test-conllu: lib-conllu CoNLLU/Test.c
	gcc -Wall -Wpedantic CoNLLU/Test.c -lconllu -L. -o test-conllu -g -pg

test-conllu-hs: lib-conllu CoNLLU/*
	ghc -no-keep-hi-files -no-keep-o-files -Wall -Wextra -lconllu -L. -O4 CoNLLU/Main.hs -o test-conllu-hs

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