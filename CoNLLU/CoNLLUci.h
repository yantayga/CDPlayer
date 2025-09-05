#ifdef __cplusplus
#pragma once
#endif

#include <stdbool.h>
#include <stddef.h>

#include "Types.h"

#ifdef __cplusplus
extern "C" {
#endif

DBHandle initCoNLLUDB();
void clearCoNLLUDB(DBHandle h);

bool loadFile(DBHandle h, const char* path);
bool loadDirectory(DBHandle h, const char* path);

bool loadBinary(DBHandle h, const char* path, bool useSentences);
bool saveBinary(DBHandle h, const char* path, bool useSentences);

char* index2word(DBHandle h, const WordId ix);
WordId word2index(DBHandle h, const char* word);

size_t wordsCount(DBHandle h);

void printStatistics(DBHandle h);

#ifdef __cplusplus
}
#endif
