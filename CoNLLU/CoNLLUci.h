#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void* DBHandle;
typedef size_t WordId;
typedef unsigned short TagId;
typedef unsigned char ShortWordId;

DBHandle initCoNLLUDB();
void clearCoNLLUDB(DBHandle h);

bool loadFile(DBHandle h, const char* path);
bool loadDirectory(DBHandle h, const char* path);

char* index2word(DBHandle h, const WordId ix);
WordId word2index(DBHandle h, const char* word);

size_t wordsCount(DBHandle h);

void printStatistics(DBHandle h);

#ifdef __cplusplus
}
#endif
