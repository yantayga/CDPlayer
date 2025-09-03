#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void* DBHandle;
typedef size_t WordId;
typedef unsigned short ShortWordId;

DBHandle initCoNLLUDB();
void clearCoNLLUDB(DBHandle h);

bool loadFile(DBHandle h, const char* path);
bool loadDirectory(DBHandle h, const char* path);

char* index2word(DBHandle h, const WordId ix);
WordId word2index(DBHandle h, const char* word);

char* index2tag(DBHandle h, const ShortWordId ix);
ShortWordId tag2index(DBHandle h, const char* word);

size_t wordsCount(DBHandle h);
size_t tagsCount(DBHandle h);

#ifdef __cplusplus
}
#endif
