#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void* DBHandle;

DBHandle initCoNLLUDB();
void clearCoNLLUDB(DBHandle h);

bool loadFile(DBHandle h, const char* path);
bool loadDirectory(DBHandle h, const char* path);

#ifdef __cplusplus
}
#endif
