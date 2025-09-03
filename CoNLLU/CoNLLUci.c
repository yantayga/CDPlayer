#include "CoNLLUci.h"
#include "CoNLLU.h"

__attribute__((visibility("default"))) DBHandle initCoNLLUDB()
{
    CoNLLUDatabase* pDB = new CoNLLUDatabase();
    pDB->reset();
    return DBHandle(pDB);
}

__attribute__((visibility("default"))) void clearCoNLLUDB(DBHandle h)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        delete pDB;
    }
}

__attribute__((visibility("default"))) bool loadFile(DBHandle h, const char* path)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        return pDB->load(path);
    }

    return false;
}

__attribute__((visibility("default"))) bool loadDirectory(DBHandle h, const char* path)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        return pDB->loadDirectory(path);
    }

    return false;
}
