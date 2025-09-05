#include "CoNLLUci.h"
#include "CoNLLU.h"

DBHandle initCoNLLUDB()
{
    CoNLLUDatabase* pDB = new CoNLLUDatabase();
    pDB->reset();
    return DBHandle(pDB);
}

void clearCoNLLUDB(DBHandle h)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        delete pDB;
    }
}

bool loadFile(DBHandle h, const char* path)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        return pDB->load(path);
    }

    return false;
}

bool loadDirectory(DBHandle h, const char* path)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        return pDB->loadDirectory(path);
    }

    return false;
}

bool loadBinary(DBHandle h, const char* path, bool useSentences)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        return pDB->loadBinary(path, useSentences);
    }

    return false;
}

bool saveBinary(DBHandle h, const char* path, bool useSentences)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        return pDB->saveBinary(path, useSentences);
    }

    return false;
}

char* index2word(DBHandle h, const WordId ix)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        // Dirty hack, until GHC Capi provides smth for const char*
        return (char*)pDB->index2word(ix).c_str();
    }

    return NULL;
}

WordId word2index(DBHandle h, const char* word)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        return pDB->word2index(word);
    }

    return -1;
}

size_t wordsCount(DBHandle h)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        //return pDB->words.size();
    }

    return -1;
}

void train(DBHandle h, double smoothingFactor)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        return pDB->train(smoothingFactor);
    }
}

void printStatistics(DBHandle h)
{
    CoNLLUDatabase* pDB = (CoNLLUDatabase*)h;

    if (pDB)
    {
        return pDB->printStatistics();
    }
}
