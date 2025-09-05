#include "CoNLLU.h"
#include "../HMM/HMM.h"

class CoNLLUHMM
{
    HMM<double, TagId, WordId> hmm;
    CoNLLUDatabase& db;

public:
    CoNLLUHMM(CoNLLUDatabase& _db)
        : hmm(_db.tags.size(), _db.words.size())
        , db(_db)
    {
    }

    void train();

    std::vector<TagId> predict(std::vector<WordId> emissions) const;
};
