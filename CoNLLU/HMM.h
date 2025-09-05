#include "CoNLLU.h"
#include "../HMM/HMM.h"

class CoNLLUHMM
{
    HMM<double, TagId, WordId> hmm;
    CoNLLUDatabase& db;
    CoNLLUWord serviceWord;

    void trainOnSentence(const CoNLLUSentence& sentence);
public:
    CoNLLUHMM(CoNLLUDatabase& _db)
        : hmm(_db.tags.size(), _db.words.size())
        , db(_db)
    {
        CoNLLUWord serviceWord;
        serviceWord.word = db.serviceTag;
        serviceWord.tags = db.serviceTag;
    }

    void train();

    std::vector<TagId> predict(std::vector<WordId> emissions) const;
};
