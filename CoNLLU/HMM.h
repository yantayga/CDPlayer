#pragma once

#include "CoNLLUSentence.h"
#include "../HMM/HMM.h"

class CoNLLUDatabase;

class CoNLLUHMM
{
    HMM<double, TagId, WordId> hmm;
    CoNLLUDatabase& db;
    CoNLLUWord serviceWord;

    void trainOnSentence(const CoNLLUSentence& sentence);
public:
    CoNLLUHMM(CoNLLUDatabase& _db);
    void train(double smoothingFactor);

    std::vector<TagId> predict(std::vector<WordId> emissions) const;
};
