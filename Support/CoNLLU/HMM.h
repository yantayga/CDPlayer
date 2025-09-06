#pragma once

#include <memory>
#include "CoNLLUSentence.h"
#include "../HMM/HMM.h"

class CoNLLUDatabase;

class CoNLLUHMM
{
    std::unique_ptr<HMM<double, TagId, WordId>> hmm;
    CoNLLUWord serviceWord;

    void trainOnSentence(const CoNLLUSentence& sentence);
public:
    CoNLLUHMM() {};

    void train(CoNLLUDatabase& db, double smoothingFactor);

    std::vector<TagId> predict(std::vector<WordId> emissions) const;

    void saveBinary(std::ostream& stream) const;
};
