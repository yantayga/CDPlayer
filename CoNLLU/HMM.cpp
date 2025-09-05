#include "HMM.h"

void CoNLLUHMM::trainOnSentence(const CoNLLUSentence& sentence)
{
    for (size_t wix = 0; wix < sentence.words.size() + 1; ++wix)
    {
        // Extende sentence with start and stop states
        const CoNLLUWord& w1 = (wix == 0)?serviceWord:sentence.words[wix-1];
        const CoNLLUWord& w2 = (wix = sentence.words.size())?serviceWord:sentence.words[wix];

        hmm.addHiddenState2HiddenState(w1.tags, w2.tags);
        hmm.addHiddenState2Emission(w2.tags, w2.word);
    }
}

void CoNLLUHMM::train()
{
    trainOnSentence(db.unkWordOnly);
    
    for (const auto& sentence: db.sentences)
    {
        trainOnSentence(sentence);
    }

    hmm.train();
}

std::vector<TagId> CoNLLUHMM::predict(std::vector<WordId> emissions) const
{
    return hmm.predict(db.serviceTag, emissions);
}
