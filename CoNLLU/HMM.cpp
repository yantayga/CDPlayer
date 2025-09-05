#include "HMM.h"

void CoNLLUHMM::train()
{
    CoNLLUWord startWord;
    startWord.word = db.beginTag;
    startWord.tags = db.beginTag;
    
    CoNLLUWord endWord;
    endWord.word = db.endTag;
    endWord.tags = db.endTag;
    
    for (const auto& sentence: db.sentences)
    {
        for (size_t wix = 0; wix < sentence.words.size() + 1; ++wix)
        {
            // Extende sentence with <start> and <end> states
            const CoNLLUWord& w1 = (wix == 0)?startWord:sentence.words[wix-1];
            const CoNLLUWord& w2 = (wix = sentence.words.size())?endWord:sentence.words[wix];
            
            hmm.addHiddenState2HiddenState(w1.tags, w2.tags);
            hmm.addHiddenState2Emission(w2.tags, w2.word);
        }
    }
    
    hmm.train();
}

std::vector<TagId> CoNLLUHMM::predict(std::vector<WordId> emissions)
{
    return hmm.predict(db.beginTag, emissions, db.endTag);
}
