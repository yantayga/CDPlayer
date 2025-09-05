#include "HMM.h"
#include "CoNLLU.h"
#include "Serialize.h"

void CoNLLUHMM::trainOnSentence(const CoNLLUSentence& sentence)
{
    hmm->addHiddenState2HiddenState(serviceWord.tags, sentence.words[0].tags);
    hmm->addHiddenState2Emission(sentence.words[0].tags, sentence.words[0].word);

    for (size_t wix = 1; wix < sentence.words.size(); ++wix)
    {
        hmm->addHiddenState2HiddenState(sentence.words[wix-1].tags, sentence.words[wix].tags);
        hmm->addHiddenState2Emission(sentence.words[wix].tags, sentence.words[wix].word);
    }

    hmm->addHiddenState2HiddenState(sentence.words[sentence.words.size() - 1].tags, serviceWord.tags);
    hmm->addHiddenState2Emission(sentence.words[0].tags, serviceWord.word);
}

void CoNLLUHMM::train(CoNLLUDatabase& db, double smoothingFactor)
{
    serviceWord.word = db.serviceTag;
    serviceWord.tags = db.serviceTag;

    hmm = std::make_unique<HMM<double, TagId, WordId>>(db.tags.size(), db.words.size());

    trainOnSentence(db.unkWordOnly);

    for (const auto& sentence: db.sentences)
    {
        trainOnSentence(sentence);
    }

    hmm->normalize(smoothingFactor);
}

std::vector<TagId> CoNLLUHMM::predict(std::vector<WordId> emissions) const
{
    if (!hmm)
        return std::vector<TagId>();

    return hmm->predict(serviceWord.word, emissions);
}

void CoNLLUHMM::saveBinary(std::ostream& stream) const
{
    serialize(stream, serviceWord);
    //hmm.saveBinary(stream);
}
