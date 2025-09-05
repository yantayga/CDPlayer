#pragma once

#include <string>
#include <vector>
#include <utility>
#include <functional>
#include <memory>
#include <unordered_map>
#include <cmath>

#include "Types.h"
#include "Statistics.h"
#include "CoNLLUSentence.h"
#include "HMM.h"

template <class Item, class Index>
class BidirectionalMap
{
    std::unordered_map<Item, Index> item2index;
    std::vector<const Item*> index2item;

public:
    BidirectionalMap() {};
    BidirectionalMap(const std::vector<Item> items);

    void clear(void);

    size_t size(void) const { return index2item.size(); };
    size_t bits(void) const { return std::ceil(std::log2(size())); };

    const Index lookupOrInsert(const Item& item);
    const Index lookup(const Item& item) const;
    const Item& lookupIndex(const Index index) const;

    void saveBinary(std::ostream& stream) const;
    void loadBinary(std::istream& stream);
};

class CoNLLUDatabase
{
    friend class CoNLLUHMM;

    Statistics statistics;

    const BidirectionalMap<std::string, ShortWordId> posTags;
    const BidirectionalMap<std::string, ShortWordId> featureNames;
    const BidirectionalMap<std::string, ShortWordId> featureValues;
    const BidirectionalMap<std::string, ShortWordId> depRels;
    const BidirectionalMap<std::string, ShortWordId> depRelModifiers;

    std::vector<CoNLLUSentence> sentences;

    BidirectionalMap<std::string, WordId> words;
    BidirectionalMap<CompoundTag, TagId> tags;

    WordId serviceTag = 0;

    CoNLLUSentence unkWordOnly;
    CoNLLUWord unknownWord;
    
    CoNLLUHMM hmm;

    std::vector<WordId> encodeWords(const std::vector<std::string>& words) const;
public:
    CoNLLUDatabase();

    void reset(void);

    bool load(const std::string& fileName);
    bool loadDirectory(const std::string& directoryName);

    bool loadBinary(const std::string& fileName, bool useSentences);
    bool saveBinary(const std::string& fileName, bool useSentences) const;

    const std::string& index2word(const WordId ix) const;
    WordId word2index(const std::string& word) const;

    void train(double smoothingFactor);

    void printStatistics(void);
};
