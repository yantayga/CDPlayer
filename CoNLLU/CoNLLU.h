#include <string>
#include <vector>
#include <utility>
#include <functional>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <cmath>
#include <iostream>
#include <cstdint>

#include "CoNLLUci.h"

typedef std::pair<ShortWordId, ShortWordId> Feature;

struct CoNLLUWord
{
    WordId word;
    WordId initialWord;
    TagId tags;
    size_t depHead;
    ShortWordId depRel;
    ShortWordId depRelModifier;
};

struct CoNLLUSentence
{
    std::vector<CoNLLUWord> words;
};

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

    void printMap(void) const
    {
        for (const auto& p: item2index)
        {
            std::cout << p.first << " -> " << p.second << ", ";
        }
    };

    void printIndex(void) const
    {
        for (size_t i = 0; i < index2item.size(); ++i)
        {
            std::cout << "[" << i << "] = " << *(index2item[i]) << ", ";
        }
    };
};

union CompoundTag
{
    struct Verbose
    {
        unsigned char POS: 6;
        unsigned __int128 features : 118;
    } coumpoundTag;
    unsigned __int128 tag128;

    bool operator==(const CompoundTag& other) const
    {
        return tag128 == other.tag128;
    }
};

template <>
struct std::hash<CompoundTag>
{
  std::size_t operator()(const CompoundTag& k) const
  {
    return std::hash<uint64_t>{}((uint64_t)k.tag128 ^ (uint64_t)(k.tag128 >> 64));
  }
};

class CoNLLUDatabase
{
    struct FileStatistics
    {
        std::string fileName;
        size_t sentencesNum;
        size_t wordsNum;
    };

    struct Statistics
    {
        std::vector<FileStatistics> files;
        std::unordered_set<std::string> errors;
        size_t maxFeaturesNum;
    };

    Statistics statistics;

    const BidirectionalMap<std::string, ShortWordId> posTags;
    const BidirectionalMap<std::string, ShortWordId> featureNames;
    const BidirectionalMap<std::string, ShortWordId> featureValues;
    const BidirectionalMap<std::string, ShortWordId> depRels;
    const BidirectionalMap<std::string, ShortWordId> depRelModifiers;

    std::vector<CoNLLUSentence> sentences;

    BidirectionalMap<std::string, WordId> words;
    BidirectionalMap<CompoundTag, TagId> tags;

    WordId beginTag;
    WordId endTag;
    WordId unkTag;

public:
    CoNLLUDatabase();

    void reset(void);

    bool load(const std::string& fileName);
    bool loadDirectory(const std::string& directoryName);

    const std::string& index2word(const WordId ix) const;
    WordId word2index(const std::string& word);

    void printStatistics(void);
};
