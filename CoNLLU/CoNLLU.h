#include <string>
#include <vector>
#include <utility>
#include <functional>
#include <memory>
#include <unordered_map>

#include <iostream>

#include "CoNLLUci.h"

typedef std::pair<WordId, WordId> Feature;

struct CoNLLUWord
{
    WordId word;
    WordId initialWord;
    ShortWordId uPOSTag;
    std::vector<Feature> features;
    size_t depHead;
    ShortWordId depRel;
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
    __attribute__((noinline)) void clear(void);
    __attribute__((noinline)) size_t size(void) { return index2item.size(); };
    __attribute__((noinline)) const Index lookupOrInsert(const Item& item);
    __attribute__((noinline)) const Item& lookupIndex(const Index index);

    void printMap(void)
    {
        for (const auto& p: item2index)
        {
            std::cout << p.first << " -> " << p.second << std::endl;
        }
    };

    void printIndex(void)
    {
        for (size_t i = 0; i < index2item.size(); ++i)
        {
            std::cout << "[" << i << "] = " << *(index2item[i]) << std::endl;
        }
    };
};

struct CoNLLUDatabase
{
    std::vector<CoNLLUSentence> sentences;

    BidirectionalMap<std::string, WordId> words;
    BidirectionalMap<std::string, ShortWordId> tags;

    WordId beginTag;
    WordId endTag;
    WordId unkTag;

    void reset(void);

    bool load(const std::string& fileName);
    bool loadDirectory(const std::string& directoryName);

    const std::string& index2word(const WordId ix);
    WordId word2index(const std::string& word);

    const std::string& index2tag(const ShortWordId ix);
    ShortWordId tag2index(const std::string& word);
};
