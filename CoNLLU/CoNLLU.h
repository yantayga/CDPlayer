#include <string>
#include <vector>
#include <utility>
#include <functional>
#include <memory>
#include <unordered_map>

#include <iostream>

typedef size_t WordId;
typedef unsigned short ShortWordId;
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
    void clear(void);
    size_t size(void) { return index2item.size(); };
    const Index lookupOrInsert(const Item& item);
    const Item& lookupIndex(const Index index);

    void print(void)
    {
        for (const auto& p: item2index)
        {
            std::cout << p.first << " -> " << p.second << std::endl;
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

    void reset(void);

    bool load(const std::string& fileName);
    bool loadDirectory(const std::string& directoryName);
};
