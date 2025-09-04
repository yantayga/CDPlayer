#include "CoNLLU.h"

#include <sstream>
#include <fstream>
#include <filesystem>
#include <cctype>
#include <locale>
#include <cassert>

#include <unicode/unistr.h>
#include <unicode/ustream.h>
#include <unicode/locid.h>

const char defStartTag[] = "<START>";
const char defEndTag[] = "<END>";
const char defUnkTag[] = "<UNK>";


const std::vector<std::string> POS_TAGS = {
// service tags
    "<start>", "<end>",
// https://universaldependencies.org/u/pos/all.html
    "x",      // other/url/foreign/unknown
    "adj",    // adjective
    "adp",    // adposition (prepositions and postpositions)
    "adv",    // adverb
    "aux",    // auxiliary
    "cconj",  // coordinating conjunction
    "det",    // determiner
    "intj",   // interjection
    "noun",   // noun
    "num",    // numeral
    "part",   // particle
    "pron",   // pronoun
    "propn",  // proper noun
    "punct",  // punctuation
    "sconj",  // subordinating conjunction
    "sym",    // symbol
    "verb",   // verb
};

const std::vector<std::string> FEATURE_NAMES = {
    "prontype", "gender", "verbform",
    "numtype", "animacy", "mood",
    "poss", "nounclass", "tense",
    "reflex", "number", "aspect",
    "other", "case", "voice",
    "abbr", "definite", "evident",
    "typo", "deixis", "polarity",
    "foreign", "deixisref", "person",
    "extpos", "degree", "polite",
    "clusivity",
};

const std::vector<std::string> FEATURE_VALUES = {
    /* prontype  */ "art", "dem", "emp", "exc", "ind", "int", "neg", "prs", "rcp", "rel", "tot",
    /* numtype   */ "card ", "dist", "frac", "mult", "ord", "range", "sets",
    /* poss      */ "yes",
    /* reflex    */ "yes",
    /* abbr      */ "yes",
    /* typo      */ "yes",
    /* foreign   */ "yes",
    /* extpos    */ "adj", "adp", "adv", "aux", "cconj", "det", "intj", "pron", "propn", "sconj",
    /* gender    */ "com", "fem", "masc", "neut",
    /* animacy   */ "anim", "hum", "inan", "nhum",
    /* nounclass */ // skipped intentionally
    /* number    */ "coll", "count", "dual", "grpa", "grpl", "inv", "pauc", "plur", "ptan", "sing", "tri",
    /* case      */ "abs", "acc", "erg", "nom",
                    "abe", "ben", "cau", "cmp", "cns", "com", "dat", "dis", "equ", "gen", "ins", "par", "tem", "tra", "voc",
                    "abl", "add", "ade", "all", "del", "ela", "ess", "ill", "ine", "lat", "loc", "per", "sbe", "sbl", "spl", "sub", "sup", "ter",
    /* definite  */ "com", "cons", "def", "ind", "spec",
    /* deixis    */ "abv", "bel", "even", "med", "nvis", "prox", "remt",
    /* deixisref */ "1", "2",
    /* degree    */ "abs", "aug", "cmp", "dim", "equ", "pos", "sup",
    /* verbform  */ "conv", "fin", "gdv", "ger", "inf", "part", "sup", "vnoun",
    /* mood      */ "adm", "cnd", "des", "imp", "ind", "int", "irr", "jus", "nec", "opt", "pot", "prp", "qot", "sub",
    /* tense     */ "fut", "imp", "past", "pqp", "pres",
    /* aspect    */ "hab", "imp", "iter", "perf", "prog", "prosp",
    /* voice     */ "act", "antip", "bfoc", "cau", "dir", "inv", "lfoc", "mid", "pass", "rcp",
    /* evident   */ "fh", "nfh",
    /* polarity  */ "neg", "pos",
    /* person    */ "0", "1", "2", "3", "4",
    /* polite    */ "elev", "form", "humb", "infm",
    /* clusivity */ "ex", "in",
};

const std::vector<std::string> DEP_RELS = {
    /* Core arguments      */ "nsubj", "obj", "iobj", "csubj", "ccomp", "xcomp",
    /* Non-core dependents */ "obl", "vocative", "expl", "dislocated", "advcl",
                              "advmod", "discourse", "aux", "cop", "mark",
    /* Nominal dependents  */ "nmod", "appos", "nummod", "acl", "amod", "det", "clf", "case",
    /* Coordination        */ "conj", "cc",
    /* Headless            */ "fixed", "flat",
    /* Loose               */ "list", "parataxis",
    /* Special             */ "compound", "orphan", "goeswith", "reparandum",
    /* Other               */ "punct", "root", "dep",
};

const std::vector<std::string> DEP_RELS_MODIFIERS = {
    "", "outer", "pass", "agent", "arg", "lmod", "tmod", "outer", "pass", "emph", "lmod", "impers", "pass", "relcl", "poss",
    "pass", "tmod", "numgov", "nummod", "gov", "foreign", "name", "lvc", "prt", "redup", "svc", "pv", "relcl", "poss", "preconj",
};

template <class Item, class Index>
BidirectionalMap<Item, Index>::BidirectionalMap(const std::vector<Item> items)
{
    std::for_each(items.begin(), items.end(), [this](auto item) { lookupOrInsert(item); });
}

template <class Item, class Index>
void BidirectionalMap<Item, Index>::clear(void)
{
    item2index.clear();
    index2item.clear();
}

template <class Item, class Index>
const Index BidirectionalMap<Item, Index>::lookupOrInsert(const Item& item)
{
    const auto res = item2index.try_emplace(item, index2item.size());

    if (res.second)
    {
        index2item.push_back(&res.first->first);
    }

    return res.first->second;
}

template <class Item, class Index>
const Index BidirectionalMap<Item, Index>::lookup(const Item& item) const
{
    auto res = item2index.find(item);
    if (res == item2index.end())
    {
        return -1;
    }

    return res->second;
}

template <class Item, class Index>
const Item& BidirectionalMap<Item, Index>::lookupIndex(const Index index) const
{
    return *index2item[index];
}

CoNLLUDatabase::CoNLLUDatabase()
        : posTags(POS_TAGS)
        , featureNames(FEATURE_NAMES)
        , featureValues(FEATURE_VALUES)
        , depRels(DEP_RELS)
        , depRelModifiers(DEP_RELS_MODIFIERS)
        , maxFeaturesNum(0)
{
    std::cout << "Bits for POS tag: " << posTags.bits() << std::endl;
    posTags.printIndex();
    std::cout << std::endl << "Bits for feature name: " << featureNames.bits() << std::endl;
    featureNames.printIndex();
    std::cout << std::endl << "Bits for feature value: " << featureValues.bits() << std::endl;
    featureValues.printIndex();
    std::cout << std::endl << "Bits for dependency relation: " << depRels.bits() << std::endl;
    depRels.printIndex();
    std::cout << std::endl << "Bits for dependency relation modifiers: " << depRelModifiers.bits() << std::endl;
    depRelModifiers.printIndex();\
    std::cout << std::endl;
}

void CoNLLUDatabase::reset(void)
{
    sentences.clear();
    words.clear();

    beginTag = words.lookupOrInsert(defStartTag);
    endTag = words.lookupOrInsert(defEndTag);
    unkTag = words.lookupOrInsert(defUnkTag);
}

inline void ltrim(std::string &s)
{
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
        return !std::isspace(ch);
    }));
}

inline void rtrim(std::string &s)
{
    s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

inline void trim(std::string &s)
{
    rtrim(s);
    ltrim(s);
}

bool parsePair(const std::string s, const std::string& delim, std::string& name, std::string& value)
{
    const std::string::size_type pos = s.find(delim);
    if (pos == std::string::npos)
    {
        return false;
    }

    name = s.substr(0, pos);
    trim(name);
    value = s.substr(pos + 1);
    trim(value);
    return true;
}

std::vector<std::string> split(const std::string s, char delim)
{
    std::vector<std::string> result;
    std::stringstream ss(s);
    std::string item;

    while(std::getline(ss, item, delim))
    {
        trim(item);
        result.push_back(item);
    }

    return result;
}

void filterNumbers(std::string& s)
{
    bool replaced = false;
    std::replace_if(s.begin(), s.end(), [&replaced](unsigned char c){ bool r = std::isdigit(c); replaced = r || replaced; return r; }, 'N');
    if (replaced)
    {
        auto last = std::unique(s.begin(), s.end());
        s.erase(last, s.end());
    }
}

void toLower(std::string& s)
{
    icu::UnicodeString us(s.c_str(), "UTF-8");
    us.toLower();
    s.clear();
    us.toUTF8String(s);
}

std::string fixTag(const std::string& s)
{
    if (s == "h") { return "adv"; }
    else if (s == "conj") { return "cconj"; }
    else return s;
}

bool CoNLLUDatabase::load(const std::string& fileName)
{
    std::ifstream stream(fileName, std::fstream::in);

    if (stream.is_open())
    {
        CoNLLUSentence sentence;
        for (std::string line; std::getline(stream, line, '\n');)
        {
            if (line.empty() || line.starts_with('#') || line.starts_with('='))
            {
                if (!sentence.words.empty())
                {
                    sentences.push_back(sentence);
                    sentence.words.clear();
                }
                continue;
            }

            toLower(line);

            std::vector<std::string> wordData = split(line, '\t');
            if (wordData.size() > 3)
            {
                // TODO: Filter foreign words
                // TODO: Filter URLs
                CoNLLUWord word;

                CompoundTag tag;
                tag.tag128 = 0;

                // skip words counter wordData[0]

                filterNumbers(wordData[1]);
                word.word = words.lookupOrInsert(wordData[1]);
                filterNumbers(wordData[2]);
                word.initialWord = words.lookupOrInsert(wordData[2]);
                tag.coumpoundTag.POS = posTags.lookup(fixTag(wordData[3]));
                if (tag.coumpoundTag.POS > posTags.size())
                {
                    std::cout << "Unknown POS tag: '" << wordData[3] << "'" << std::endl;
                }

                // optional
                std::string featuresLine;
                if (wordData.size() > 4) featuresLine += wordData[4];
                if (wordData.size() > 5) featuresLine += '|' + wordData[5];

                std::vector<std::string> features = split(featuresLine, '|');
                std::sort(features.begin(), features.end());
                size_t addedFeatures = 0;
                for (auto featurePair: features)
                {
                    std::string name, value;
                    if (parsePair(featurePair, "=", name, value))
                    {
                        if (name.empty() || value.empty())
                        {
                            continue;
                        }

                        ShortWordId fname = featureNames.lookup(name);
                        ShortWordId fvalue = featureValues.lookup(value);
                        if (fname > featureNames.size() || fvalue > featureValues.size())
                        {
                            std::cout << "Unknown feature pair '" << featurePair << "' for POS tag '" << wordData[3] << "'" << std::endl;
                        }
                        else
                        {
                            ++addedFeatures;
                            tag.coumpoundTag.features =
                                tag.coumpoundTag.features << (featureNames.bits() + featureValues.bits()) |
                                fname << featureNames.bits() | fvalue;
                        }
                    }
                }
                if (addedFeatures > maxFeaturesNum) maxFeaturesNum = addedFeatures;


                word.tags = tags.lookupOrInsert(tag);

                try
                {
                    if (wordData.size() > 6) word.depHead = std::stoul(wordData[6]);
                }
                catch(std::invalid_argument&)
                {
                    word.depHead = 0;
                }

                if (wordData.size() > 7 && wordData[7] != "_")
                {
                    std::string depRelMain, depRelMod;
                    if (!parsePair(wordData[7], ":", depRelMain, depRelMod))
                    {
                        depRelMain = wordData[7];
                        depRelMod = "";
                    }

                    ShortWordId depRel = depRels.lookup(depRelMain);
                    if (depRel > depRels.size())
                    {
                        std::cout << "Unknown dependency relation '" << depRelMain << "' for POS tag '" << wordData[3] << "'" << std::endl;
                    }
                    else
                    {
                        word.depRel = depRel;
                    }

                    ShortWordId depRelModifier = depRelModifiers.lookup(depRelMod);
                    if (depRelModifier > depRelModifiers.size())
                    {
                        std::cout << "Unknown dependency relation modifier '" << depRelMain << ": " << depRelMod << "' for POS tag '" << wordData[3] << "'" << std::endl;
                    }
                    else
                    {
                        word.depRelModifier = depRelModifier;
                    }
                }

                sentence.words.push_back(word);
            }
        }
    }

    std::cout << "Max features num = " << maxFeaturesNum
              << ", bits per features = " << maxFeaturesNum * (featureNames.bits() + featureValues.bits())
              << ", overall bits per word tags = " << posTags.bits() + maxFeaturesNum * (featureNames.bits() + featureValues.bits()) << std::endl;


    return false;
}

bool CoNLLUDatabase::loadDirectory(const std::string& directoryName)
{
    if (!std::filesystem::exists(directoryName))
    {
        return false;
    }

    for (auto const& dir_entry : std::filesystem::recursive_directory_iterator{directoryName})
    {
        if (std::filesystem::is_regular_file(dir_entry))
        {
            load(dir_entry.path());
            continue;
        }
    }
    return true;
}

const std::string& CoNLLUDatabase::index2word(const WordId ix) const
{
    return words.lookupIndex(ix);
}

WordId CoNLLUDatabase::word2index(const std::string& word)
{
    return words.lookupOrInsert(word);
}
