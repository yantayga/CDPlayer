#include "CoNLLU.h"
#include "Serialize.h"

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
    // Service
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
    "poss", /*"nounclass",*/ "tense",
    "reflex", "number", "aspect",
    "other", "case", "voice",
    "abbr", "definite",/* "evident",*/
    "typo", "deixis", "polarity",
    "foreign", "deixisref", "person",
    "extpos", "degree", "polite",
    /*"clusivity", */"numform", "hyph",
    "subcat", "nametype", "style",
    // added for short verbs/adjs
    "short",
};

const std::vector<std::string> FEATURE_VALUES = {
    /* prontype  */ "art", "dem", "emp", "exc", "ind", "int", "neg", "prs", "rcp", "rel", "tot",
    /* numtype   */ "card", "dist", "frac", "mult", "ord", "range", "sets",
    /* numform   */ "combi", "digit", "roman", "word",
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
    /* case      */ "nom", "gen", "dat", "acc", "abl", "ins", "voc", "par", "loc",
//                  "abs", "ben", "cmp", "cns", "equ", "erg", "ess", "com", "lat", "ter", "tra", "cau", // skipped intentionally
//                  "ine", "ill", "ela", "add", "ade", "all", "sup", "spl", "del", "sub", "sbe", "per", // skipped intentionally
//                  "tem", "abe", "dis", "ine", "sbl", // skipped intentionally
    /* definite  */ "com", "cons", "def", "ind", "spec",
    /* deixis    */ "abv", "bel", "even", "med", /* "nvis", */ "prox", "remt",
    /* deixisref */ "1", "2",
    /* degree    */ "abs", "aug", "cmp", "dim", "equ", "pos", "sup",
    /* verbform  */ "conv", "fin", "gdv", "ger", "inf", "part", "sup", "vnoun",
    /* mood      */ "cnd", "imp", "ind", "int", "pot", "sub",
//                  "adm", "irr", "jus", "nec", "prp", "qot", "des", "opt", // skipped intentionally
    /* tense     */ "fut", "imp", "past", "pqp", "pres",
    /* aspect    */ "hab", "imp", "iter", "perf", "prog", "prosp",
    /* voice     */ "act", "mid", "pass", "rcp",
//                  "antip", "bfoc", "cau", "dir", "inv", "lfoc", // skipped intentionally
//    /* evident   */ "fh", "nfh", // skipped intentionally
    /* polarity  */ "neg", "pos",
    /* person    */ "0", "1", "2", "3", "4",
    /* polite    */ "elev", "form", "humb", "infm",
//    /* clusivity */ "ex", "in", // skipped intentionally
    /* subcat    */ "ditr", "indir", "intr", "tran",
    /* nametype  */ "com", "geo", "giv", "nat", "oth", "pat", "pro", "prs", "sur", "zoon",
    /* style     */ "arch", "coll", "expr", "form", "rare", "slng", "vrnc", "vulg",
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

void CoNLLUWord::saveBinary(std::ostream& stream) const
{
    write(stream, word);
    write(stream, initialWord);
    write(stream, tags);
    write(stream, depHead);
    write(stream, depRel);
    write(stream, depRelModifier);
}

bool CoNLLUWord::loadBinary(std::istream& stream)
{
    read(stream, word);
    read(stream, initialWord);
    read(stream, tags);
    read(stream, depHead);
    read(stream, depRel);
    read(stream, depRelModifier);

    return true;
}

void CoNLLUSentence::saveBinary(std::ostream& stream) const
{
    write(stream, words.size());
    for (size_t i = 0; i < words.size(); ++i)
    {
        words[i].saveBinary(stream);
    }
}

bool CoNLLUSentence::loadBinary(std::istream& stream)
{
    size_t size = 0;
    read(stream, size);

    words.resize(size);

    for (size_t i = 0; i < size; ++i)
    {
        words[i].loadBinary(stream);
    }

    return true;
}

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

template <class Item, class Index>
void BidirectionalMap<Item, Index>::saveBinary(std::ostream& stream) const
{
    write(stream, size());
    for (size_t i = 0; i < index2item.size(); ++i)
    {
        write(stream, lookupIndex(i));
    }
}

template <class Item, class Index>
bool BidirectionalMap<Item, Index>::loadBinary(std::istream& stream)
{
    size_t size = 0;
    read(stream, size);

    index2item.resize(size);

    for (size_t i = 0; i < size; ++i)
    {
        Item item;
        if (read(stream, item))
        {
            const auto res = item2index.try_emplace(item, i);
            index2item[i] = &res.first->first;
        }
        else
        {
            return false;
        }
    }

    return true;
}

CoNLLUDatabase::CoNLLUDatabase()
        : posTags(POS_TAGS)
        , featureNames(FEATURE_NAMES)
        , featureValues(FEATURE_VALUES)
        , depRels(DEP_RELS)
        , depRelModifiers(DEP_RELS_MODIFIERS)
{
    reset();
}

void CoNLLUDatabase::reset(void)
{
    sentences.clear();
    words.clear();
    tags.clear();

    statistics.files.clear();
    statistics.errors.clear();
    statistics.maxFeaturesNum = 0;

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

bool fixFeatureName(std::string& s, std::string& pos)
{
    if (s.starts_with("form"))
    {
        s = "numform";
        return true;
    }
    if (s.starts_with("anom"))
    {
        s = "typo";
        return true;
    }
    if (s.starts_with("tran"))
    {
        s = "subcat";
        return true;
    }
    if (s.starts_with("anim"))
    {
        s = "animacy";
        return true;
    }
    if (s.starts_with("predic") || s.starts_with("decl"))
    {
        return false;
    }
    if (s.starts_with("nametype"))
    {
        pos = "propn";
        return true;
    }
    if (s.starts_with("variant"))
    {
        s = "short";
        return true;
    }
    return true;
}

bool fixFeatureValue(std::string& s)
{
    if (s.starts_with("aor") || s.starts_with("notpast"))
    {
        s = "pres";
        return true;
    }
    if (s.starts_with("fut"))
    {
        s = "fut";
        return true;
    }
    if (s.starts_with("tran"))
    {
        s = "tran";
        return true;
    }
    if (s.starts_with("intr"))
    {
        s = "intr";
        return true;
    }
    if (s.starts_with("long"))
    {
        s = "full";
        return true;
    }
    if (s.starts_with("split") || s.starts_with("init") || s.starts_with("short"))
    {
        s = "yes";
        return true;
    }
    if (s.starts_with("ptr"))
    {
        s = "pat";
        return true;
    }
    if (s.starts_with("obsc"))
    {
        s = "vulg";
        return true;
    }
    if (s == "no" || s == "full")
    {
        return false;
    }
    return true;
}

bool CoNLLUDatabase::load(const std::string& fileName)
{
    std::ifstream stream(fileName, std::fstream::in);

    if (stream.is_open())
    {
        FileStatistics st;

        st.fileName = fileName;
        st.sentencesNum = 0;
        st.wordsNum = 0;

        CoNLLUSentence sentence;
        for (std::string line; std::getline(stream, line, '\n');)
        {
            if (line.empty() || line.starts_with('#') || line.starts_with('='))
            {
                if (!sentence.words.empty())
                {
                    ++st.sentencesNum;
                    st.wordsNum += sentence.words.size();
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
                    statistics.errors.insert("Unknown POS tag: '" + wordData[3] + "'.");
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
                        std::string newPOS;
                        if (name.empty() || value.empty() || !fixFeatureName(name, newPOS)|| !fixFeatureValue(value))
                        {
                            statistics.errors.insert("Ignored feature pair '" + featurePair + "' for POS tag '" + wordData[3] + "'.");
                            if (!newPOS.empty()) tag.coumpoundTag.POS = posTags.lookup(newPOS);
                            continue;
                        }

                        ShortWordId fname = featureNames.lookup(name);
                        ShortWordId fvalue = featureValues.lookup(value);
                        if (fname > featureNames.size() || fvalue > featureValues.size())
                        {
                            statistics.errors.insert("Unknown feature pair '" + name + "=" + value + +"/" + featurePair + "' for POS tag '" + wordData[3] + "'.");
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
                if (addedFeatures > statistics.maxFeaturesNum)
                {
                    statistics.maxFeaturesNum = addedFeatures;
                    statistics.errors.insert("File: " + fileName + ": maximum size '" +
                                             std::to_string(statistics.maxFeaturesNum) +
                                             "' found for line '" + featuresLine + "'.");
                }


                word.tags = tags.lookupOrInsert(tag);

                try
                {
                    if (wordData.size() > 6) word.depHead = std::stoul(wordData[6]);
                }
                catch(std::invalid_argument&)
                {
                    word.depHead = 0;
                }

                // TODO: extract transitivity from deprel
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
                        //statistics.errors.insert("File: " + fileName + ": unknown dependency relation '" + depRelMain + "' for POS tag '" + wordData[3] + "'." );
                    }
                    else
                    {
                        word.depRel = depRel;
                    }

                    ShortWordId depRelModifier = depRelModifiers.lookup(depRelMod);
                    if (depRelModifier > depRelModifiers.size())
                    {
                        //statistics.errors.insert("File: " + fileName + ": unknown dependency relation modifier '" + depRelMain + ": " + depRelMod + "' for POS tag '" + wordData[3] + "'.");
                    }
                    else
                    {
                        word.depRelModifier = depRelModifier;
                    }
                }

                sentence.words.push_back(word);
            }
        }

        statistics.files.push_back(st);
    }
    else
    {
        statistics.errors.insert("Failed to open " + fileName + ".");
    }

    return false;
}

bool CoNLLUDatabase::loadDirectory(const std::string& directoryName)
{
    if (!std::filesystem::exists(directoryName))
    {
        statistics.errors.insert("Failed to open " + directoryName + ".");
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

void CoNLLUDatabase::printStatistics(void)
{
    std::cout << "Database statistics:" << std::endl;

    std::cout << "Bits for POS tag: " << posTags.bits() << std::endl;
    posTags.printIndex();
    std::cout << std::endl << "Bits for feature name: " << featureNames.bits() << std::endl;
    featureNames.printIndex();
    std::cout << std::endl << "Bits for feature value: " << featureValues.bits() << std::endl;
    featureValues.printIndex();
    std::cout << std::endl << "Bits for dependency relation: " << depRels.bits() << std::endl;
    depRels.printIndex();
    std::cout << std::endl << "Bits for dependency relation modifiers: " << depRelModifiers.bits() << std::endl;
    depRelModifiers.printIndex();
    std::cout << std::endl;

    std::cout << "Max features num = " << statistics.maxFeaturesNum
              << ", bits per features = " << statistics.maxFeaturesNum * (featureNames.bits() + featureValues.bits())
              << ", overall bits per word tags = " << posTags.bits() + statistics.maxFeaturesNum * (featureNames.bits() + featureValues.bits()) << std::endl;

    std::cout << "Overall sentences: " << sentences.size() << "." << std::endl;
    std::cout << "Overall words: " << words.size() << "." << std::endl;
    std::cout << "Overall tags: " << tags.size() << "." << std::endl;

    std::cout << "Files loaded:" << std::endl;
    for (auto& file: statistics.files)
    {
        std::cout << "File: " << file.fileName << ": " << file.sentencesNum << " sentences, " << file.wordsNum << " words." << std::endl;
    }

    std::cout << "Errors:" << std::endl;
    for (auto& error: statistics.errors)
    {
        std::cout << error << std::endl;
    }
}

bool CoNLLUDatabase::loadBinary(const std::string& fileName)
{
    std::ifstream stream(fileName, std::fstream::in);

    if (stream.is_open())
    {
        reset();

        if (!readMagic(stream))
        {
            std::cout << "Wrong file type: " << fileName << std::endl;
            return false;
        }

        if (!words.loadBinary(stream))
        {
            std::cout << "Cannot read words: " << fileName << std::endl;
            return false;
        }

        if (!tags.loadBinary(stream))
        {
            std::cout << "Cannot read words: " << fileName << std::endl;
            return false;
        }

        size_t size = 0;
        read(stream, size);

        sentences.resize(size);

        for (size_t i = 0; i < size; ++i)
        {
            sentences[i].loadBinary(stream);
        }

        return true;
    }

    std::cout << "Could not open: " << fileName << std::endl;
    return false;
}

bool CoNLLUDatabase::saveBinary(const std::string& fileName) const
{
    std::ofstream stream(fileName, std::fstream::out | std::ios::binary | std::ios::trunc);

    if (stream.is_open())
    {
        writeMagic(stream);

        words.saveBinary(stream);
        tags.saveBinary(stream);

        write(stream, sentences.size());

        for (const auto& sentence: sentences)
        {
            sentence.saveBinary(stream);
        }

        return true;
    }

    std::cout << "Could not open: " << fileName << std::endl;
    return false;
}

