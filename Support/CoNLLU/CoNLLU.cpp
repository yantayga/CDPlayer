#include "CoNLLU.h"
#include "../Serialize/Serialize.h"

#include <sstream>
#include <fstream>
#include <filesystem>
#include <cctype>
#include <locale>
#include <cassert>

#include <unicode/unistr.h>
#include <unicode/ustream.h>
#include <unicode/locid.h>

#include <gzstream.h>

#include "CoNLLUStd.h"

template <class Item, class Index>
BidirectionalMap<Item, Index>::BidirectionalMap(const std::vector<Item> items)
{
    std::for_each(items.begin(), items.end(), [this](auto item) { lookupOrInsert(item); });
}

template<class Item, class Index>
template<typename Initializer>
BidirectionalMap<Item, Index>::BidirectionalMap(const std::vector<Initializer> initializers)
{
    for (const auto& item: initializers)
    {
        const auto res = item2index.try_emplace(item.name, TagFeatures(index2item.size(), item.items));
        index2item.push_back(&res.first->first);
    }
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
        return Index(-1);
    }

    return res->second;
}

template <class Item, class Index>
const Item& BidirectionalMap<Item, Index>::lookupIndex(const Index index) const
{
    return *index2item[index];
}

template <class Item, class Index>
bool BidirectionalMap<Item, Index>::isValidIndex(const Index index) const
{
    return index != Index(-1);
}

template <class Item, class Index>
void BidirectionalMap<Item, Index>::saveBinary(std::ostream& stream) const
{
    uint64_t l = size();
    serialize(stream, l);
    for (size_t i = 0; i < l; ++i)
    {
        serialize(stream, lookupIndex(i));
    }
}

template <class Item, class Index>
void BidirectionalMap<Item, Index>::loadBinary(std::istream& stream)
{
    uint64_t size = 0;
    deserialize(stream, size);

    index2item.resize(size);

    for (size_t i = 0; i < size; ++i)
    {
        Item item;

        deserialize(stream, item);

        const auto res = item2index.try_emplace(item, i);

        index2item[i] = &res.first->first;
    }
}

CoNLLUDatabase::CoNLLUDatabase()
        : posTags(TAG_DESCRIPTIONS)
        , featureValues(FEATURE_VALUES)
        , depRels(DEP_RELS)
        , depRelModifiers(DEP_RELS_MODIFIERS)
{
    reset();

    CompoundTag t;
    t.POS = posTags.lookup("x").index;
    unknownWord.tags = tags.lookupOrInsert(t);

    unknownWord.word = words.lookupOrInsert("");
    unkWordOnly.words.push_back(unknownWord);
}

void CoNLLUDatabase::reset(void)
{
    sentences.clear();
    words.clear();
    tags.clear();

    statistics.clear();

    serviceTag = words.lookupOrInsert(defServiceTag);
    CompoundTag t;
    tags.lookupOrInsert(t);
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

std::vector<std::string> split(std::string s, std::string delims)
{
    std::for_each(delims.begin(), delims.end(), [&](char &c) { std::replace(s.begin(), s.end(), c, '\n'); });
    
    std::stringstream ss(s);
    std::string item;
    std::vector<std::string> result;
    while(std::getline(ss, item))
    {
        trim(item);
        if (!item.empty())
        {
            result.push_back(item);
        }
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

bool fixFeatureName(std::string& s, const std::string& oldpos, std::string& pos)
{
    if (s.starts_with("form"))
    {
        s = "numform";
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
    if ((oldpos == "adj" || oldpos == "noun") && (s == "aspect" || s == "verbform" || s == "subcat" || s == "voice"))
    {
        pos = "verb";
        return false;
    }
    if (oldpos == "adj" && s == "nametype") 
    {
        pos = "propn";
        return false;
    }
    if (s.starts_with("predic"))
    {
        // skip
        return false;
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
    if (s.starts_with("split") || s.starts_with("init"))
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
    if (s == "no" || s == "full" || s == "long")
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
        size_t sentencesNum = 0;
        size_t wordsNum = 0;

        CoNLLUSentence sentence;
        size_t lines = 0;
        for (std::string line; std::getline(stream, line, '\n');)
        {
            ++lines;
            if (line.empty() || line.starts_with('#') || line.starts_with('='))
            {
                if (!sentence.words.empty())
                {
                    ++sentencesNum;
                    wordsNum += sentence.words.size();
                    sentences.push_back(sentence);
                    sentence.words.clear();
                }
                continue;
            }

            toLower(line);
            
            if (line.starts_with('\t'))
            {
                line = '0' + line; 
            }
            
            std::vector<std::string> wordData = split(line, "\t");
            
            if (wordData.size() > 3)
            {
                // TODO: Filter foreign words
                // TODO: Filter URLs
                CoNLLUWord word = {0};

                // skip words counter wordData[0]

                filterNumbers(wordData[1]);
                word.word = words.lookupOrInsert(wordData[1]);

                filterNumbers(wordData[2]);
                word.initialWord = words.lookupOrInsert(wordData[2]);

                bool needToFillFeatures = true;
                size_t reassignCounter = 0;
                while (needToFillFeatures)
                {
                    needToFillFeatures = false;
                    CompoundTag tag = {0};

                    const TagFeatures& posTag = posTags.lookup(fixTag(wordData[3]));

                    tag.POS = posTag.index;
                    if (!posTags.isValidIndex(tag.POS))
                    {
                        statistics.addMessage(fileName, "Unknown POS tag: '" + wordData[3] + "' -> 'x'.");
                        wordData[3] = "x";
                        continue;
                    }
                    
                    std::string featuresLine;
                    if (wordData.size() > 4 && wordData[4] != "_") featuresLine += wordData[4];
                    if (wordData.size() > 5 && wordData[5] != "_") featuresLine += '|' + wordData[5];

                    std::vector<std::string> features = split(featuresLine, "/|");
                    std::sort(features.begin(), features.end());
                    size_t addedFeatures = 0;
                    for (auto featurePair: features)
                    {
                        std::string name, value;
                        if (!parsePair(featurePair, "=", name, value))
                        {
                            statistics.addMessage(fileName, "Wrong feature pair without '=': '" + 
                                                  featurePair + "' for POS tag '" + wordData[3] + "' in '" + featuresLine + "' .");
                            continue;
                        }

                        std::string newPOS;
                        if (name.empty() || value.empty() || !fixFeatureName(name, wordData[3], newPOS)|| !fixFeatureValue(value))
                        {
                            statistics.addMessage(fileName, "Ignored feature pair '" + featurePair + "' for POS tag '" + wordData[3] + "'.");
                            if (!newPOS.empty())
                            {
                                statistics.addMessage(fileName, "New POS tag assigned: " +  wordData[3] + " -> " + newPOS + " for features '" + featuresLine + "'.");
                                wordData[3] = newPOS;
                                ++reassignCounter;
                                needToFillFeatures = true;
                                break;
                            }
                            continue;
                        }

                        ShortWordId fname = posTag.items.lookup(name);
                        ShortWordId fvalue = featureValues.lookup(value);
                        if (!posTag.items.isValidIndex(fname) || !featureValues.isValidIndex(fvalue))
                        {
                            statistics.addMessage(fileName, "Unknown feature pair '" + name + "=" + value + +"/" + featurePair + "' for POS tag '" + wordData[3] + "'.");
                        }
                        else
                        {
                            tag.features[addedFeatures].featureNameId = fname;
                            tag.features[addedFeatures].featureValueId = fvalue;
                            ++addedFeatures;
                        }

                        if (addedFeatures >= sizeof(tag.features) / sizeof(tag.features[0]))
                        {
                            statistics.addMessage(fileName, "Maximum features number reached for POS tag '" + wordData[3] + "'.");
                            break;
                        }
                    }

                    if (needToFillFeatures)
                    {
                        continue;
                    }

                    statistics.updateMaxFeaturesNum(addedFeatures);
                    
                    word.tags = tags.lookupOrInsert(tag);
                }
                
                try
                {
                    if (wordData.size() > 6) word.depHead = std::stoul(wordData[6]);
                }
                catch(std::invalid_argument&)
                {
                    word.depHead = 0;
                }

                // TODO: extract verb transitivity from deprel
                if (wordData.size() > 7 && wordData[7] != "_")
                {
                    std::string depRelMain, depRelMod;
                    if (!parsePair(wordData[7], ":", depRelMain, depRelMod))
                    {
                        depRelMain = wordData[7];
                        depRelMod = "";
                    }

                    ShortWordId depRel = depRels.lookup(depRelMain);
                    if (!depRels.isValidIndex(depRel))
                    {
//                        statistics.addMessage(fileName, 
//                            "Unknown dependency relation '" + depRelMain + "' for POS tag '" + wordData[3] + "'." );
                    }
                    else
                    {
                        word.depRel = depRel;
                    }

                    ShortWordId depRelModifier = depRelModifiers.lookup(depRelMod);
                    if (!depRelModifiers.isValidIndex(depRelModifier))
                    {
//                        statistics.addMessage(fileName, 
//                            "Unknown dependency relation modifier '" + depRelMain + ": " + depRelMod + "' for POS tag '" + wordData[3] + "'.");
                    }
                    else
                    {
                        word.depRelModifier = depRelModifier;
                    }
                }

                sentence.words.push_back(word);
            }
        }

        if (!sentence.words.empty())
        {
            ++sentencesNum;
            wordsNum += sentence.words.size();
            sentences.push_back(sentence);
            sentence.words.clear();
        }

        statistics.addFile(fileName, sentencesNum, wordsNum);
    }
    else
    {
        statistics.addMessage(fileName, "Failed to open.");
        return false;
    }

    return true;
}

bool CoNLLUDatabase::loadDirectory(const std::string& path)
{
    if (!std::filesystem::exists(path))
    {
        statistics.addMessage(path, "Failed to open.");
        return false;
    }

    if (!std::filesystem::is_directory(path))
    {
        return load(path);
    }

    for (auto const& dir_entry : std::filesystem::recursive_directory_iterator{path})
    {
        if (std::filesystem::is_regular_file(dir_entry))
        {
            // TODO: make it multithreaded
            load(dir_entry.path());
            continue;
        }
    }
    return true;
}

std::vector<WordId> CoNLLUDatabase::encodeWords(const std::vector<std::string>& words) const
{
    std::vector<WordId> res(words.size());

    for (size_t i = 0; i < words.size(); ++i)
    {
        res[i] = word2index(words[i]);
    }

    return res;
}

const std::string& CoNLLUDatabase::index2word(const WordId ix) const
{
    return words.lookupIndex(ix);
}

WordId CoNLLUDatabase::word2index(const std::string& word) const
{
    WordId res = words.lookup(word);
    if (words.isValidIndex(res))
    {
        return res;
    }

    return unknownWord.word;
}

void CoNLLUDatabase::printStatistics(void)
{
    std::cout << "Database statistics:" << std::endl;

    std::cout << "Sentences: " << sentences.size() << "." << std::endl;
    std::cout << "Words: " << words.size() << "." << std::endl;
    std::cout << "Tags: " << tags.size() << "." << std::endl;

    statistics.print();
}

bool CoNLLUDatabase::loadBinary(const std::string& fileName, bool useSentences)
{
    igzstream stream;

    stream.open(fileName.c_str());

    if (stream.good())
    {
        reset();

        if (!readMagic(stream))
        {
            std::cout << "Wrong file type: " << fileName << std::endl;
            return false;
        }
        words.loadBinary(stream);

        tags.loadBinary(stream);

        hmm.loadBinary(*this, stream);

        if (useSentences)
        {
            size_t size = 0;
            deserialize(stream, size);

            sentences.resize(size);

            std::cout << "CSentences to load: " << size << std::endl;
            for (size_t i = 0; i < size; ++i)
            {
                sentences[i].loadBinary(stream);
            }

            unkWordOnly.loadBinary(stream);
        }

        return true;
    }

    std::cout << "Could not open: " << fileName << std::endl;
    return false;
}

bool CoNLLUDatabase::saveBinary(const std::string& fileName, bool useSentences) const
{
    ogzstream stream;

    stream.open(fileName.c_str());

    if (stream.good())
    {
        writeMagic(stream);

        words.saveBinary(stream);

        tags.saveBinary(stream);

        hmm.saveBinary(stream);

        if (useSentences)
        {
            serialize(stream, sentences.size());

            for (const auto& sentence: sentences)
            {
                sentence.saveBinary(stream);
            }

            unkWordOnly.saveBinary(stream);
        }
        else
        {
            serialize(stream, size_t(0));
        }

        return true;
    }

    std::cout << "Could not open: " << fileName << std::endl;
    return false;
}

void CoNLLUDatabase::train(double smoothingFactor)
{
    hmm.train(*this, smoothingFactor);
}

std::vector<std::string> CoNLLUDatabase::tokenize(const std::string& sentence)
{
    std::string s(sentence);
    toLower(s);
    return split(s, " \t");
}

std::vector<std::string> CoNLLUDatabase::tag(const std::vector<std::string>& sentence)
{
    std::vector<WordId> encoded(sentence.size());

    for (size_t i = 0; i < sentence.size(); ++i)
    {
        encoded[i] = words.lookup(sentence[i]);
        if (!words.isValidIndex(encoded[i]))
        {
            encoded[i] = unknownWord.word;
        }
    }

    std::vector<TagId> predicted = hmm.predict(encoded);

    std::vector<std::string> res(predicted.size());

    for (size_t i = 0; i < predicted.size(); ++i)
    {
        CompoundTag tag = tags.lookupIndex(predicted[i]);

        std::string s = sentence[i] + ": " + posTags.lookupIndex(tag.POS) + ": ";
        for (size_t f = 0; f < MAX_FEATURES_PER_WORD; ++f)
        {
            if (tag.features[f].featureNameId == 0)
                break;

            //s += featureNames.lookupIndex(tag.features[f].featureNameId) + "=" + featureValues.lookupIndex(tag.features[f].featureValueId) + ", ";
        }

        res[i] = s;
    }

    return res;
}