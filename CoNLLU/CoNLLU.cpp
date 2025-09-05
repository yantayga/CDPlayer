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

#include <gzstream.h>

#include "CoNLLUStd.h"

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
        : posTags(POS_TAGS)
        , featureNames(FEATURE_NAMES)
        , featureValues(FEATURE_VALUES)
        , depRels(DEP_RELS)
        , depRelModifiers(DEP_RELS_MODIFIERS)
{
    reset();

    unknownWord.word = words.lookupOrInsert("");
    unkWordOnly.words.push_back(unknownWord);
}

void CoNLLUDatabase::reset(void)
{
    sentences.clear();
    words.clear();
    tags.clear();

    statistics.files.clear();
    statistics.errors.clear();
    statistics.maxFeaturesNum = 0;

    serviceTag = words.lookupOrInsert(defServiceTag);
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

bool fixFeatureName(std::string& s, std::string& pos)
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
    if (s == "no" || s == "full" || s.starts_with("long"))
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
        size_t lines = 0;
        for (std::string line; std::getline(stream, line, '\n');)
        {
            ++lines;
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
                CoNLLUWord word = {0};

                CompoundTag tag = {0};

                // skip words counter wordData[0]

                filterNumbers(wordData[1]);
                word.word = words.lookupOrInsert(wordData[1]);

                filterNumbers(wordData[2]);
                word.initialWord = words.lookupOrInsert(wordData[2]);

                tag.POS = posTags.lookup(fixTag(wordData[3]));
                if (tag.POS > posTags.size())
                {
                    statistics.errors.insert("Unknown POS tag: '" + wordData[3] + "'.");
                    tag.POS = posTags.lookup("x");
                    sentence.words.push_back(word);
                    continue;
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
                    if (!parsePair(featurePair, "=", name, value))
                    {
                        statistics.errors.insert("Wrong feature pair '" + featurePair + "' for POS tag '" + wordData[3] + "'.");
                        continue;
                    }

                    std::string newPOS;
                    if (name.empty() || value.empty() || !fixFeatureName(name, newPOS)|| !fixFeatureValue(value))
                    {
                        statistics.errors.insert("Ignored feature pair '" + featurePair + "' for POS tag '" + wordData[3] + "'.");
                        if (!newPOS.empty()) tag.POS = posTags.lookup(newPOS);
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
                        tag.features[addedFeatures].featureNameId = fname;
                        tag.features[addedFeatures].featureValueId = fvalue;
                        ++addedFeatures;
                    }

                    if (addedFeatures >= sizeof(tag.features) / sizeof(tag.features[0]))
                    {
                        statistics.errors.insert("Maximum features number reached for POS tag '" + wordData[3] + "'.");
                        break;
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
                    if (depRel > depRels.size())
                    {
                        statistics.errors.insert("Unknown dependency relation '" + depRelMain + "' for POS tag '" + wordData[3] + "'." );
                    }
                    else
                    {
                        word.depRel = depRel;
                    }

                    ShortWordId depRelModifier = depRelModifiers.lookup(depRelMod);
                    if (depRelModifier > depRelModifiers.size())
                    {
                        statistics.errors.insert("Unknown dependency relation modifier '" + depRelMain + ": " + depRelMod + "' for POS tag '" + wordData[3] + "'.");
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
    if (res > words.size())
    {
        return unknownWord.word;
    }

    return res;
}

void CoNLLUDatabase::printStatistics(void)
{
    std::cout << "Database statistics:" << std::endl;

    std::cout << "Bits for POS tag: " << posTags.bits() << std::endl;
    std::cout << "Bits for feature name: " << featureNames.bits() << std::endl;
    std::cout << "Bits for feature value: " << featureValues.bits() << std::endl;
    std::cout << "Bits for dependency relation: " << depRels.bits() << std::endl;
    std::cout << "Bits for dependency relation modifiers: " << depRelModifiers.bits() << std::endl;

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

        size_t size = 0;
        deserialize(stream, size);

        if (useSentences)
        {
            sentences.resize(size);

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
    return split(s, ' ');
}

std::vector<std::string> CoNLLUDatabase::tag(const std::vector<std::string>& sentence)
{
    std::vector<WordId> encoded(sentence.size());

    for (size_t i = 0; i < sentence.size(); ++i)
    {
        encoded[i] = words.lookup(sentence[i]);
        std::cout << sentence[i] << " -> " << encoded[i] << std::endl;
        if (encoded[i] > words.size())
            encoded[i] = unknownWord.word;
    }

    std::vector<TagId> predicted = hmm.predict(encoded);

    std::vector<std::string> res(predicted.size());

    std::cout << "tags " << tags.size() << ", words " << words.size() << std::endl;
    std::cout << "posTags " << posTags.size() << ", featureNames " << featureNames.size() << ", featureValues " << featureValues.size() << std::endl;
    for (size_t i = 0; i < predicted.size(); ++i)
    {
        CompoundTag tag = tags.lookupIndex(predicted[i]);

        std::cout << sentence[i] << " -> " << predicted[i] << " -> " << int(tag.POS) << std::endl;
        for (size_t f = 0; f < MAX_FEATURES_PER_WORD; ++f)
        {
            std::cout << int(tag.features[f].featureNameId) << ":" << int(tag.features[f].featureValueId) << ", ";
        }
        std::cout  << std::endl;

        std::string s = sentence[i] + ": " + posTags.lookupIndex(tag.POS) + ": ";
        for (size_t f = 0; f < MAX_FEATURES_PER_WORD; ++f)
        {
            if (tag.features[f].featureNameId == 0)
                break;

            s += featureNames.lookupIndex(tag.features[f].featureNameId) + "=" + featureValues.lookupIndex(tag.features[f].featureValueId) + ", ";
        }

        res[i] = s;
    }

    return res;
}