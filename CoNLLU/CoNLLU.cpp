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
const Item& BidirectionalMap<Item, Index>::lookupIndex(const Index index)
{
    return *index2item[index];
}

void CoNLLUDatabase::reset(void)
{
    sentences.clear();
    words.clear();
    tags.clear();

    beginTag = words.lookupOrInsert(defStartTag);
    endTag = words.lookupOrInsert(defEndTag);
    unkTag = words.lookupOrInsert(defUnkTag);

    ShortWordId beginTagT = tags.lookupOrInsert(defStartTag);
    ShortWordId endTagT = tags.lookupOrInsert(defEndTag);
    ShortWordId unkTagT = tags.lookupOrInsert(defUnkTag);

    assert(beginTag == beginTagT && endTag == endTagT && unkTag == unkTagT);
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
                // skip words counter

                //  we need it
                filterNumbers(wordData[1]);
                word.word = words.lookupOrInsert(wordData[1]);
                filterNumbers(wordData[2]);
                word.initialWord = words.lookupOrInsert(wordData[2]);
                word.uPOSTag = tags.lookupOrInsert(wordData[3]);

                // optional
                std::string featuresLine;
                if (wordData.size() > 4) featuresLine += wordData[4];
                if (wordData.size() > 5) featuresLine += '|' + wordData[5];

                const std::vector<std::string> features = split(featuresLine, '|');
                for (auto featurePair: features)
                {
                    std::string name, value;
                    if (parsePair(featurePair, "=", name, value))
                    {
                        if (name.empty() || value.empty())
                        {
                            continue;
                        }

                        Feature f;
                        f.first = tags.lookupOrInsert(name);
                        f.second = tags.lookupOrInsert(value);
                        word.features.push_back(f);
                    }
                }

                try
                {
                    if (wordData.size() > 6) word.depHead = std::stoul(wordData[6]);
                }
                catch(std::invalid_argument&)
                {
                    word.depHead = 0;
                }

                if (wordData.size() > 7) word.depRel = tags.lookupOrInsert(wordData[7]);

                sentence.words.push_back(word);
            }
        }
    }

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

const std::string& CoNLLUDatabase::index2word(const WordId ix)
{
    return words.lookupIndex(ix);
}

WordId CoNLLUDatabase::word2index(const std::string& word)
{
    return words.lookupOrInsert(word);
}

const std::string& CoNLLUDatabase::index2tag(const ShortWordId ix)
{
    return tags.lookupIndex(ix);
}

ShortWordId CoNLLUDatabase::tag2index(const std::string& word)
{
    return tags.lookupOrInsert(word);
}
