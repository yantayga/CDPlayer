#pragma once

#include <string>
#include <vector>
#include <unordered_set>

class Statistics
{
    struct FileStatistics
    {
        std::string fileName;
        size_t sentencesNum;
        size_t wordsNum;
    };

    std::vector<FileStatistics> files;
    std::unordered_set<std::string> messages;
    size_t maxFeaturesNum;

public:
    void clear(void);

    void updateMaxFeaturesNum(size_t v);

    void addFile(const std::string& fileName, size_t sentencesNum, size_t wordsNum);

    void addMessage(const std::string& fileName, const std::string& msg);

    void print(void) const;
};
