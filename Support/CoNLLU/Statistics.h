#pragma once

#include <string>
#include <vector>
#include <unordered_set>

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

    void print(void) const;
};
