#include "Statistics.h"

#include <iostream>

void Statistics::clear(void)
{
    files.clear();
    messages.clear();
    maxFeaturesNum = 0;
}

void Statistics::updateMaxFeaturesNum(size_t v)
{
    if (v > maxFeaturesNum)
    {
        maxFeaturesNum = v;
    }
}

void Statistics::addFile(const std::string& fileName, size_t sentencesNum, size_t wordsNum)
{
    FileStatistics st;
    st.fileName = fileName;
    st.sentencesNum = sentencesNum;
    st.wordsNum = wordsNum;
    files.push_back(st);
}

void Statistics::addMessage(const std::string& fileName, const std::string& msg)
{
    messages.insert(msg);
}

void Statistics::print(void) const
{
    std::cout << "Files loaded:" << std::endl;
    for (auto& file: files)
    {
       std::cout << "File: " << file.fileName << ": " << file.sentencesNum << " sentences, " << file.wordsNum << " words." << std::endl;
    }

    std::cout << "Messages:" << std::endl;
    for (auto& msg: messages)
    {
        std::cout << msg << std::endl;
    }
}
