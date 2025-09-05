#include "Statistics.h"

#include <iostream>

void Statistics::print(void) const
{
    std::cout << "Files loaded:" << std::endl;
    for (auto& file: files)
    {
        std::cout << "File: " << file.fileName << ": " << file.sentencesNum << " sentences, " << file.wordsNum << " words." << std::endl;
    }

    std::cout << "Errors:" << std::endl;
    for (auto& error: errors)
    {
        std::cout << error << std::endl;
    }
}
