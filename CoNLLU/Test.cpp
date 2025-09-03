#include <iostream>

#include "CoNLLU.h"

int main(int argc, char** argv)
{
    if (argc < 2)
    {
        std::cout << "Usage: " << argv[0] << "<path>" << std::endl;
    }

    CoNLLUDatabase db;

    if (db.loadDirectory(argv[1]))
    {
        std::cout << "Words: " << db.words.size() << std::endl;
        std::cout << "Tags: " << db.tags.size() << std::endl;
        std::cout << "Sentences: " << db.sentences.size() << std::endl << std::endl;

        db.words.printIndex();
        std::cout << std::endl;
        db.tags.printIndex();
    }
    else
    {
        std::cout << "Failed..." << std::endl;
    }

    return 0;
}