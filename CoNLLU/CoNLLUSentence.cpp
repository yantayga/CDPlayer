#include "CoNLLUSentence.h"
#include "Serialize.h"

void CoNLLUSentence::saveBinary(std::ostream& stream) const
{
    write(stream, words.size());
    for (size_t i = 0; i < words.size(); ++i)
    {
        write(stream, words[i]);
    }
}

void CoNLLUSentence::loadBinary(std::istream& stream)
{
    size_t size = 0;
    read(stream, size);

    words.resize(size);

    for (size_t i = 0; i < size; ++i)
    {
        read(stream, words[i]);
    }
}
