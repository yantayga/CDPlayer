#include "CoNLLUSentence.h"
#include "Serialize.h"

void CoNLLUSentence::saveBinary(std::ostream& stream) const
{
    serialize(stream, words.size());
    for (size_t i = 0; i < words.size(); ++i)
    {
        serialize(stream, words[i]);
    }
}

void CoNLLUSentence::loadBinary(std::istream& stream)
{
    size_t size = 0;
    deserialize(stream, size);

    words.resize(size);

    for (size_t i = 0; i < size; ++i)
    {
        deserialize(stream, words[i]);
    }
}
