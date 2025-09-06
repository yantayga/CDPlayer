#include "Serialize.h"
#include <algorithm>
#include <cstring>

const char magic[] = "CONLLU";

void writeMagic(std::ostream& stream)
{
    stream.write(magic, sizeof(magic));
}

bool readMagic(std::istream& stream)
{
    char buffer[sizeof(magic)];
    return stream.read(buffer, sizeof(buffer)) && (std::memcmp(magic, buffer, sizeof(buffer)) == 0);
}

constexpr size_t MAX_STRING_LEN = 1024;


template<>
void serialize<std::string>(std::ostream& stream, const std::string& t)
{
    uint32_t l = std::min(t.length(), MAX_STRING_LEN - 1);
    serialize(stream, l);
    stream.write(t.c_str(), l);
}

template<>
void deserialize<std::string>(std::istream& stream, std::string& t)
{  
    uint32_t l = 0;
    deserialize(stream, l);
    
    l = std::min(size_t(l), MAX_STRING_LEN - 1);
    
    static char buffer[MAX_STRING_LEN];

    stream.read(buffer, l);
    buffer[l] = '\0';

    t = buffer;
}
