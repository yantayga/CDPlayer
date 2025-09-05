#include "Serialize.h"
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

template<>
void write<std::string>(std::ostream& stream, const std::string& t)
{
    size_t l = t.size();
    write(stream, l);
    stream.write(t.c_str(), l);
}

template<>
void read<std::string>(std::istream& stream, std::string& t)
{
    size_t l = 0;
    read(stream, l);
    t.resize(l);
    stream.read(t.data(), l);
}
