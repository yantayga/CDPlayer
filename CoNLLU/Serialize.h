#include <fstream>

void writeMagic(std::ostream& stream);
bool readMagic(std::istream& stream);

template<typename T>
void write(std::ostream& stream, const T& t)
{
    stream.write(reinterpret_cast<const char*>(&t), sizeof(t));
}

template<typename T>
bool read(std::istream& stream, T& t)
{
    stream.read(reinterpret_cast<char*>(&t), sizeof(t));
    return true;
}
