#pragma once

#include <fstream>
#include <iostream>

void writeMagic(std::ostream& stream);
bool readMagic(std::istream& stream);

template<typename T>
void serialize(std::ostream& stream, const T& t)
{
    stream.write(reinterpret_cast<const char*>(&t), sizeof(t));
}

template<typename T>
void deserialize(std::istream& stream, T& t)
{
    stream.read(reinterpret_cast<char*>(&t), sizeof(t));
}

template<>
void serialize<std::string>(std::ostream& stream, const std::string& t);

template<>
void deserialize<std::string>(std::istream& stream, std::string& t);

template<typename T>
void serializePtr(std::ostream& stream, const T* const t, uint32_t len)
{
    stream.write(reinterpret_cast<const char*>(t), len * sizeof(T*));
}

template<typename T>
void deserializePtr(std::istream& stream, T*& t, uint32_t len)
{
    stream.read(reinterpret_cast<char*>(t), len * sizeof(T*));
}
