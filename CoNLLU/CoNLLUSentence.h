#pragma once
#include <vector>
#include <iostream>


#include "Types.h"
#include "CoNLLUci.h"

struct CoNLLUWord
{
    WordId word = 0;
    TagId tags = 0;
    size_t depHead = 0;
    ShortWordId depRel = 0;
    ShortWordId depRelModifier = 0;
};

struct CoNLLUSentence
{
    std::vector<CoNLLUWord> words;

    void saveBinary(std::ostream& stream) const;
    void loadBinary(std::istream& stream);
};

constexpr size_t MAX_FEATURES_PER_WORD = 11;

struct CompoundTag
{
    WordId initialWord = 0;
    ShortWordId POS = 0;
    struct Features
    {
        ShortWordId featureNameId;
        ShortWordId featureValueId;
    } features[MAX_FEATURES_PER_WORD] = {0};

    bool operator==(const CompoundTag& other) const
    {
        return initialWord == other.initialWord && POS == other.POS && features == other.features;
    }
};

template <>
struct std::hash<CompoundTag>
{
  std::size_t operator()(const CompoundTag& k) const
  {
    return std::hash<uint64_t>{}((uint64_t)k.POS ^ (uint64_t)k.POS << 32);
  }
};
