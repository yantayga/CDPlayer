#pragma once
#include <vector>
#include <iostream>

#include <cstring>

#include "Types.h"
#include "CoNLLUci.h"

struct CoNLLUWord
{
    WordId word = 0;
    WordId initialWord = 0;
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

constexpr size_t MAX_FEATURES_PER_WORD = 16;

struct CompoundTag
{
    ShortWordId POS = 0;
    union
    {
        struct
        {
            ShortWordId featureNameId;
            ShortWordId featureValueId;
        } features[MAX_FEATURES_PER_WORD] = {0};
        // for hashing
        uint64_t featuresH[sizeof(features) / (sizeof(uint64_t))];
    };

    bool operator==(const CompoundTag& other) const
    {
        return POS == other.POS && std::memcmp(featuresH, other.featuresH, sizeof(featuresH)) == 0;
    }
};

template <>
struct std::hash<CompoundTag>
{
  std::size_t operator()(const CompoundTag& k) const
  {
    uint64_t res = (uint64_t)k.POS;
    for (const auto fh: k.featuresH)
    {
        res ^= fh;
    }
    return std::hash<uint64_t>{}(res);
  }
};
