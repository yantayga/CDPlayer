#pragma once
#include <vector>
#include <iostream>

#include <cstring>
#include <x86intrin.h>

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
    struct
    {
        ShortWordId featureNameId = 0;
        ShortWordId featureValueId = 0;
    } features[MAX_FEATURES_PER_WORD];

    bool operator==(const CompoundTag& other) const
    {
        return POS == other.POS && std::memcmp(features, other.features, sizeof(features)) == 0;
    }
};

template <>
struct std::hash<CompoundTag>
{
  std::size_t operator()(const CompoundTag& k) const
  {
    uint64_t res = (uint64_t)k.POS;
    for (const auto f: k.features)
    {
        res ^= uint64_t(f.featureNameId);
        _rotl(res, 8);
        res ^= uint64_t(f.featureValueId);
        _rotl(res, 8);
    }
    return std::hash<uint64_t>{}(res);
  }
};
