#pragma once

#include <algorithm>

#include "Matrix.h"

template<typename N, typename HS, typename ES>
class HMM
{
    Matrix<N> hss2hs;
    Matrix<N> hss2es;

    HMM();
public:
    HMM(HS hiddenStates, ES emissions)
        : hss2hs(hiddenStates, hiddenStates)
        , hss2es(hiddenStates, emissions)
    {
    };

    void addHiddenState2HiddenState(HS srcHS, HS dstHS)
    {
        ++hss2hs.at(srcHS, dstHS);
    }

    void addHiddenState2Emission(HS srcHS, ES dstES)
    {
        ++hss2es.at(srcHS, dstES);
    }

    void train()
    {
        hss2hs.calculateRowSums();
        hss2hs.normalize();

        hss2es.calculateRowSums();
        hss2es.normalize();
    }

    // Maybe Viterbi algorithm
    std::vector<HS> predict(HS serviceTag, const std::vector<ES>& emissions) const
    {
        if (emissions.empty())
        {
            return std::vector<HS>();
        }

        const size_t hsNum = hss2hs.numRows();

        // step 0: fill with <start> -> first word probs
        std::vector<N> hsPrev(hss2hs.getRow(serviceTag));

        // for calculating the resulting path back; first column always leeds to <start>
        // by column
        std::vector<std::vector<HS>> backPaths(emissions.size(), std::vector<HS>(hsNum, serviceTag));

        for (size_t i = 1; i < emissions.size(); ++i)
        {
            std::vector<N> hsCurrent(hsNum);

            for (HS hsTo = 0; hsTo < hsNum; ++hsTo)
            {
                std::vector<N> hsTmp(hsPrev);
                hss2hs.multiplyByColumn(hsTmp, hsTo);

                HS idx = maxIndex(hsCurrent);

                backPaths[i][hsTo] = idx;
                hsCurrent[hsTo] = hsTmp[idx];
            }

            // multiply elementwise to hs2emissions probs
            hss2es.multiplyByColumn(hsCurrent, i);
            std::swap(hsPrev, hsCurrent);
        }

        // step last: calculating transitions form prevstates to <end>
        // given hsPrev
        hss2hs.multiplyByColumn(hsPrev, serviceTag);

        std::vector<HS> res(emissions.size(), serviceTag);
        //trace back the most probable path
        HS prevState = maxIndex(hsPrev);
        for (size_t i = emissions.size(); i != 0; --i)
        {
            prevState = backPaths[prevState][i - 1];
            res[i - 1] = prevState;
        }

        return res;
    };

    HS maxIndex(const std::vector<N>& v) const
    {
        return *std::max_element(v.begin(), v.end());
    }
};
