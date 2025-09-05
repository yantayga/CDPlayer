#pragma once

#include <algorithm>

#include "Matrix.h"

template<typename N, typename HS, typename ES>
class HMM
{
    Matrix<N> hss2hs;
    Matrix<N> hss2es;

public:
    HMM() {};
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

    void normalize(double smoothingFactor)
    {
        hss2hs.calculateRowSums();
        hss2hs.normalize(smoothingFactor);

        hss2es.calculateRowSums();
        hss2es.normalize(smoothingFactor);
    }

    // Maybe Viterbi algorithm
    std::vector<HS> predict(HS serviceTag, const std::vector<ES>& emissions) const
    {
        if (emissions.empty())
        {
            return std::vector<HS>();
        }

        const size_t hsNum = hss2es.numRows();
        const size_t seqSize = emissions.size();


        Matrix<N> prob(seqSize, hsNum);
        Matrix<N> prev(seqSize, hsNum);

        // From start to first word
        const std::vector<N> inits(hss2hs.getRow(serviceTag));

        // From first word to second
        for(size_t hsTo = 0; hsTo < hsNum; hsTo++)
            prob.at(0, hsTo) = inits[hsTo] * hss2es.at(hsTo, emissions[0]);

        for (size_t i = 1; i < seqSize; ++i)
        {
            for(size_t hsTo = 0; hsTo < hsNum; hsTo++)
            {
                for(size_t hsFrom = 0; hsFrom < hsNum; hsFrom++)
                {
                    N p = prob.at(i - 1, hsFrom) * hss2hs.at(hsFrom, hsTo) * hss2es.at(hsTo, emissions[i]);
                    if (p > prob.at(i, hsFrom))
                    {
                        prob.at(i, hsTo) = p;
                        prev.at(i, hsTo) = hsFrom;
                    }
                }
            }
        }

        std::vector<HS> res(seqSize);

        N pMax = 0;
        res[emissions.size() - 1] = serviceTag;
        for(size_t hsFrom = 0; hsFrom < hsNum; hsFrom++)
        {
            N p = prob.at(emissions.size() - 1, hsFrom); //* hss2hs.at(hsFrom, serviceTag);
            if (p > pMax)
            {
                pMax = p;
                res[emissions.size() - 1] = hsFrom;
            }
        }

        for (size_t i = emissions.size() - 1; i != 0; --i)
        {
            res[i - 1] = prev.at(i, res[i]);
        }

        return res;
    };
};
