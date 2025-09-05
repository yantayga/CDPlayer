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
//        hss2hs.print();
//        hss2es.print();
        
        hss2hs.calculateRowSums();
        hss2es.calculateRowSums();

//        hss2hs.print();
//        hss2es.print();

        hss2hs.normalize(smoothingFactor);
        hss2es.normalize(smoothingFactor);

//        hss2hs.print();
//        hss2es.print();
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
//        std::cout << "hsNum " << hsNum << ", seqSize " << seqSize << std::endl;


        Matrix<N> prob(seqSize, hsNum);
        Matrix<N> prev(seqSize, hsNum);

        // From start to first word
        const std::vector<N> inits(hss2hs.getRow(serviceTag));
//        std::cout << "inits [" << serviceTag << "]: ";
//        std::for_each(inits.begin(), inits.end(), [&](const N &n) { std::cout << n << ", "; });
//        std::cout << std::endl << std::endl;

        // From first word to second
//        std::cout << "ITERATION 0 for " << emissions[0] << std::endl;
        for(size_t hsTo = 0; hsTo < hsNum; hsTo++)
            prob.at(0, hsTo) = inits[hsTo] * hss2es.at(hsTo, emissions[0]);
//        prob.print();
//        std::cout << std::endl;

        for (size_t i = 1; i < seqSize; ++i)
        {
//            std::cout << "ITERATION " << i << " for " << emissions[i] << std::endl;
            for(size_t hsTo = 0; hsTo < hsNum; hsTo++)
            {
                for(size_t hsFrom = 0; hsFrom < hsNum; hsFrom++)
                {
                    N p = prob.at(i - 1, hsFrom) * hss2hs.at(hsFrom, hsTo) * hss2es.at(hsTo, emissions[i]);
                    if (p > prob.at(i, hsTo))
                    {
//                        std::cout << "UPDATED " << hsFrom << " -> " << i << " -> " << hsTo << ": " << p << std::endl;
                        prob.at(i, hsTo) = p;
                        prev.at(i, hsTo) = hsFrom;
                    }
                }
            }
//            prob.print();
//            std::cout << std::endl;
        }

        std::vector<HS> res(seqSize);

        N pMax = 0;
        res[emissions.size() - 1] = serviceTag;
        for(size_t hsFrom = 0; hsFrom < hsNum; hsFrom++)
        {
            N p = prob.at(emissions.size() - 1, hsFrom) * hss2hs.at(hsFrom, serviceTag);
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
