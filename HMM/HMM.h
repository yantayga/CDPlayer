#pragma once

#include "Matrix.h"

template<typename N, typename HS, typename ES>
class HMM
{
    Matrix<N> hss2hs;
    Matrix<N> hss2es;
    
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
        ++hss2hs.at(srcHS, dstES);
    }
    
    void train()
    {
        hss2hs.calculateRowSums();
        hss2hs.normalize();
        
        hss2es.calculateRowSums();
        hss2es.normalize();
    }
    
    std::vector<HS> predict(HS startTag, const std::vector<ES>& emissions, HS endTag)
    {
        //std::vector<N>(hss2hs.getRow(startTag));
    };
};
