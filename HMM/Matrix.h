#pragma once

#include <algorithm>
#include <numeric>

template<typename N>
class Matrix
{
    size_t rows;
    size_t cols;

    N* data;
    N* rowSums;

    Matrix();

public:
    Matrix(size_t numrows, size_t numcols)
        : rows(numrows)
        , cols(numcols)
        , data(new N[numrows * numcols])
        , rowSums(new N[numrows])
    {
        std::fill(data, data + numrows * numcols, N(0));
        std::fill(rowSums, rowSums + numrows, N(0));
    };

    ~Matrix()
    {
        delete rowSums;
        delete data;
    };

    N& at(size_t row, size_t col)
    {
        return data[row * cols + col];
    };

    void calculateRowSums()
    {
        for (size_t i = 0; i < rows; ++i)
        {
            size_t rowStart = i * cols;
            rowSums[i] = std::accumulate(&data[rowStart], &data[rowStart + cols], N(0));
        }
    }

    void normalize()
    {
        for (size_t i = 0; i < rows; ++i)
        {
            size_t rowStart = i * cols;
            std::for_each(&data[rowStart], &data[rowStart + cols], [&](N &n) { n /= rowSums[i]; });
        }
    }

    void denormalize()
    {
        for (size_t i = 0; i < rows; ++i)
        {
            size_t rowStart = i * cols;
            std::for_each(&data[rowStart], &data[rowStart + cols], [&](N &n) { n *= rowSums[i]; });
        }
    }

    std::vector<N> getColumn(size_t col)
    {
    }
};
