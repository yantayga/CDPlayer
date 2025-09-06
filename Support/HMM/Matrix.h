#pragma once

#include <algorithm>
#include <numeric>
#include <iostream>

template<typename N>
class Vector
{
};

template<typename N>
class Matrix
{
    size_t rows = 0;
    size_t cols = 0;

    N* data = NULL;
    N* rowSums = NULL;

public:
    Matrix() {};

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

    const N& at(size_t row, size_t col) const
    {
        return data[row * cols + col];
    };

    size_t numRows() const
    {
        return rows;
    }

    size_t numCols() const
    {
        return cols;
    }

    void calculateRowSums()
    {
        for (size_t i = 0; i < rows; ++i)
        {
            const size_t rowStart = i * cols;
            rowSums[i] = std::accumulate(&data[rowStart], &data[rowStart + cols], N(0));
        }
    }

    void normalize(N smoothingFactor)
    {
        for (size_t i = 0; i < rows; ++i)
        {
            const size_t rowStart = i * cols;
            std::for_each(&data[rowStart], &data[rowStart + cols], [&](N &n) { n += smoothingFactor; n /= (rowSums[i] + smoothingFactor * cols); });
        }
    }

    void denormalize(N smoothingFactor)
    {
        for (size_t i = 0; i < rows; ++i)
        {
            const size_t rowStart = i * cols;
            std::for_each(&data[rowStart], &data[rowStart + cols], [&](N &n) { n *= (rowSums[i] - smoothingFactor); n-= smoothingFactor * cols; });
        }
    }

    std::vector<N> getRow(size_t row) const
    {
        const size_t rowStart = row * cols;
        return std::vector(&data[rowStart], &data[rowStart + cols]);
    }
    
    void print(void)
    {
        for (size_t i = 0; i < rows; ++i)
        {
            const size_t rowStart = i * cols;
            std::for_each(&data[rowStart], &data[rowStart + cols], [&](N &n) { std::cout << n << ", "; });
            std::cout << " -> " << rowSums[i] << std::endl;
        }
    }
};
