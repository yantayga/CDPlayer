#pragma once

#include <algorithm>
#include <numeric>
#include <iostream>

#include "../Serialize/Serialize.h"

template<typename N>
class Vector
{
};

template<typename N>
class Matrix
{
    uint32_t rows = 0;
    uint32_t cols = 0;

    N* data = NULL;
    N* rowSums = NULL;

    void alloc(void)
    {
        data = new N[rows * cols];
        rowSums = new N[rows];
        std::fill(data, data + rows * cols, N(0));
        std::fill(rowSums, rowSums + rows, N(0));
    }
    
    void free(void)
    {
        delete rowSums;
        delete data;
    }
    
public:
    Matrix() {};

    Matrix(uint32_t numrows, uint32_t numcols)
        : rows(numrows)
        , cols(numcols)
    {
        alloc();
    };

    ~Matrix()
    {
        free();
    };

    N& at(uint32_t row, uint32_t col)
    {
        return data[row * cols + col];
    };

    const N& at(uint32_t row, uint32_t col) const
    {
        return data[row * cols + col];
    };

    uint32_t numRows() const
    {
        return rows;
    }

    uint32_t numCols() const
    {
        return cols;
    }

    void calculateRowSums()
    {
        for (uint32_t i = 0; i < rows; ++i)
        {
            const uint32_t rowStart = i * cols;
            rowSums[i] = std::accumulate(&data[rowStart], &data[rowStart + cols], N(0));
        }
    }

    void normalize(N smoothingFactor)
    {
        for (uint32_t i = 0; i < rows; ++i)
        {
            const uint32_t rowStart = i * cols;
            std::for_each(&data[rowStart], &data[rowStart + cols], [&](N &n) { n += smoothingFactor; n /= (rowSums[i] + smoothingFactor * cols); });
        }
    }

    void denormalize(N smoothingFactor)
    {
        for (uint32_t i = 0; i < rows; ++i)
        {
            const uint32_t rowStart = i * cols;
            std::for_each(&data[rowStart], &data[rowStart + cols], [&](N &n) { n *= (rowSums[i] - smoothingFactor); n-= smoothingFactor * cols; });
        }
    }

    std::vector<N> getRow(uint32_t row) const
    {
        const uint32_t rowStart = row * cols;
        return std::vector(&data[rowStart], &data[rowStart + cols]);
    }
    
    void print(void)
    {
        for (uint32_t i = 0; i < rows; ++i)
        {
            const uint32_t rowStart = i * cols;
            std::for_each(&data[rowStart], &data[rowStart + cols], [&](N &n) { std::cout << n << ", "; });
            std::cout << " -> " << rowSums[i] << std::endl;
        }
    }

    void saveBinary(std::ostream& stream) const
    {
        serialize(stream, rows);
        serialize(stream, cols);

        serializePtr(stream, data, rows * cols);
        serializePtr(stream, rowSums, rows);
    }
    
    void loadBinary(std::istream& stream)
    {
        deserialize(stream, rows);
        deserialize(stream, cols);

        free();
        alloc();

        deserializePtr(stream, data, rows * cols);
        deserializePtr(stream, rowSums, rows);
    }
};
