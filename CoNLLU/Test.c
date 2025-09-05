#include <stdio.h>

#include "CoNLLUci.h"

int main(int argc, char** argv)
{
    if (argc < 2)
    {
        printf("Usage: %s <path>\n", argv[0]);
    }

    DBHandle h = initCoNLLUDB();

    if (loadDirectory(h, argv[1]))
    {
        printStatistics(h);

        saveBinary(h, "./data.bin", true);
        loadBinary(h, "./data.bin", true);
        printStatistics(h);
        saveBinary(h, "./data1.bin", true);
        saveBinary(h, "./data0.bin", false);
    }
    else
    {
        printf("Failed...\n");
    }

    clearCoNLLUDB(h);

    return 0;
}