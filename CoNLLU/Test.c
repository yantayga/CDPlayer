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
        printf("Success...\n");
    }
    else
    {
        printf("Failed...\n");
    }

    clearCoNLLUDB(h);

    return 0;
}