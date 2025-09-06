#include <stdio.h>

#include "../Support/Support.h"

int main(int argc, char** argv)
{
    if (argc < 3)
    {
        printf("Usage: %s <path> <sentence>\n", argv[0]);
        return -1;
    }

    DBHandle h = initCoNLLUDB();

    printf("Loading %s ...\n", argv[1]);
    if (loadDirectory(h, argv[1]))
    {
        //printStatistics(h);

//        printf("Saving binary ...\n");
//        saveBinary(h, "./data.bin", true);

//        printf("Loading binary ...\n");
        //loadBinary(h, "./dataT.bin", false);

        printStatistics(h);
//        printf("Saving binary again ...\n");
//        saveBinary(h, "./data1.bin", true);

//        printf("Saving binary w/o sentences...\n");
//        saveBinary(h, "./data0.bin", false);

        printf("Training...\n");
        train(h, 0);

//        saveBinary(h, "./dataT.bin", false);

        printf("Tagging...\n");
        printf(": %s\n", tag(h, argv[2]));
    }
    else
    {
        printf("Failed...\n");
    }

    clearCoNLLUDB(h);

    return 0;
}