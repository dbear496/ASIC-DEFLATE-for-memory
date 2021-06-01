#include "Vlz77CompressorDecompressorWrapper.h"
#include "verilated.h"
#include <sys/stat.h>
#include <fstream>
#include <iostream>
#include <time.h>
#define PAGE_SIZE 4096		 // number of bytes in a page
#define TIMEOUT_ENABLE true
#define TIMEOUT_CYCLES 20000

int main(int argc, char **argv, char **env)
{
	Verilated::commandArgs(argc, argv);
	Vlz77CompressorDecompressorWrapper *top = new Vlz77CompressorDecompressorWrapper;
	struct stat fileStats;
	size_t fileBytes;
	char *fileName = argv[argc - 1];

	if (stat(fileName, &fileStats) == 0)
	{
		fileBytes = fileStats.st_size;
		printf("File is %d bytes\n", fileBytes);
	}
	else
	{
		printf("Failed to get file statistics, exitting now\n");
		return 1;
	}

	char inputArray[PAGE_SIZE];
	char outputArray[PAGE_SIZE];
	std::ifstream myFile(fileName, std::ifstream::in | std::ifstream::binary);

	for (int loopIndex = 0; loopIndex < fileBytes / PAGE_SIZE; loopIndex++)
	{
		if (fileBytes >= PAGE_SIZE)
		{
			myFile.read(inputArray, PAGE_SIZE);
		}
		else
		{
			printf("Error, file cannot be read fully\n");
		}

		top->start = 0;
		top->reset = 0;
		top->clock = 0;
		for (size_t index = 0; index < PAGE_SIZE; index++)
		{
			top->dataIn[index] = inputArray[index];
		}
		top->eval();
		top->reset = 1;
		top->eval();
		top->clock = 1;
		top->eval();
		top->clock = 0;
		top->eval();
		top->clock = 1;
		top->eval();
		top->clock = 0;
		top->eval();
		top->reset = 0;
		top->clock = 1;
		top->eval();
		top->clock = 0;
		top->eval();

		top->start = 1;

		bool lastFinished = true;
		long long counter = 0;

		while (!(top->finished && !lastFinished) || counter > 500000)
		{
			counter++;
			lastFinished = top->finished;
			top->eval();
			top->clock = 0;
			top->eval();
			top->clock = 1;
#if TIMEOUT_ENABLE
			if (counter > TIMEOUT_CYCLES) {
				printf("Timeout error, %d clock cycles exceeded\n", counter);
				return -1;
			}
#endif
		}
		bool equal = true;

		for (size_t index = 0; index < PAGE_SIZE; index++)
		{
			outputArray[index] = (char)top->dataOut[index];
		}

		for (size_t index = 0; index < PAGE_SIZE; index++)
		{
			if (outputArray[index] != inputArray[index])
			{
				printf("index %d %d!=%d ", index, (char)outputArray[index], (char)inputArray[index]);
				equal = false;
			}
		}
		if (equal)
		{
			printf("%d:i=o:uncompressed=%d:lz77=%d:compCycles=%d,decompCycles=%d,\n", loopIndex, PAGE_SIZE, top->totalCompressedBytes, top->compressorCycles, top->decompressorCycles);
			std::cout << std::flush;
		}
		else
		{
			printf("loop %d of file %s: ", loopIndex, fileName);
			printf("input does not equal output\n");
		}
	}
	printf(argv[0]);
	printf("\n");
	delete top;
	exit(0);
}
