#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>

int systemtimeUsec = 0;

#ifndef INCLUDE_STDLIB
#define MEM_ALOC_SIZE 2048
static unsigned char alocMem[MEM_ALOC_SIZE];
static int memAlocPointer = 0;
void * malloc( size_t size ) {
    if ((memAlocPointer + size) > (MEM_ALOC_SIZE - 1)) {
        return 0;
    }
    int p = memAlocPointer;
    if (size & 3) {
        size =  (size & ~3) + 4;
    }

    memAlocPointer += size;
    return (void*) &alocMem[p];

}
#else
int _kill(pid_t pid, int sig) { return 0; };
pid_t _getpid(void) { return 0; };
#endif


/**
 * 
 * 
 */
uint32_t millis() {
    return systemtimeUsec / 1000;
}
