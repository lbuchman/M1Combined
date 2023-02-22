#pragma once
#include <cstdint>

namespace RamTests {

/*
 * Define NULL pointer value.
 */
#ifndef NULL
#define NULL  (void *) 0
#endif

/*
 * Set the data bus width.
 */
typedef unsigned long datum;

/*
 * Function prototypes.
 */
int run_all(uint32_t ram_start, uint32_t ram_size);
datum memTestDataBus(volatile datum * address);
datum* memTestAddressBus(volatile datum * baseAddress, unsigned long nBytes);
datum * memTestDevice(volatile datum * baseAddress, unsigned long nBytes);

}
