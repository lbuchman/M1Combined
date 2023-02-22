
#ifndef __INTERFACE_H__
#define __INTERFACE_H__

#include <stdio.h>
#include <inttypes.h>
#include <stdint.h>


#define hostSocketName "/tmp/scanner_socket" // socket name for external IPC

int SendData(uint8_t  *msg, size_t msgSize);
// int udpSocketInit();

#endif
