#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <errno.h>
#include <sys/un.h>
#include "interface.h"
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdint.h>
#include <sys/types.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <errno.h>


#define socketBufferSize 256
#define SERVERADDRESS "127.0.0.1"
#define Destination_UDP 10000

static int mySocket = -1;
static struct sockaddr_in server;

/*!
 ******************************************************************************************************************
 * \brief   Initialize socket
 *  @param socketName socket name
 *  @param bufferSize send buffer size
 * \return  file descriptor for the socket
 ******************************************************************************************************************
 */
static int udpSocketInit() {
    mySocket = socket(AF_INET, SOCK_DGRAM, 0);

    if(mySocket < 0) {
        fprintf(stderr, "Error opening socket");
        return EXIT_FAILURE;
    }

    bzero((char*)&server, sizeof(server));
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = inet_addr(SERVERADDRESS);
    server.sin_port = htons(Destination_UDP);
    fprintf(stderr, "udp socket is initialized\n\r");
    return 0;
}

/*!
 ********************************************************************
 * \brief   Send buffer to the socket
 * \return  On success, zero.  On error errno
 ********************************************************************
 */
int SendData(uint8_t  *msg, size_t msgSize) {
    udpSocketInit();
    if(sendto(mySocket, msg, msgSize, 0, (const struct sockaddr*)&server, sizeof(server)) < 0) {
        fprintf(stderr, "Error in sendto() %d\n", errno);
        close(mySocket);
        return EXIT_FAILURE;
    }
    close(mySocket);
    return 0;
}
