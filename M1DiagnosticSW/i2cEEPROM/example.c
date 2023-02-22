#include <stdio.h>
#include <stdlib.h>
#include <linux/i2c-dev.h>
#include <fcntl.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <linux/i2c.h>

#define DEVICE_PATH "/dev/i2c-1"

#define PAGE_SIZE 64

#define DEVICE_ADDR 0x50 // 0b1010xxxx


int file_desc;
char buffer[PAGE_SIZE + 2]; // 64 bytes + 2 for the address

void teardownI2C()
{
    int result = close(file_desc);
}

void setupI2C()
{
    file_desc = open(DEVICE_PATH, O_RDWR);
    if(file_desc < 0)
    {
    printf("%s\n", strerror(errno));
    exit(1);
    }
    if(ioctl(file_desc, I2C_SLAVE, DEVICE_ADDR) < 0)
    {
    printf("%s\n", strerror(errno));
    teardownI2C();
    exit(1);

    }
}

int write_to_device(char addr_hi, char addr_lo, char * buf, int len)
{
     struct i2c_rdwr_ioctl_data msg_rdwr;
     struct i2c_msg i2cmsg;
     char my_buf[PAGE_SIZE + 2];
     if(len > PAGE_SIZE + 2)
     {
     printf("Can't write more than %d bytes at a time.\n", PAGE_SIZE);
     return -1;
     }
     int i;
     my_buf[0] = addr_hi;
     my_buf[1] = addr_lo;

     for(i= 0; i < len; i++)
     {
     my_buf[2+i] = buf[i];
     }
     msg_rdwr.msgs = &i2cmsg;
     msg_rdwr.nmsgs = 1;
     i2cmsg.addr  = DEVICE_ADDR;
     i2cmsg.flags = 0;
     i2cmsg.len   = 2+len;
     i2cmsg.buf   = my_buf;

    if(ioctl(file_desc,I2C_RDWR,&msg_rdwr)<0)
    {
    printf("write_to_device(): %s\n", strerror(errno));
    return -1;
    }

    return 0;

}

int read_from_device(char addr_hi, char addr_lo, char * buf, int len)
{
    struct i2c_rdwr_ioctl_data msg_rdwr;
    struct i2c_msg             i2cmsg;



    if(write_to_device(addr_hi, addr_lo ,NULL,0)<0)
    {
    printf("read_from_device(): address reset did not work\n");
    return -1;
    }

    msg_rdwr.msgs = &i2cmsg;
    msg_rdwr.nmsgs = 1;

    i2cmsg.addr  = DEVICE_ADDR;
    i2cmsg.flags = I2C_M_RD;
    i2cmsg.len   = len;
    i2cmsg.buf   = buf;

    if(ioctl(file_desc,I2C_RDWR,&msg_rdwr)<0)
    {
    printf("read_from_device(): %s\n", strerror(errno));
    return -1;
    }


    return 0;
}

void fill_buffer(char *buf)
{
    int i = 0;
    while(i < PAGE_SIZE && *buf)
    {
    buffer[i+2] = *buf++;
    }
    while(i++ < PAGE_SIZE-1)
    {
    buffer[i+2] = '*'; // fill the buffer with something
    }
}


int main()
{

    setupI2C(); //setup

    fill_buffer("Here are some words.");
    write_to_device(0x01, 0x00, buffer, PAGE_SIZE);
    char newbuf[PAGE_SIZE];

    if(read_from_device(0x01, 0x00, newbuf, PAGE_SIZE)>0)
    {
    printf("%s\n", newbuf);
    }


    teardownI2C(); //cleanup
    return EXIT_SUCCESS;
}
