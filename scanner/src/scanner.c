#include <stdio.h>
#include <fcntl.h>
#include <linux/input.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/select.h>
#include <string.h>
/* According to earlier standards */
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include "interface.h"


void INThandler() {
    sleep(1);
    exit(-1);
}

char ttab[] = {
    0,  27, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', '\b',  /* Backspace */
    '\t', 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\n',       /* Enter key */
    0, 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '`',   0,       /* Left shift */
    '\\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/',   0,                  /* Right shift */
    '*',
    0,  /* Alt */
    ' ',  /* Space bar */
    0,  /* Caps lock */
    0,  /* 59 - F1 keycat /proc/bus/input/devices | awk '/SCANNER/{for(a=0;a>=0;a++){getline;{if(/kbd/==1){ print
$NF;exit 0;}}}}' | awk '{ print $4 }' ... > */
    0,   0,   0,   0,   0,   0,   0,   0,
    0,  /* < ... F10 */
    0,  /* 69 - Num lock*/
    0,  /* Scroll Lock */
    0,  /* Home key */
    0,  /* Up Arrow */
    0,  /* Page Up */
    '-',
    0,  /* Left Arrow */
    0,
    0,  /* Right Arrow */
    '+',
    0,  /* 79 - End key*/
    0,  /* Down Arrow */
    0,  /* Page Down */
    0,  /* Insert Key */
    0,  /* Delete Key */
    0,   0,   0,
    0,  /* F11 Key */
    0,  /* F12 Key */
    0,  /* All other keys are undefined */
};


char ntab[] = {
    0,  27, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', '\b',   /* Backspace */
    '\t', 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\n',        /* Enter key */
    0, 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '`',   0,        /* Left shift */
    '\\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/',   0,                   /* Right shift */
    '*',
    0,  /* Alt */
    ' ',  /* Space bar */
    0,  /* Caps lock */
    0,  /* 59 - F1 key ... > */
    0,   0,   0,   0,   0,   0,   0,   0,
    0,  /* < ... F10 */
    0,  /* 69 - Num lock*/
    0,  /* Scroll Lock */
    0,  /* Home key */
    0,  /* Up Arrow */
    0,  /* Page Up */
    '-',
    0,  /* Left Arrow */
    0,
    0,  /* Right Arrow */
    '+',
    0,  /* 79 - End key*/
    0,  /* Down Arrow */
    0,  /* Page Down */
    0,  /* Insert Key */
    0,  /* Delete Key */
    0,   0,   0,
    0,  /* F11 Key */
    0,  /* F12 Key */
    0,  /* All other keys are undefined */
};

char stab[] = {
    0,  27, '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', 0,      /* Backspace */
    0, 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}',   0,         /* Enter key */
    0, 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':', '"',   0, '\n',        /* Left shift */
    0, 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?',   0,                   /* Right shift */
    '*',
    0,  /* Alt */
    ' ',  /* Space bar */
    0,  /* Caps lock */
    0,  /* 59 - F1 key ... > */
    0,   0,   0,   0,   0,   0,   0,   0,
    0,  /* < ... F10 */
    0,  /* 69 - Num lock*/
    0,  /* Scroll Lock */
    0,  /* Home key */
    0,  /* Up Arrow */
    0,  /* Page Up */
    '-',
    0,  /* Left Arrow */
    0,
    0,  /* Right Arrow */
    '+',
    0,  /* 79 - End key*/
    0,  /* Down Arrow */
    0,  /* Page Down */
    0,  /* Insert Key */
    0,  /* Delete Key */
    0,   0,   0,
    0,  /* F11 Key */
    0,  /* F12 Key */
    0,  /* All other keys are undefined */
};

/* return -1 if nt valid */
int isDeviceValid(int fd) {
    struct input_id device_info;
    /* suck out some device information */
    if(ioctl(fd, EVIOCGID, &device_info)) {
        perror("evdev ioctl");
        printf("fd = %d\n", fd);
        return -1;
    }

   // printf("vendor %04hx product %04hx version %04hx", device_info.vendor, device_info.product, device_info.version);

    switch(device_info.bustype) {
        case BUS_PCI :
           // printf(" is on a PCI bus\n");
            break;

        case BUS_USB :
           // printf(" is on a Universal Serial Bus\n");
            return 0;
            break;
    }
    
    return -1;
}

int main(int argc, char* argv[]) {
    int opt;
    fd_set rfds;
    struct timeval tv;
    int retval;
    printf("SCANNER drive rev 0.2\n");
    char devname[64];
    devname[0] = 0;

    while((opt = getopt(argc, argv, "e:")) != EOF) {
        switch(opt) {

            case 'e':
                strcpy(devname, optarg);
                break;

            case 'h':
                printf("usage: %s -e <event device name, like /dev/input/event10>\n", argv[0]);
                return 0;

            default:
                printf("use -h for usage\n");
                return -1;
                break;
        }
    }

    if(!strlen(devname)) {
        printf("usage: %s -e <event device name, like /dev/input/event10>\n", argv[0]);
        return -1;
    }

    struct input_event ev;
    int shift = 0;
    char line[4096], *p = line;

    signal(SIGINT, INThandler);
//    udpSocketInit();

    fputs("starting\n", stdout);

    //fputs("starting\n", stderr);
    int device = -1;
    while(1) {
     
       if (device == -1) {
           printf("Device name is %s\n", devname);
           device = open(devname, O_RDONLY);
           if (device < 0) {
              perror("open()");
              sleep(5);
              continue;
           }
       }
       
       if (isDeviceValid(device) != 0) {
           printf("scanner is no loger connected\n");
           sleep(5);
           device = -1;
           continue;
       }
       
       
        FD_ZERO(&rfds);
        FD_SET(device, &rfds);
        tv.tv_sec = 0;
        tv.tv_usec = 100000;
        retval = select(device + 1, &rfds, NULL, NULL, &tv);
        // usleep(100);

        if(retval == -1) {
            perror("select()");
            continue;
        }

        if(!retval) {
            continue;
        }

        read(device, &ev, sizeof(ev));

        if(ev.type == 1) {
            if(ev.code == 42) {
                shift = ev.value;
            }
            else
                if(ev.value) {
                    //printf("Key: %i State: %i\n", ev.code, ev.value);
                    char *t = shift ? stab : ntab;
                    char ch = t[ev.code];

                    if(ch == '\n') {
                        *p = '\0';
                        fputs(line, stdout);
                        fputc('\n', stdout);
                        fflush(stdout);
                        strcat(line, "\n");
                        SendData((uint8_t*) line, strlen(line) + 1);
                        //fputs(line, stderr); fputc('\n', stderr); fflush(stderr);
                        p = line;
                    }
                    else {
                        *p++ = ch;
                    }
                }
        }
    }
}
