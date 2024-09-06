
#ifndef BOARD_H
#define BOARD_H

struct rdIO {
    int pinN;
    int pinMode;
    int defValue;
    int value;
};

struct readerPins {
    rdIO d0;
    rdIO d1;
    rdIO rLed;
    rdIO gLed;
    rdIO bz;
};

void boardInit();

#endif
