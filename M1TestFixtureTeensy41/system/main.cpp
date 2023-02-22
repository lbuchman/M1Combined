#include <Arduino.h>
#include <cstdint>
#include <init.h>
#include <hw.h>


int main(int argc, char **argv) {
    pinMode(WATCHDOG_LED, OUTPUT);
    LoggerSerialDev.begin(LoggerSerialBaudRate);
    setupFw();
    mainLoop();
}



