/*singleLEDLibrary
* A library for non interupting lighting effects for single LED's
* Pim Ostendorf - 2017.11.24
* Lots of changes for STM32MP1 baremetal
*/
#pragma once
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <inttypes.h>
#include "drivers/pinconf.hh"

extern "C" uint32_t millis();

template<GPIO GPIOx, PinNum PINMASK, PinPolarity pinPolarity>
class sllib {
public:
    //public variables and fucntions
    sllib() {
        PinConf{GPIOx, PINMASK}.init(PinMode::Output, PinPull::None, pinPolarity);
        setOff();
    };

    //blink
    void blinkSingle(int speed);
    void blinkSingle(int timeHigh, int timeLow);

    //blink pattern
    void patternSingle(int pattern[], int speed);

    //future update function
    void update();
    void setPatternSingle(int pattern[], int lenghtarray);
    void setBreathSingle(int speed);
    void setFlickerSingle();
    void setBlinkSingle(int speed);
    void setRandomBlinkSingle(int minTime, int maxTime);
    void setOffSingle();
    void setOnSingle() {
        runningFunction = 0;
        setOn();
    }

private:
    //private variables and fucntion
    //gobal variables
    unsigned long milOld = 0;
    int runningFunction = 0;
    int speedp = 0;
    int timep = 0;
    //int pPatt[];
    int* arrP = 0;
    //variables for blinking
    bool ioBlink = false;
    int rndTemp = 0;
    int maxPWM = 10;
    bool _pwmPin = false;;

    //variables for pattern
    int counter = 0;

    void setOn() {
        if (pinPolarity == PinPolarity::Normal)
            PinConf{GPIOx, PINMASK}.high();
        else
            PinConf{GPIOx, PINMASK}.low();
    };
    void setOff() {
        if (pinPolarity == PinPolarity::Normal)
            PinConf{GPIOx, PINMASK}.low();
        else
            PinConf{GPIOx, PINMASK}.high();
    };
};


template<GPIO GPIOx, PinNum PINMASK, PinPolarity pinPolarity>
void sllib<GPIOx, PINMASK, pinPolarity>::update() {
    switch(runningFunction) {
        case 0:
            break;

        case 1:
            patternSingle(arrP, speedp);
            break;

        case 4:
            blinkSingle(speedp);
            break;
    }
}

template<GPIO GPIOx, PinNum PINMASK, PinPolarity pinPolarity> 
void sllib<GPIOx, PINMASK, pinPolarity>::setOffSingle() {
    runningFunction = 0;
    setOff();
}

// simple blinking function
template<GPIO GPIOx, PinNum PINMASK, PinPolarity pinPolarity> 
void sllib<GPIOx, PINMASK, pinPolarity>::blinkSingle(int speed) {
    if((milOld + speed) < millis()) {
        milOld = millis();

        if(ioBlink == false) {
            setOn();
            ioBlink = true;
        }
        else {
            setOff();
            ioBlink = false;
        }
    }
}

template<GPIO GPIOx, PinNum PINMASK, PinPolarity pinPolarity> 
void sllib<GPIOx, PINMASK, pinPolarity>::setRandomBlinkSingle(int minTime, int maxTime) {
    runningFunction = 5;
    speedp = minTime;
    timep = maxTime;
}

template<GPIO GPIOx, PinNum PINMASK, PinPolarity pinPolarity> 
void sllib<GPIOx, PINMASK, pinPolarity>::setPatternSingle(int pattern[], int lengthArray) {
    if(arrP != 0) {
        delete [] arrP;
    }

    arrP = new int [lengthArray];

    for(int i = 0; i < lengthArray; i++) {
        arrP[i] = pattern[i];
    }

    speedp = lengthArray;
    runningFunction = 1;
}

template<GPIO GPIOx, PinNum PINMASK, PinPolarity pinPolarity> 
void sllib<GPIOx, PINMASK, pinPolarity>::setBreathSingle(int speed) {
    runningFunction = 2;
    speedp = speed;
}

template<GPIO GPIOx, PinNum PINMASK, PinPolarity pinPolarity> 
void sllib<GPIOx,PINMASK, pinPolarity>::setFlickerSingle() {
    runningFunction = 3;
}

template<GPIO GPIOx, PinNum PINMASK, PinPolarity pinPolarity> 
void sllib<GPIOx, PINMASK, pinPolarity>::setBlinkSingle(int speed) {
    runningFunction = 4;
    speedp = speed;
}

//function for pattern based blinking, first entry always turns the led high
template<GPIO GPIOx, PinNum PINMASK, PinPolarity pinPolarity> 
void sllib<GPIOx, PINMASK, pinPolarity>::patternSingle(int pattern[], int lengthArray) {
    if(counter < lengthArray) {
        if((milOld + pattern[counter]) < millis()) {
            milOld = millis();

            if(counter % 2 == 0) {
                setOff();
            }
            else {
                setOn();
            }

            counter++;
        }
    }
    else {
        counter = 0;
    }
}

//overload to allow for async blinking
template<GPIO GPIOx, PinNum PINMASK, PinPolarity pinPolarity> 
void sllib<GPIOx, PINMASK, pinPolarity>::blinkSingle(int timeHigh, int timeLow) {
    if(ioBlink == false) {
        if((milOld + timeHigh) < millis()) {
            milOld = millis();
            setOff();
            ioBlink = true;
        }
    }
    else {
        if((milOld + timeLow) < millis()) {
            milOld = millis();
            setOn();
            ioBlink = false;
        }
    }
}

