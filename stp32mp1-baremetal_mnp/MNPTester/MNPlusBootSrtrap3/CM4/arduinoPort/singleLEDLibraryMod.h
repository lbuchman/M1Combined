/*singleLEDLibrary
* based on Pim Ostendorf - 2017.11.24 single led lib
*/

#ifndef singleLEDLibraryMod_hh
#define singleLEDLibraryMod_hh

#include "singleLEDLibrary.h"
#include "TaskSchedulerDeclarations.h"


static constexpr int watchdogPattern[] = {400, 600};

class sllibMod : public sllib {
    public:
        sllibMod(int pin, bool pwmPin = false): sllib(pin, pwmPin) {
            self = this;
        };

        void begin() {
            setMaxPwm(255);
            ledWatchdog();
            ledTask.begin(ledISR, 100000);
        };

        void ledWatchdog() {
            if(patern == 1) {
                return;
            }

            noInterrupts();
            setPatternSingle((int*) watchdogPattern, sizeof(watchdogPattern) / sizeof(int));
            interrupts();
            patern = 1;
        };

    private:
        int patern = -1;
        static sllibMod* self;
        IntervalTimer ledTask;
        static  void ledISR(void) {
            self->update();
        }
};

#endif
