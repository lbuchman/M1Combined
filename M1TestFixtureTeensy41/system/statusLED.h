/*singleLEDLibrary
* based on Pim Ostendorf - 2017.11.24 single led lib
*/

#ifndef StatusLed_hh
#define StatusLed_hh
#include <Arduino.h>
#include <wiring.h>
#include <singleLEDLibrary.h>
#include <TaskSchedulerDeclarations.h>
#include <logger.hpp>

extern Scheduler* pts;


class statusLed : public sllib {
    public:
        statusLed(int pin, bool invertedLed, bool pwmPin = false): sllib(pin, invertedLed, pwmPin) {};

        void begin() {
            setMaxPwm(255);
            ledTask.enable();
            setPatternSingle((int*) blinkPattern, sizeof(blinkPattern) / sizeof(int));
            // setOffSingle();
        };
        void setBlinkSingle() {
            setPatternSingle((int*) blinkPattern, sizeof(blinkPattern) / sizeof(int));
        }

    private:
        int blinkPattern[2] = {100, 500};
        Task ledTask{TASK_MILLISECOND * 50, TASK_FOREVER, "LED Task", [this](void) -> void {
                update();
            }, pts, false, NULL, NULL};
};

#endif
