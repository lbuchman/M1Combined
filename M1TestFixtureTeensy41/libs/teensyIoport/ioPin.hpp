/*
MIT License


Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
#ifndef IOPIN_BASE_H
#define IOPIN_BASE_H
#include <cmath>
#include <core_pins.h>
#include <TaskSchedulerDeclarations.h>
#include <functional>
#include <iostream>
#include <ADC.h>
//#include <ADC_util.h>
#include <datatypes.h>
#include <logger.hpp>
#include <hw.h>

extern Scheduler* pts;

// Class to manage IO functionality
class TIOPin {
    public:
        TIOPin() = delete;
        TIOPin(ADC_Module* _adc, pinIdDefinition _pinId, uint8_t _pin, uint8_t _muxChannel, uint8_t _pinType, String _pinName, uint8_t _pinNumber, String _pinDesc, double _calib, double _reqValue, float _minValue, float _maxValue, groupdDefinition _group = groupdDefinition::NotDefinedGroup): adc(_adc), pinId(_pinId), pin(_pin), muxChannel(int2muxSelection(_muxChannel)), pinType(_pinType), pinName(_pinName), pinNumber(_pinNumber), pinDesc(_pinDesc), calib(_calib), reqValue(_reqValue), minValue(_minValue), maxValue(_maxValue), group(_group) {
            plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "Adding IO-> pin:  pinName %s(%d), hw pin: %2d, muxChannel: %d, pin type: (%d), minValue = %f, maxValue = %f", pinName.c_str(), pinIdDefinition2Int(pinId), pin, muxChannel, pinType, minValue, maxValue);
            Serial.printf("pin = %d to mode %d\n\r, pin, mode");
            pinMode(pin, pinType & 0x03);
            pinMode(ADChSel0, OUTPUT);
            pinMode(ADChSel1, OUTPUT);
            pinMode(ADChSel2, OUTPUT);
        };

        void pinConfig(int mode) {
            pinMode(pin, mode);
        }

        uint8_t get_pin() {
            return pin;
        }

        groupdDefinition get_group() {
            return group;
        }

        void set_pin(int state) {
            digitalWrite(pin, state);
        }

        String get_pinName() {
            return pinName;
        }

        String get_pinDesc() {
            return  pinDesc;
        }

        uint8_t get_pinNumber() {
            return pinNumber;
        }

        double getValue(double refCalib) {
            if(adc) {
                setMuxCh();
                watchdog();
                delay(50);
                watchdog();
                pinValue = (calib * refCalib * adc->analogRead(pin)) / adc->getMaxValue();
                if(adc->fail_flag != ADC_ERROR::CLEAR) {
                    plogger->error(plogger->printHeader, (char*) __FILE__, __LINE__, "Error reading A/D on pin %d, name %s, error = %d", pin, pinName.c_str(), adc->fail_flag);
                    return -1;
                }

                return pinValue;
            }

            return digitalRead(pin) * 3.3;
        }

        pinIdDefinition get_pinId() {
            return pinId;
        }

        muxSelection get_muxChannel() {
            return muxChannel;
        }

        uint8_t get_pinType() {
            return pinType;
        }

        bool isPinAnalog() {
            if(adc) {
                return true;
            }

            return false;
        }

        double getMinValue() {
            return minValue;
        }

        double getMaxValue() {
            return maxValue;
        }

        double getReqValue() {
            return reqValue;
        }

        bool isValueWithinTolerance() {
            if(!adc) {
                return true;
            }

            if(pinValue < maxValue && pinValue > minValue) {
                return true;
            }

            return  false;
        };

        void begin() {
            // scanTask.enable();
            // scanTask.restart();
        }

    private:
        double pinValue;
        ADC_Module*   adc;
        pinIdDefinition pinId;     // defines the pin ID. Pin ID is used for the pin identifier for read or write
        uint32_t     pin;          // hardware pin
        muxSelection muxChannel;   // only for A/D input
        uint8_t      pinType;      // input or output or A/D input, for A/D input MUX channel shall be set
        String       pinName;      // same as pinId but string
        uint8_t      pinNumber;    // pin number
        String       pinDesc;      // pin on m1-3200 connection
        double       calib;        // calibration value, actual = calib * 3.3V RawAD/(2^12)
        double       reqValue;     // expected
        double       minValue;     // expected value min
        double       maxValue;     // expected value max
        groupdDefinition  group;   // group of io for filtering

        void setMuxCh() {
            digitalWrite(ADChSel0, muxChannel & 1);
            digitalWrite(ADChSel1, (muxChannel & 2) >> 1);
            digitalWrite(ADChSel2, (muxChannel & 4) >> 2);
            plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "set mux value %d", muxChannel);

        }

        /* Task scanTask{TASK_MILLISECOND * 300, TASK_FOREVER, "Supervised Activate Task", [this](void) -> void {
                 plogger->debug(plogger->printHeader, (char*) __FILE__, __LINE__, "millis = %ld, doing pin %s", millis(), get_pinName().c_str());
                 getValue();
             }, pts, false, NULL, NULL};
        */
};

#endif
