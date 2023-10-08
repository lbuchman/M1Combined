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
#ifndef IOENGINE__H
#define IOENGINE__H

#include <Arduino.h>
#include <ADC.h>
#include <core_pins.h>
#include <functional>
#include <iostream>
#include <ArduinoJson.h>
#include <datatypes.h>
#include <logger.hpp>
#include <hw.h>
#include <ioPin.hpp>
#include <statusLED.h>

#define refVoltage 3.3
#define V12Scale 4.571428571 * refVoltage
#define V033Scale 1.196 * refVoltage
#define BatCellScale 2.0 * refVoltage
#define V0RibbonSelectScale V05Scale / 1.671
#define V05Scale 2.0 * refVoltage
#define V06Scale 2.22 * refVoltage // 2.222
#define LedScale 4.56 * refVoltage 

class TioEngine {
    private:
        ADC*  adc = new ADC;
        std::map<pinIdDefinition, TIOPin*> ioPins = {
            { pinIdDefinition::M1Boot1, new TIOPin(nullptr, pinIdDefinition::M1Boot1, 34, 0, OUTPUT, "M1Boot1", 0, "M1Boot1", 1.0, 0.0, 0.0, 0.0, groupdDefinition::NotDefinedGroup)},
            { pinIdDefinition::Tamper, new TIOPin(nullptr, pinIdDefinition::Tamper, 33, 0, OUTPUT, "TamperPin", 0, "TamperPin", 1.0, 0.0, 0.0, 0.0, groupdDefinition::NotDefinedGroup)},
            { pinIdDefinition::BatEnable, new TIOPin(nullptr, pinIdDefinition::BatEnable, 22, 0, OUTPUT, "BatEnablePin", 0, "BatEnablePin", 0.0, 1.0, 0, 0, groupdDefinition::NotDefinedGroup)},
            { pinIdDefinition::BattLoadEn, new TIOPin(nullptr, pinIdDefinition::BattLoadEn, 21, 0, OUTPUT, "BattLoadEn", 0, "BattLoadEn", 0.0, 1.0, 0, 0, groupdDefinition::NotDefinedGroup)},
            { pinIdDefinition::TargetPwrControl, new TIOPin(nullptr, pinIdDefinition::TargetPwrControl, 20, 0, OUTPUT, "TargetPwrControl", 0, "TargetPwrControl", 0.0, 1.0, 0, 0, groupdDefinition::NotDefinedGroup)},
            { pinIdDefinition::BASE12VAD, new TIOPin(adc->adc0, pinIdDefinition::BASE12VAD, A1, 0, INPUT_ANALOG_NOMUX, "base12VAD", 0, "base12VAD", V12Scale, 12.0, 11.5, 12.5, groupdDefinition::PowerGroup)},
            { pinIdDefinition::Bat12VAD, new TIOPin(adc->adc1, pinIdDefinition::Bat12VAD, A17, 0, INPUT_ANALOG_NOMUX, "bat12VAD", 0, "bat12VAD", V12Scale, 12.0, 11.5, 12.5, groupdDefinition::PowerGroup)},
            { pinIdDefinition::BatChargeVAD, new TIOPin(adc->adc0, pinIdDefinition::BatChargeVAD, A9, 0, INPUT_ANALOG_NOMUX, "batChargeVAD", 0, "batChargeVAD", V12Scale, 13.0, 11.6, 14.5, groupdDefinition::PowerGroup)},
            { pinIdDefinition::SwitchTargetPower, new TIOPin(nullptr, pinIdDefinition::SwitchTargetPower, 35, 0, INPUT_PULLUP, "SwitchTargetPower", 1, "SwitchTargetPower", 0.0, 0, 0, 0, groupdDefinition::NotDefinedGroup)},
            { pinIdDefinition::BootSwitch, new TIOPin(nullptr, pinIdDefinition::BootSwitch, 40, 0, INPUT_PULLUP, "BootSwitch", 2, "BootSwitch", 0.0, 0, 0, 0, groupdDefinition::NotDefinedGroup)},
            { pinIdDefinition::BatCellBat, new TIOPin(adc->adc1, pinIdDefinition::BatCellBat, A15, 0, INPUT_ANALOG_NOMUX, "BatCellBat", 0, "BatCellBat", BatCellScale, 2.8, 3.0, 3.3, groupdDefinition::PowerGroup)},
            { pinIdDefinition::I2C_SDA, new TIOPin(nullptr, pinIdDefinition::I2C_SDA, 18, 0, INPUT, "J5.3", 3, "I2C_SDA", V033Scale, 3.3, 3.1, 3.5, groupdDefinition::RibbonCableGroupDynamic)},
            { pinIdDefinition::P6V_2, new TIOPin(adc->adc0, pinIdDefinition::P6V_2, A0, 2, INPUT_ANALOG_MUX, "J5.5", 5, "P6V_2", V06Scale, 6.0, 5.8, 6.2, groupdDefinition::RibbonCableGroupStatic)},
            { pinIdDefinition::I2C_SCL, new TIOPin(nullptr, pinIdDefinition::I2C_SCL, 19, 0, INPUT, "J5.6", 6, "I2C_SCL", V033Scale, 3.3, 3.1, 3.5, groupdDefinition::RibbonCableGroupDynamic)},
            { pinIdDefinition::P6V_3, new TIOPin(adc->adc0, pinIdDefinition::P6V_3, A0, 3, INPUT_ANALOG_MUX, "J5.7", 7, "P6V_3", V06Scale, 6.0, 5.8, 6.2, groupdDefinition::RibbonCableGroupStatic)},
            { pinIdDefinition::P6V_1, new TIOPin(adc->adc0, pinIdDefinition::P6V_1, A0, 1, INPUT_ANALOG_MUX, "J5.8", 8, "P6V_1", V06Scale, 6.0, 5.8, 6.2, groupdDefinition::RibbonCableGroupStatic)},
            { pinIdDefinition::P12V_1, new TIOPin(adc->adc0, pinIdDefinition::P12V_1, A0, 4, INPUT_ANALOG_MUX, "J5.13", 13, "P12V_1", V12Scale, 11.8, 11.5, 12.5, groupdDefinition::RibbonCableGroupStatic)},
            { pinIdDefinition::P12V_2, new TIOPin(adc->adc0, pinIdDefinition::P12V_2, A0, 0, INPUT_ANALOG_MUX, "J5.14", 14, "P12V_2", V12Scale, 11.8, 11.5, 12.5, groupdDefinition::RibbonCableGroupStatic)},
            { pinIdDefinition::R_S1, new TIOPin(adc->adc0, pinIdDefinition::R_S1, A0, 5, INPUT_ANALOG_MUX, "J5.15", 15, "R_S1", V0RibbonSelectScale, 0.0, -0.5, 0.5, groupdDefinition::RibbonCableGroupDynamic)},
            { pinIdDefinition::R_S2, new TIOPin(adc->adc0, pinIdDefinition::R_S2, A2, 7, INPUT_ANALOG_MUX, "J5.16", 16, "R_S2", V0RibbonSelectScale,  0.0, -0.5, 0.5, groupdDefinition::RibbonCableGroupDynamic)},
            { pinIdDefinition::R_nRst, new TIOPin(adc->adc0, pinIdDefinition::R_nRst, A0, 6, INPUT_ANALOG_MUX, "J5.17", 17, "RnRst", V0RibbonSelectScale,  0.0, -0.5, 0.5, groupdDefinition::RibbonCableGroupDynamic)},
            { pinIdDefinition::R_S0, new TIOPin(adc->adc0, pinIdDefinition::R_S0, A2, 6, INPUT_ANALOG_MUX, "J5.18", 18, "R_S0", V0RibbonSelectScale,  0.0, -0.5, 0.5, groupdDefinition::RibbonCableGroupDynamic)},
            { pinIdDefinition::R_S7, new TIOPin(adc->adc0, pinIdDefinition::R_S7, A0, 7, INPUT_ANALOG_MUX, "J5.19", 19, "R_S7", V0RibbonSelectScale,  0.0, -0.5, 0.5, groupdDefinition::RibbonCableGroupDynamic)},
            { pinIdDefinition::R_G3, new TIOPin(adc->adc0, pinIdDefinition::R_G3, A2, 5, INPUT_ANALOG_MUX, "J5.20", 20, "R_G3", V05Scale,  0.0, -0.5, 0.5, groupdDefinition::RibbonCableGroupStatic)},
            { pinIdDefinition::R_G4, new TIOPin(adc->adc0, pinIdDefinition::R_G4, A2, 4, INPUT_ANALOG_MUX, "J5.22", 22, "R_G4", V05Scale,  0.0, -0.5, 0.5, groupdDefinition::RibbonCableGroupStatic)},
            { pinIdDefinition::R_G1, new TIOPin(adc->adc0, pinIdDefinition::R_G1, A2, 0, INPUT_ANALOG_MUX, "J5.23", 23, "R_G1", V05Scale,  0.0, -0.5, 0.5, groupdDefinition::RibbonCableGroupStatic)},
            { pinIdDefinition::R_G5, new TIOPin(adc->adc0, pinIdDefinition::R_G5, A2, 3, INPUT_ANALOG_MUX, "J5.24", 24, "R_G5", V05Scale,  0.0, -0.5, 0.5, groupdDefinition::RibbonCableGroupStatic)},
            { pinIdDefinition::R_G2, new TIOPin(adc->adc0, pinIdDefinition::R_G2, A2, 1, INPUT_ANALOG_MUX, "J5.25", 25, "R_G2", V05Scale,  0.0, -0.5, 0.5, groupdDefinition::RibbonCableGroupStatic)},
            { pinIdDefinition::R_G6, new TIOPin(adc->adc0, pinIdDefinition::R_G6, A2, 2, INPUT_ANALOG_MUX, "J5.26", 26, "R_G6", V05Scale,  0.0, -0.5, 0.5, groupdDefinition::RibbonCableGroupStatic)},
            { pinIdDefinition::J6_3_V33, new TIOPin(adc->adc0, pinIdDefinition::J6_3_V33, A3, 2, INPUT_ANALOG_MUX, "TP36", 3, "TP36", V033Scale, 3.3, 3.1, 3.6, groupdDefinition::TestpointsGroup)},
            { pinIdDefinition::J6_4_V33, new TIOPin(adc->adc0, pinIdDefinition::J6_4_V33, A3, 3, INPUT_ANALOG_MUX, "TP31", 4, "TP31", V033Scale, 3.3, 3.1, 3.6, groupdDefinition::TestpointsGroup)},
            { pinIdDefinition::J6_5_V33, new TIOPin(adc->adc0, pinIdDefinition::J6_5_V33, A3, 4, INPUT_ANALOG_MUX, "TP34", 5, "TP34", V033Scale, 3.3, 3.1, 3.6, groupdDefinition::TestpointsGroup)},
            { pinIdDefinition::J6_6_V33, new TIOPin(adc->adc0, pinIdDefinition::J6_6_V33, A3, 5, INPUT_ANALOG_MUX, "TP35", 6, "TP35", V033Scale, 3.3, 3.1, 3.6, groupdDefinition::TestpointsGroup)},
            { pinIdDefinition::J6_7_V33, new TIOPin(adc->adc0, pinIdDefinition::J6_7_V33, A3, 6, INPUT_ANALOG_MUX, "TP33", 7, "TP33", V033Scale, 3.3, 3.1, 3.6, groupdDefinition::TestpointsGroup)},
            { pinIdDefinition::J6_2_V50, new TIOPin(adc->adc1, pinIdDefinition::J6_2_V50, A3, 7, INPUT_ANALOG_MUX, "TP025", 4, "TP025",  V05Scale, 5.0, 4.8, 5.3, groupdDefinition::TestpointsGroup)},
            { pinIdDefinition::LED_POS, new TIOPin(adc->adc1, pinIdDefinition::LED_POS, A3, 0, INPUT_ANALOG_MUX,  "J8.1", 1, "LED_POS", LedScale, 0.4, 0.547, 2.64)},
            { pinIdDefinition::LED_NEG, new TIOPin(adc->adc1, pinIdDefinition::LED_NEG, A3, 1, INPUT_ANALOG_MUX, "J8.2", 2, "LED_NEG", LedScale, 0.4, 0.547, 2.64)},
            { pinIdDefinition::Buzz, new TIOPin(nullptr, pinIdDefinition::Buzz, 4, 0, OUTPUT, "Buzz", 0, "Buzz", 0, 1.0, 0, 0)},
            { pinIdDefinition::Sol1, new TIOPin(nullptr, pinIdDefinition::Sol1, 9, 0, OUTPUT, "Sol1", 0, "Sol1", 0, 1.0, 0, 0)},
            { pinIdDefinition::Sol2, new TIOPin(nullptr, pinIdDefinition::Sol2, 10, 0, OUTPUT, "Sol2", 0, "Sol2", 0, 1.0, 0, 0)},
            { pinIdDefinition::Sol3, new TIOPin(nullptr, pinIdDefinition::Sol3, 11, 0, OUTPUT, "Sol3", 0, "Sol3", 0, 1.0, 0, 0)}
        };
        bool beeperPulse;
        int  beeperCount;
        int  beeperDuration;

    public:
        TioEngine() {};
        void printIO(Stream& stream) {
            for(auto const& item : ioPins) {
                stream.printf("pinName %s(%d), hw pin: %2d, muxChannel: %d, pin type: (%d)\n\r", item.second->get_pinName().c_str(), pinIdDefinition2Int(item.second->get_pinId()), item.second->get_pin(), muxSelection2Int(item.second->get_muxChannel()), item.second->get_pinType());
            }
        }

        void begin() {

            adc->adc0->setReference(ADC_REFERENCE::REF_3V3);
            adc->adc0->setAveraging(12); // set number of averages
            adc->adc0->setResolution(12); // set bits of resolution
            adc->adc0->setConversionSpeed(ADC_CONVERSION_SPEED::LOW_SPEED); // change the conversion speed
            adc->adc0->setSamplingSpeed(ADC_SAMPLING_SPEED::LOW_SPEED); // change the sampling speed

            adc->adc1->setReference(ADC_REFERENCE::REF_3V3);
            adc->adc1->setAveraging(12); // set number of averages
            adc->adc1->setResolution(12); // set bits of resolution
            adc->adc1->setConversionSpeed(ADC_CONVERSION_SPEED::LOW_SPEED); // change the conversion speed
            adc->adc1->setSamplingSpeed(ADC_SAMPLING_SPEED::LOW_SPEED); // change the sampling speed

            for(auto const& item : ioPins) {
                if(item.second->isPinAnalog()) {
                    item.second->begin();
                    delay(5);
                }
            }
        }

        void testTestpoints(groupdDefinition group, Stream& stream) {
            DynamicJsonDocument retData(5000);
            JsonArray array = retData.to<JsonArray>();
            DynamicJsonDocument jsonItem(512);

            for(auto const& item : ioPins) {
                if(!item.second->isPinAnalog()) {
                    continue;
                }

                if(item.second->get_group() != group) {
                    continue;
                }

                double value = item.second->getValue(getrefcalib());
                bool valueValid = item.second->isValueWithinTolerance();
                jsonItem.clear();
                jsonItem["pinName"] = item.second->get_pinName();
                jsonItem["pinId"] = pinIdDefinition2Int(item.second->get_pinId());
                jsonItem["pinDesc"] = item.second->get_pinDesc();
                jsonItem["pinNumber"] = item.second->get_pinNumber();
                jsonItem["valueValid"] = valueValid;
                jsonItem["value"] = value;
                jsonItem["minValue"] = item.second->getMinValue();
                jsonItem["maxValue"] = item.second->getMaxValue();
                jsonItem["group"] = groupdDefinition2Int(item.second->get_group());
                array.add(jsonItem);
            }

            serializeJson(retData, stream);
        }

        void buzzerBeep(bool pulse = true) {
            beeperPulse = pulse;
            beeperCount = 7;

            if(pulse) {
                beeperDuration = 5;
            }
            else {
                beeperDuration = 5;
            }

            digitalWrite(ioPins[pinIdDefinition::Buzz]->get_pin(), HIGH);
            buzzerTask.restartDelayed(TASK_SECOND / beeperDuration);
        }

        void testTestpoint(pinIdDefinition pinId, Stream& stream) {
            DynamicJsonDocument jsonItem(512);

            for(auto const& item : ioPins) {
                if(!item.second->isPinAnalog()) {
                    continue;
                }

                if(item.second->get_pinId() != pinId) {
                    continue;
                }

                double value = item.second->getValue(getrefcalib());
                bool valueValid = item.second->isValueWithinTolerance();
                jsonItem.clear();
                jsonItem["pinName"] = item.second->get_pinName();
                jsonItem["pinId"] = pinIdDefinition2Int(item.second->get_pinId());
                jsonItem["pinDesc"] = item.second->get_pinDesc();
                jsonItem["pinNumber"] = item.second->get_pinNumber();
                jsonItem["valueValid"] = valueValid;
                jsonItem["value"] = value;
                jsonItem["minValue"] = item.second->getMinValue();
                jsonItem["maxValue"] = item.second->getMaxValue();
            }

            serializeJson(jsonItem, stream);
        }

        void getIoJson(Stream& stream) {
            DynamicJsonDocument retData(4096 << 1);
            JsonArray array = retData.to<JsonArray>();
            DynamicJsonDocument jsonItem(4096 << 1);

            for(auto const& item : ioPins) {
                jsonItem["pinName"] = item.second->get_pinName();
                jsonItem["pinId"] = pinIdDefinition2Int(item.second->get_pinId());
                jsonItem["group"] = groupdDefinition2Int(item.second->get_group());
                jsonItem["reqValue"] = item.second->getReqValue();

                jsonItem["pinDesc"] = item.second->get_pinDesc();
                jsonItem["pinType"] = item.second->get_pinType();

                array.add(jsonItem);
            }

            serializeJson(retData, stream);
        }

        void setIoPin(pinIdDefinition _pinId, int state) {
            auto item = ioPins.find(_pinId);

            if(item == ioPins.end()) {
                return;
            }

            item->second->set_pin(state);
        }

        void configIoPin(pinIdDefinition _pinId, int config) {
            auto item = ioPins.find(_pinId);

            if(item == ioPins.end()) {
                return;
            }

            item->second->pinConfig(config);
        }


        bool isIoOutput(pinIdDefinition _pinId) {
            auto item = ioPins.find(_pinId);

            if(item == ioPins.end()) {
                return false;
            }

            if(item->second->get_pinType() != OUTPUT) {
                return false;
            }

            return true;
        }

        bool isIoInput(pinIdDefinition _pinId) {
            auto item = ioPins.find(_pinId);

            if(item == ioPins.end()) {
                return false;
            }

            if(item->second->get_pinType() == OUTPUT) {
                return false;
            }

            return true;
        }

        pinIdDefinition isPinValid(int _pinId) {
            if(_pinId < 0 || _pinId >= pinIdDefinition2Int(pinIdDefinition::END_OF_LIST)) {
                return pinIdDefinition::END_OF_LIST;
            }

            return int2pinIdDefinition(_pinId);
        }

        double getIoPinValue(pinIdDefinition _ioPin) {

            auto item = ioPins.find(_ioPin);

            if(item == ioPins.end()) {
                plogger->error(plogger->printHeader, (char*) __FILE__, __LINE__, "cannot find pin");
                return false;
            }

            return item->second->getValue(getrefcalib());
        }

        double getrefcalib() {
            #define REF_AVR 32 
            double calValue = 1.0;
            static double zenerVoltage = 0;
            static int count = 0;
            double refCalib = (refVoltage * adc->adc0->analogRead(A11)) / adc->adc0->getMaxValue();
            if (count < REF_AVR) {
               zenerVoltage += refCalib;
               count += 1;
               return 1.0;
              
            }
            else {
               calValue = 2.448 / (zenerVoltage / REF_AVR);
            }
            // plogger->error(plogger->printHeader, (char*) __FILE__, __LINE__, "%f  %f",  calValue, refCalib);
            // Serial.printf("%f  %f\n\r", calValue, refCalib);
            return calValue;
        }

        Task buzzerTask{TASK_SECOND * 3, TASK_ONCE, "LED Task", [this](void) -> void {
                if(digitalRead(ioPins[pinIdDefinition::Buzz]->get_pin())) {
                    digitalWrite(ioPins[pinIdDefinition::Buzz]->get_pin(), LOW);
                }
                else {
                    digitalWrite(ioPins[pinIdDefinition::Buzz]->get_pin(), HIGH);
                }

                beeperCount -= 1;

                if(!beeperCount) {
                    return;
                }

                if(beeperPulse) {
                    buzzerTask.restartDelayed(TASK_SECOND / beeperDuration);
                }
            }, pts, false, NULL, NULL};

        void singLedControl(statusLed& led, ledState state) {
            switch(state) {
                case ledOff:
                    led.setOffSingle();
                    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "singleLedControl() led.setOffSingle()");
                    break;

                case ledOn:
                    led.seOnSingle();
                    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "singleLedControl() led.setONSingle()");
                    break;

                case ledBlink:
                    led.setBlinkSingle();
                    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "singleLedControl() led.setBlinkSingle()");
                    break;
            }
        }
};

#endif
