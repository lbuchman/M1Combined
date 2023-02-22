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

#ifndef DATATYPES_H
#define DATATYPES_H

#include <Arduino.h>

#define INPUT_ANALOG_NOMUX 0x40
#define INPUT_ANALOG_MUX 0x80 //core_pins does not define A/D, so will hack it, will only use 4 first bit for pinMode, if bit 8 is set it will define that the input is analog input, if bit 7 is set the A/D input is direct to MCU, i.e. no Mux
enum  muxSelection : uint8_t {
    muxch0 = 0,
    muxch1 = 1,
    muxch2 = 2,
};

enum  ledState : uint8_t {
    ledOff = 0,
    ledOn = 1,
    ledBlink = 2,
};

enum class groupdDefinition : uint8_t {
    NotDefinedGroup         = 0,
    RibbonCableGroupStatic  = 1,
    RibbonCableGroupDynamic = 2,
    PowerGroup              = 3,
    TestpointsGroup         = 4
};

enum class pinIdDefinition : uint8_t {
    Tamper           = 0,
    BatEnable        = 1,
    BattLoadEn       = 2,
    TargetPwrControl = 3,
    BASE12VAD        = 4,
    ChargeStatus     = 5,
    Bat12VAD         = 6,
    BatChargeVAD     = 7,
    P6V_2            = 8,
    P6V_3            = 9,
    P12V_1           = 10,
    R_S1             = 11,
    R_nRst           = 12,
    R_S7             = 13,
    P6V_1            = 14,
    P12V_2           = 15,
    R_G1             = 16,
    R_G2             = 17,
    R_G3             = 18,
    R_G4             = 19,
    R_G5             = 20,
    R_G6             = 21,
    R_S0             = 22,
    R_S2             = 23,
    /*J6_1_V33*/BatCellBat = 24,
    J6_2_V50         = 25,
    J6_3_V33         = 26,
    J6_4_V33         = 27,
    J6_5_V33         = 28,
    J6_6_V33         = 29,
    J6_7_V33         = 30,
    J6_8_V33         = 31,
    LED_POS          = 32,
    LED_NEG          = 33,
    Boot2            = 34,
    LED_Ready        = 35,
    LED_Prog         = 36,
    LED_Testing      = 37,
    LED_Failed       = 38,
    Buzz             = 39,
    SwitchTargetPower = 40,
    BootSwitch        = 41,
    SwitchPrintLabel = 42,
    SwitchProgram    = 43,
    I2C_SDA          = 44,
    I2C_SCL          = 45,
    M1Boot1          = 46,
    Sol1             = 47,
    Sol2             = 48,
    Sol3             = 49,
    END_OF_LIST      = 50
};



#define int2groupdDefinition(x) static_cast<groupdDefinition>(x)
#define groupdDefinition2Int(x) static_cast<std::underlying_type_t<groupdDefinition>>(x)

#define int2pinIdDefinition(x) static_cast<pinIdDefinition>(x)
#define pinIdDefinition2Int(x) static_cast<std::underlying_type_t<pinIdDefinition>>(x)

#define int2muxSelection(x) static_cast<muxSelection>(x)
#define muxSelection2Int(x) static_cast<std::underlying_type_t<muxSelection>>(x)

#define int2ledState(x) static_cast<ledState>(x)
#define ledState2Int(x) static_cast<std::underlying_type_t<ledState>>(x)


const char * const BoolToString(bool b);
#endif
