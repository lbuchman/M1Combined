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

#include <datatypes.h>

#if 0
String InputDefaultStateToString(InputDefaultState state) {
    switch(state) {
        case InputDefaultState::NormallyOpen:
            return "NormallyOpen";

        case InputDefaultState::NormallyClosed:
            return "NormallyClosed";

        default:
            return "not known";
    }
}


String KeypadModeToString(KeypadMode mode)  {
    switch(mode) {
        case KeypadMode::Burst4Bit:
            return "Burst4Bit";

        case KeypadMode::Burst8Bit:
            return "Burst8Bit";

        default:
            return "not known";
    }
}

String ModeOfInputToString(ModeOfInput mode)  {
    switch(mode) {
        case ModeOfInput::Supervised:
            return "Supervised";

        case ModeOfInput::Unsupervised:
            return "Unsupervised";

        default:
            return "not known";
    }
}

String ReaderTypeToString(ReaderType state) {
    switch(state) {
        case ReaderType::Wiegand:
            return "Wiegand";

        case ReaderType::Osdp:
            return "Osdp";

        default:
            return "not known";
    }
}



String IODefinitionToString(IODefinition id) {
    switch(id) {
        case IODefinition::none:
            return "none";

        case IODefinition::d0:
            return "d0";

        case IODefinition::d1:
            return "d1";

        case IODefinition::bz:
            return "bz";

        case IODefinition::led:
            return "led";

        case IODefinition::powerFault:
            return "powerFault";

        case IODefinition::cabTamper:
            return "cabTamper";

        case IODefinition::contrPower:
            return "contrPower";

        case IODefinition::uart:
            return "uart";

        case IODefinition::Reader1Strike:
            return "Reader1Strike";

        case IODefinition::Reader1AuxRelay:
            return "Reader1AuxRelay";

        case IODefinition::Reader2Strike:
            return "Reader2Strike";

        case IODefinition::Reader2AuxRelay:
            return "Reader2AuxRelay";

        case IODefinition::dnStreamEn:
            return "dnStreamEn";

        case IODefinition::supervisedInput1:
            return "supervisedInput1";

        case IODefinition::supervisedInput2:
            return "supervisedInput2";

        case IODefinition::supervisedInput3:
            return "supervisedInput3";

        case IODefinition::supervisedInput4:
            return "supervisedInput4";

        case IODefinition::supervisedInput5:
            return "supervisedInput5";

        case IODefinition::supervisedInput6:
            return "supervisedInput6";

        case IODefinition::supervisedInput7:
            return "supervisedInput7";

        case IODefinition::supervisedInput8:
            return "supervisedInput8";

        case IODefinition::ds:
            return "ds";

        case IODefinition::rex:
            return "rex";

        default:
            return "notKnown";
    }
}

String ControllerIdToString(ControllerId id) {
    switch(id) {
        case ControllerId::MER_Ser3:
            return "MER-Ser3";

        case ControllerId::MER_Ser2:
            return "MER-Ser2";

        case ControllerId::S2_Contr:
            return "S2-Contr";

        default:
            return "Not known Controller " + String(int(id));
    }
}
ControllerId StringToControllerId(String id) {
    if(id == "MER-Ser3") {
        return ControllerId::MER_Ser3;
    }

    if(id == "MER-Ser2") {
        return ControllerId::MER_Ser2;
    }

    if(id == "S2-Contr") {
        return ControllerId::S2_Contr;
    }

    return ControllerId::LNL_INVALID;
}

#endif
const char * const BoolToString(bool b) {
    return b ? "true" : "false";
}
