#include <string>
#include <set>
#include <tuple>
#include "drivers/pinconf.hh"
#include "commands.hh"
#include "serial.h"
#include "delay.h"
#include "ddr/ram_tests.hh"
#include "ddr/stm32mp15-osd32mp1-ddr3-1x4Gb.dtsi"
#include "ddr/stm32mp1_ram.h"

extern Serial stream;
static std::tuple<bool, GPIO> textToPort(char _port);
std::tuple<bool, PinNum> textToPin(char* _pin);
std::tuple<bool, PinMode> textToPinMode(char* pinMode);
std::tuple<bool, PinPull> textToPulMode(char* pinMode);
char* toLower(char* s);
char* toUpper(char* s);

/*
 */
void setgpio(int arg_cnt, char **args)  {
    if(arg_cnt != 4) {
        stream.printf("{ \"status\": false, \"error\": \"invalid argument required 4 parameters, got %d, type help followed by enter for help\" }\n\r", arg_cnt);
        return;
    }

    GPIO port;
    bool retStatus;

    std::tie(retStatus, port) = textToPort(args[1][0]);

    if(!retStatus) {
        stream.printf("{ \"status\": false, \"error\": \"invalid port\"}\n\r");
        return;
    };

    PinNum pin;

    std::tie(retStatus, pin) = textToPin(args[2]);

    if(!retStatus) {
        stream.printf("{ \"status\": false, \"error\": \"invalid pin\"}\n\r");
        return;
    };

    int value = atoi(args[3]);

    if(value) PinConf{port, pin}.high();
    else PinConf{port, pin}.low();

    stream.printf("{ \"status\": true }\n\r");
}

/*
 */
void confgpio(int arg_cnt, char **args) {
    if(arg_cnt != 5) {
        stream.printf("{ \"status\": false, \"error\": \"invalid argument required 5 parameters, got %d, type help followed by enter for help\" }\n\r", arg_cnt);
        return;
    }

    GPIO port;
    bool retStatus;
    std::tie(retStatus, port) = textToPort(args[1][0]);

    if(!retStatus) {
        stream.printf("{ \"status\": false, \"error\": \"invalid port\"}\n\r");
        return;
    };

    PinNum pin;

    std::tie(retStatus, pin) = textToPin(args[2]);

    if(!retStatus) {
        stream.printf("{ \"status\": false, \"error\": \"invalid pin\"}\n\r");
        return;
    };

    PinMode pinMode;

    std::tie(retStatus, pinMode) = textToPinMode(args[3]);

    if(!retStatus) {
        stream.printf("{ \"status\": false, \"error\": \"invalid mode\"}\n\r");
        return;
    };

    PinPull pullMode;

    std::tie(retStatus, pullMode) = textToPulMode(args[4]);

    if(!retStatus) {
        stream.printf("{ \"status\": false, \"error\": \"invalid pull mode\"}\n\r");
        return;
    };

    PinConf{port, pin, PinAF::AFNone}.init(pinMode, pullMode);
    stream.printf("{ \"status\": true }\n\r");
};

/*
 */
void getgpio(int arg_cnt, char **args) {
    if(arg_cnt != 3) {
        stream.printf("{ \"status\": false, \"error\": \"invalid argument required 3 parameters, got %d, type help followed by enter for help\" }\n\r", arg_cnt);
        return;
    }

    GPIO port;
    bool retStatus;
    std::tie(retStatus, port) = textToPort(args[1][0]);

    if(!retStatus) {
        stream.printf("{ \"status\": false, \"error\": \"invalid port\"}\n\r");
        return;
    };

    PinNum pin;

    std::tie(retStatus, pin) = textToPin(args[2]);

    if(!retStatus) {
        stream.printf("{ \"status\": false, \"error\": \"invalid pin\"}\n\r");
        return;
    };
    int value = PinConf{port, pin, PinAF::AFNone}.read();
    stream.printf("{ \"status\": true, \"value\": %d, \"port\": %d, \"pin\": %d }\n\r", value, port, pin);
};

/*
 */
void pulsegpio(int arg_cnt, char **args)  {
    if(arg_cnt != 4) {
        stream.printf("{ \"status\": false, \"error\": \"invalid argument required 4 parameters, got %d, type help followed by enter for help\" }\n\r", arg_cnt);
        return;
    }

    GPIO port;
    bool retStatus;

    std::tie(retStatus, port) = textToPort(args[1][0]);

    if(!retStatus) {
        stream.printf("{ \"status\": false, \"error\": \"invalid port\"}\n\r");
        return;
    };

    PinNum pin;

    std::tie(retStatus, pin) = textToPin(args[2]);

    if(!retStatus) {
        stream.printf("{ \"status\": false, \"error\": \"invalid pin\"}\n\r");
        return;
    };

    int value = atoi(args[3]);
    PinConf{port, pin}.low();
    udelay(1000);
    PinConf{port, pin}.high();
    udelay(value);
    PinConf{port, pin}.low();

    stream.printf("{ \"status\": true }\n\r");
}

/*
 */
char* toLower(char* s) {
    for(char *p = s; *p; p++) {
        *p = tolower(*p);
    }

    return s;
}

/*
 */
char* toUpper(char* s) {
    for(char *p = s; *p; p++) {
        *p = toupper(*p);
    }

    return s;
}

std::tuple<bool, GPIO> textToPort(char _port) {
    char lowerCasePort = tolower(_port);

    switch(lowerCasePort) {
        case 'a':
            return std::make_tuple(true, GPIO::A);

        case 'b':
            return std::make_tuple(true, GPIO::B);

        case 'c':
            return std::make_tuple(true, GPIO::C);

        case 'd':
            return std::make_tuple(true, GPIO::D);

        case 'e':
            return std::make_tuple(true, GPIO::E);

        case 'f':
            return std::make_tuple(true, GPIO::F);

        case 'g':
            return std::make_tuple(true, GPIO::G);

        case 'h':
            return std::make_tuple(true, GPIO::H);

        case 'i':
            return std::make_tuple(true, GPIO::I);

        case 'j':
            return std::make_tuple(true, GPIO::J);

        case 'k':
            return std::make_tuple(true, GPIO::K);

        case 'z':
            return std::make_tuple(true, GPIO::Z);
    }

    return std::make_tuple(false, GPIO::A);
}

std::tuple<bool, PinNum> textToPin(char* _pin) {
    int pin = atoi(_pin);

    switch(pin) {
        case 0:
            return std::make_tuple(true, PinNum::_0);

        case 1:
            return std::make_tuple(true, PinNum::_1);

        case 2:
            return std::make_tuple(true, PinNum::_2);

        case 3:
            return std::make_tuple(true, PinNum::_3);

        case 4:
            return std::make_tuple(true, PinNum::_4);

        case 5:
            return std::make_tuple(true, PinNum::_5);

        case 6:
            return std::make_tuple(true, PinNum::_6);

        case 7:
            return std::make_tuple(true, PinNum::_7);

        case 8:
            return std::make_tuple(true, PinNum::_8);

        case 9:
            return std::make_tuple(true, PinNum::_9);

        case 10:
            return std::make_tuple(true, PinNum::_10);

        case 11:
            return std::make_tuple(true, PinNum::_11);

        case 12:
            return std::make_tuple(true, PinNum::_12);

        case 13:
            return std::make_tuple(true, PinNum::_13);

        case 14:
            return std::make_tuple(true, PinNum::_14);

        case 15:
            return std::make_tuple(true, PinNum::_15);
    }

    return std::make_tuple(false, PinNum::_15);
}

std::tuple<bool, PinMode> textToPinMode(char* pinMode) {
    toLower(pinMode);

    if(!strcmp(pinMode, "input")) {
        return std::make_tuple(true, PinMode::Input);
    }

    if(!strcmp(pinMode, "output")) {
        return std::make_tuple(true, PinMode::Output);
    }

    return std::make_tuple(false, PinMode::Alt);
}

std::tuple<bool, PinPull> textToPulMode(char* pullMode) {
    toLower(pullMode);

    if(!strcmp(pullMode, "up")) {
        return std::make_tuple(true, PinPull::Up);
    }

    if(!strcmp(pullMode, "down")) {
        return std::make_tuple(true, PinPull::Down);
    }

    if(!strcmp(pullMode, "none")) {
        return std::make_tuple(true, PinPull::None);
    }

    return std::make_tuple(false, PinPull::None);
}

void ddrtest(int arg_cnt, char **args) {
    if(arg_cnt != 3) {
        stream.printf("{ \"status\": false, \"error\": \"invalid argument required 3 parameters, got %d, type help followed by enter for help\" }\n\r", arg_cnt);
        return;
    }

    RamTests::datum* address = (RamTests::datum*)(DDR_BASE_ADDRESS + strtol(args[1], NULL, 16));
    uint32_t size = (uint32_t) strtol(args[2], NULL, 16);

    RamTests::datum* ret = RamTests::memTestDevice(address, size);

    if(ret) {
        stream.printf("{ \"status\": false, \"error\": \"first address at which integrity problem was uncovered %p\"}\n\r", ret);
        return;
    }

    stream.printf("{ \"status\": true, \"address\": \"%p\", \"size\": \"0x%x\" }\n\r", address, size);
};

void ddradrbus(int arg_cnt, char **args) {
    (void) arg_cnt;
    (void) args;

    RamTests::datum* ret = RamTests::memTestAddressBus((RamTests::datum*) DDR_BASE_ADDRESS, stm32mp1_ddr_get_size());

    if(ret) {
        stream.printf("{ \"status\": false, \"error\": \"first address at which an aliasing problem was uncovered %p\"}\n\r", ret);
        return;
    }

    stream.printf("{ \"status\": true }\n\r");
};

void ddrdatbus(int arg_cnt, char **args) {
    (void) arg_cnt;
    (void) args;

    RamTests::datum ret = RamTests::memTestDataBus((RamTests::datum*) DDR_BASE_ADDRESS);

    if(ret) {
        stream.printf("{ \"status\": false, \"error\": \"first pattern that failed 0x%x\"}\n\r", ret);
        return;
    }

    stream.printf("{ \"status\": true }\n\r");
}
