#include "boot_media_loader.hh"
#include "clocks.hh"
#include "ddr/ram_tests.hh"
#include "ddr/stm32mp1_ram.h"
#include "delay.h"
#include "drivers/leds.hh"
#include "drivers/uart.hh"
#include "pmic.hh"
#include "print.hh"
#include "stm32mp157cxx_ca7.h"
#include "systeminit.h"
#include "Cmd.h"
#include <tuple>
#include "stm32disco_conf.hh"
#include "drivers/pinconf.hh"
#include "commands.hh"
#include "singleLEDLibrary.h"
#include "system.h"
#include "ddr/stm32mp15-osd32mp1-ddr3-1x4Gb.dtsi"
#include "eeprom24aa02.hh"
#include "MD5.h"

static int logEnabled = 0;

Serial stream;
SerialTerminal serialTerminal;
namespace Board = STM32MP1Disco;
constexpr uint32_t dlytime = 50; // loop time is 50uSec
char *fwRev = (char*) "0.10";
EEPROM24aa02 eeprom{Board::PMIC::I2C_config};
static int searchString(char* strIn, char* strOut, char terminatedBy, int maxLegth);
bool checkEEPROMBlank();
#define EEPROM_START_ADDRESS 0x0
#define EEPROM_START_ADDRESS_EXT 0x70

int main() {
    Board::LED_RUN ledRun;
    Board::LED_CPU ledCpu;
    Board::LED_DBG_LED1 led_debug1;
    Board::LED_DBG_LED2 led_debug2;
    Board::LED_DBG_LED3 led_debug3;
    Board::LED_DBG_LED4 led_debug4;


    auto clockspeed = SystemClocks::init_core_clocks(Board::HSE_Clock_Hz, Board::MPU_MHz);
    security_init();

    Uart<Board::ConsoleUART> console(Board::UartRX, Board::UartTX, Board::UartTX, 115200);
    Uart<Board::Rx422UART> rs422Port(Board::Rx422UartRX, Board::Rx422UartTX, Board::Rx422UartTX, 115200);
    Uart<Board::Rx485UART> rx485Port(Board::Rx485UartRX, Board::Rx485UartTX, Board::Rx485UartDE, 115200);

    for(int count = 0; count < 32; count++) {
        udelay(1000);
        Uart<Board::ConsoleUART>::readChar();
    }

    for(int count = 0; count < 32; count++) {
        Uart<Board::Rx422UART>::putchar(0x30);
        udelay(1000);
        Uart<Board::Rx422UART>::readChar();
    }

    for(int count = 0; count < 32; count++) {
        Uart<Board::Rx485UART>::putchar(0x20);
        udelay(1000);
        Uart<Board::Rx485UART>::readChar();
    }


    if(Board::PMIC::HasSTPMIC) {
        STPMIC1 pmic{Board::PMIC::I2C_config};

        if(!pmic.setup_vddcore_pwr()) {
            stream.printf("Could not setup PMIC VDDCORE\n\r");
        }

        if(!pmic.setup_ddr3_pwr()) {
            stream.printf("Could not setup PMIC DDR voltages\n\r");
        }
    }

    stm32mp1_ddr_setup();

    serialTerminal.setPromtOff();

    Board::EEPROM_Wp.init(PinMode::Output, PinPull::None);
    Board::EEPROM_Wp.low();

    // add commands to the serial terminal
    serialTerminal.begin(&stream);
    serialTerminal.cmdAdd("help", "display help", [](int arg_cnt, char **args) -> void {
        (void) arg_cnt;
        (void) args;
        serialTerminal.help();
    });
    serialTerminal.cmdAdd("promtoff", "disable console promt", [](int arg_cnt, char **args) -> void {
        (void) arg_cnt;
        (void) args;
        serialTerminal.setPromtOff();
    });
    serialTerminal.cmdAdd("promton", "enable console promt", [](int arg_cnt, char **args) -> void {
        (void) arg_cnt;
        (void) args;
        serialTerminal.setPromtOn();
    });
    serialTerminal.cmdAdd("getfwrev", "return fw revision for ict test", [](int arg_cnt, char **args) -> void {
        (void) arg_cnt;
        (void) args;
        stream.printf("{ \"status\": true,  \"fwrev\": \"%s\", \"cpu\": \"m1\" }\n\r", fwRev);
    });
    serialTerminal.cmdAdd("testrs485", "test rs485 loopback test, needs rs485 slave to be connected in echo mode", [](int arg_cnt, char **args) -> void {
        (void) arg_cnt;
        (void) args;

        while(Uart<Board::Rx485UART>::available()) Uart<Board::Rx485UART>::readChar();
        for(int count = 0x30; count < 0x33; count++) {
            Uart<Board::Rx485UART>::putchar(count);
            int timeout = 10;

            while(timeout > 0 && !Uart<Board::Rx485UART>::available()) {
                timeout -= 1;
                udelay(1000);
            };

            char ret = Uart<Board::Rx485UART>::readChar();

            if(ret != count) {
                stream.printf("{ \"status\": false,  \"error\": \"no reply match send = %d rec = %d\" }\n\r", count, ret);
                return;
            }

            udelay(5000);
        }
        stream.printf("{ \"status\": true }\n\r", fwRev);
    });

    serialTerminal.cmdAdd("writeeepromdata", "arg: <serialN> <secret> <force overwrite 1 or do not overwrite0>", [](int arg_cnt, char **args) -> void {
        if(arg_cnt != 4) {
            stream.printf("{ \"status\": false, \"error\": \"invalid argument required 3 parameters, got %d, type help followed by enter for help\" }\n\r", arg_cnt);
            return;
        }
        
        
        int overwrite = atoi(args[3]);
        if (!checkEEPROMBlank() && overwrite == 0 ) {
            stream.printf("{ \"status\": false, \"error\": \"EEPROM is not Blank\" }\n\r");
            return;
        }
        Board::EEPROM_Wp.low();
        char eepromString[128];
        memset(eepromString, 0 , sizeof(eepromString));
        memset(eepromString, 0, sizeof(eepromString));
        sprintf(eepromString, "%s\n%s\n", args[1], args[2]);
        int  dataLength = strlen(eepromString);
        unsigned char* hash = MD5::make_hash(eepromString);
        //generate the digest (hex encoding) of our hash
        char *md5str = MD5::make_digest(hash, 16);
        memcpy(&eepromString[dataLength], hash, 16);

        bool retvalue = eeprom.writeData((uint8_t*) eepromString, dataLength + 16, EEPROM_START_ADDRESS);
        if (!retvalue) {
            stream.printf("{ \"status\": false, \"error\": \"error writing EEPROM\" }\n\r");
            Board::EEPROM_Wp.high();
            return;
        }
        Board::EEPROM_Wp.high();
        stream.printf("{ \"status\": true, \"serial\": \"%s\", \"secret\": \"%s\", \"hash\": \"%s\" }\n\r", args[1], args[2], md5str);
    });

    serialTerminal.cmdAdd("verifyeepromdata", "check EEPROM data integrity", [](int arg_cnt, char **args) -> void {
        if(arg_cnt != 1) {
            stream.printf("{ \"status\": false, \"error\": \"invalid argument required no parameters, got %d, type help followed by enter for help\" }\n\r", arg_cnt);
            return;
        }
        Board::EEPROM_Wp.high();
        uint8_t datain[128];
        memset(datain, 0, sizeof(datain));
        if (!eeprom.readData(datain, sizeof(datain), EEPROM_START_ADDRESS)) {
           stream.printf("{ \"status\": false, \"error\": \"error reading EEPROM\" }\n\r"); 
        }

        char  serial[64];
        char  secret[64];
        int dataPosition1, dataPosition2;
        unsigned char hash[16];
        
        if(!(dataPosition1 = searchString((char*) datain, serial, '\n', 64))) {
            stream.printf("{ \"status\": false, \"error\": \"invalid EEPROM Data, cannot decode serial number\" }\n\r");
            return;
        }
   
        if(!(dataPosition2 = searchString((char*) &datain[dataPosition1], secret, '\n', 64))) {
            stream.printf("{ \"status\": false, \"error\": \"invalid EEPROM Data, cannot decode secret number\" }\n\r");
            return;
        }

         memcpy(hash,&datain[dataPosition1 + dataPosition2], 16); 
         char *md5str = MD5::make_digest(hash, 16);
  
         char eepromString[128];
         memset(eepromString, 0, sizeof(eepromString));
         sprintf(eepromString, "%s\n%s\n", serial, secret);
         unsigned char* hashv = MD5::make_hash(eepromString);
         if (memcmp(hashv,hash, 16)) {
             stream.printf("{ \"status\": false, \"error\": \"invalid EEPROM Data hash\", \"serial\": \"%s\", \"secret\": \"%s\", \"hash\": \"%s\" }\n\r", serial, secret, md5str);
             return;
         }
       
         stream.printf("{ \"status\": true, \"serial\": \"%s\", \"secret\": \"%s\", \"hash\": \"%s\" }\n\r", serial, secret, md5str);
    });
    
    serialTerminal.cmdAdd("checkeeeprom", "checkeeeprom arg: none", [](int arg_cnt, char **args) -> void {
        Board::EEPROM_Wp.low();  
        char* eepromString = (char*) "test string";
        bool retvalue = eeprom.writeData((uint8_t*) eepromString, strlen(eepromString) + 1, EEPROM_START_ADDRESS_EXT);
        if (!retvalue) {
            stream.printf("{ \"status\": false, \"error\": \"error writing EEPROM\" }\n\r");
            Board::EEPROM_Wp.high(); 
            return;
        }  
        udelay(100000);
        Board::EEPROM_Wp.high();

        char* eepromStringRet[16];
        memset(eepromStringRet, 0, sizeof(eepromStringRet));
        if(!eeprom.readData((uint8_t*) eepromStringRet, strlen(eepromString) + 1, EEPROM_START_ADDRESS_EXT)) {
           stream.printf("{ \"status\": false, \"error\": \"error reading EEPROM\" }\n\r"); 
           return;
        }

        if (strncmp((char*) eepromString, (char*) eepromStringRet, sizeof(eepromString))) {
            stream.printf("{ \"status\": false, \"error\": \"check eeprom failed, compare failed\" }\n\r");
            return;
        }

        char* eepromString1 = (char*) "test1 string";
        retvalue = eeprom.writeData((uint8_t*) eepromString, strlen(eepromString) + 1, 200);
        if (!retvalue) {
            stream.printf("{ \"status\": false, \"error\": \"error writing EEPROM\" }\n\r");
            Board::EEPROM_Wp.high(); 
            return;
        }  
        memset(eepromStringRet, 0, sizeof(eepromStringRet));
        if (!eeprom.readData((uint8_t*) eepromStringRet, strlen(eepromString) + 1, 200)) {
           stream.printf("{ \"status\": false, \"error\": \"error reading EEPROM\" }\n\r");
           return;
        }

        if (!strncmp((char*) eepromString1, (char*) eepromStringRet, sizeof(eepromStringRet))) {
            stream.printf("{ \"status\": false, \"error\": \"check eeprom failed Wp pin failed\" }\n\r");
            return;
        }

        stream.printf("{ \"status\": true }\n\r");
    });

    serialTerminal.cmdAdd("testrs422", "send char and recive reply", [](int arg_cnt, char **args) -> void {
        (void) arg_cnt;
        (void) args;
        char ret;
        int count;

        Uart<Board::Rx422UART>::readChar();
        char testBufferTx[6] = "1abr5";
        char testBufferRx[6];

        while(Uart<Board::Rx422UART>::available()) Uart<Board::Rx422UART>::readChar();
        for(count = 0; count < strlen(testBufferTx); count++) {
            Uart<Board::Rx422UART>::putchar(testBufferTx[count]);
            int timeout = 20;

            while(timeout > 0 && !Uart<Board::Rx422UART>::available()) {
                timeout -= 1;
                udelay(1000);
            };

            ret = Uart<Board::Rx422UART>::readChar();

            if(ret != testBufferTx[count]) {
                stream.printf("{ \"status\": false,  \"error\": \"no reply match count = %d send = %d rec = %d\" }\n\r", count, 101,  ret);
                return;
            }
        }

        stream.printf("{ \"status\": true, \"count\": %d }\n\r", count);
    });


    serialTerminal.cmdAdd("setgpio", "arg: <port> <pin> <value>", setgpio);
    serialTerminal.cmdAdd("getgpio", "arg: <port> <pin> <value>", getgpio);
    serialTerminal.cmdAdd("confgpio", "arg: <port> <pin> <mode [input or output]> <pull [up, down, none]>", confgpio);
    serialTerminal.cmdAdd("pulsegpio", "arg: <port> <pin> <width [uSec]>", pulsegpio);
    serialTerminal.cmdAdd("ddradrbus", "Test the addr bus wiring by performing a walking 1's test on the bits of the address and checking for aliasing", ddradrbus);
    serialTerminal.cmdAdd("ddrtest", "Test DDR integrity, arg: <offset from DDR base, in HEX> <size in HEX>", ddrtest);
    serialTerminal.cmdAdd("ddrdatbus", "Test the data bus wiring in a memory region by performing a walking 1's test at a fixed address within that region, arg: <offset from mem base hex> <memsize hex>", ddrdatbus);

    int patternLdDbg1[] = {200, 100, 200, 500};
    led_debug1.setPatternSingle(patternLdDbg1, 4);
    ledRun.setBlinkSingle(100);
    int pattern[] = {50, 100, 50, 500};
    ledCpu.setPatternSingle(pattern, 4);


    while(1) {
        udelay(dlytime);
        systemtimeUsec += dlytime;
        serialTerminal.cmdPoll();
        led_debug1.update();
        ledRun.update();
        led_debug2.update();
        led_debug3.update();
        led_debug4.update();
        ledCpu.update();
    }
}

/*
 * return position of the terminatedBy char, 0 if failed
 * 
 */ 
int searchString(char* strIn, char* strOut, char terminatedBy, int maxLegth) {
    int count = 0;

    while(count  < maxLegth) {
        strOut[count] = strIn[count];

        if(strIn[count] == terminatedBy) {
            count++;
            strOut[count -1] = 0;
            return count;
        }
        count++;
    }
    
    return 0;
}


/*
 * return position of the terminedBy char, 0 if failed
 * 
 */ 
bool checkEEPROMBlank() {
    int count = 0;
    uint8_t datain[128];
    memset(datain, 0, sizeof(datain));
    eeprom.readData(datain, sizeof(datain), 0);
    uint8_t blankData[32];
    memset(blankData, 0xFF, sizeof(blankData));
    return memcmp(blankData, datain, sizeof(blankData)) == 0;
    
    return false;
}
