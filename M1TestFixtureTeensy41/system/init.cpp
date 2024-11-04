#include <Arduino.h>
#include <string.h>
#include <iostream>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <cstdint>
#include <ADC.h>
#include <ADC_util.h>
#include <logger.hpp>
#include <Cmd.h>
#include "TaskScheduler.h"
#include <singleLEDLibraryMod.h>
#include <hw.h>
#include <watchdog.h>
#include <shellFunctor.hpp>
#include <init.h>
#include <power.h>
#include <ioEngine.hpp>

// https://github.com/knolleary/pubsubclient/blob/master/examples/mqtt_auth/mqtt_auth.ino
using namespace std;

#define WATCHDOG_INT TASK_SECOND * 2

int hang_counter = 0;

ShellFunctor* pshell;
Scheduler* pts = NULL;
TioEngine* pioEngine = NULL;
SerialTerminal* pserialTerminal;

Logger<256> *plogger = NULL;
extern std::map<String, shellFunc> shellFunctions;


time_t getTeensy3Time() {
    return Teensy3Clock.get();
};

int setupFw() {
    pts = new Scheduler;
    plogger = new Logger<256>(LoggerSerialDev);
    plogger->warn(plogger->printHeader, (char*) __FILE__, __LINE__, "\n\r*************** REBOOT ********************");
    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "M1-3200 test fixture IO test board Rev %s. Build Time <%s-%s>", Rev, __DATE__, __TIME__);
    plogger->setLogLevel(logLevel::kLogInfo);

    pioEngine = new TioEngine;

    static sllibMod watchDogLed(WATCHDOG_LED, false);
    static Task watchdogTaskHw(WATCHDOG_INT, TASK_FOREVER, "Watchdog Task", [](void) -> void {
        watchdog();
    }, pts, false, NULL, NULL);

    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "Teensy RTC clock at: %d", Teensy3Clock.get());
    setSyncProvider(getTeensy3Time);
    setSyncInterval(24 * 3600);

    pshell = new ShellFunctor(shellFunctions);
    static SerialTerminal serialTerminal(*pshell, *pts);
    pserialTerminal = &serialTerminal;
    static SerialTerminal serialTerminal1(*pshell, *pts);
    Serial.begin(115200);
    // Serial1.begin(115200);
    serialTerminal.begin(&Serial,false, nullptr, false);
    // serialTerminal1.begin(&Serial1, false, nullptr, false);

    pioEngine->begin();

    Serial2.begin(115200);
    Serial2.transmitterEnable(26);
    static Task Rs485Echo(TASK_MILLISECOND, TASK_FOREVER, "Rs485Echo Task", [](void) -> void {
        if(Serial2.available()) {
            char test = Serial2.read();
            plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "Rec Char 0x%x\n\r", test);
            Serial2.write(test);
        }
    }, pts, false, NULL, NULL);

    watchDogLed.begin();
    watchdogTaskHw.enable();
    enableWatchdog();
    watchdog();
    Rs485Echo.enable();

    // enable switch to control target power
    static int targetPowerSwitchValue = 1;
    static Task targetPowerSwitch(TASK_MILLISECOND * 100, TASK_FOREVER, "targetPowerSwitch Task", [](void) -> void {
        int powerSwitchValue = round(pioEngine->getIoPinValue(pinIdDefinition::SwitchTargetPower));
        int bootPinSwitchValue = round(pioEngine->getIoPinValue(pinIdDefinition::BootSwitch) / 3);
        int LeverSensor = round(pioEngine->getIoPinValue(pinIdDefinition::LeverSensor) / 3);
        
        if (LeverSensor) {
             pioEngine->setIoPin(pinIdDefinition::TargetPwrControl, 0);
             pioEngine->setIoPin(pinIdDefinition::BatEnable, 0);
             pioEngine->setIoPin(pinIdDefinition::Sol2, 0);
             return;
        }
        
        pioEngine->setIoPin(pinIdDefinition::Sol2, 1);
        
        if(powerSwitchValue == 0) {
            if(powerSwitchValue != targetPowerSwitchValue) {
                int powerPinValue = pioEngine->getIoPinValue(pinIdDefinition::TargetPwrControl);

                if(powerPinValue) {
                    pioEngine->setIoPin(pinIdDefinition::TargetPwrControl, 0);
                    pioEngine->setIoPin(pinIdDefinition::BatEnable, 0);
                }
                else {
                    pioEngine->setIoPin(pinIdDefinition::M1Boot1, bootPinSwitchValue);
                    pioEngine->setIoPin(pinIdDefinition::TargetPwrControl, 1);
                }
            }
        }

        targetPowerSwitchValue = powerSwitchValue;

    }, pts, false, NULL, NULL);
    targetPowerSwitch.enable();
    return 0;
}

void mainLoop() {
    while(true) {
        pts->execute();
    }
}
