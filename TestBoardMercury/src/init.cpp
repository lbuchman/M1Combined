// Copyright 2024 Leo Buchman
#include <Arduino.h>
#include <string.h>
#include <stdlib.h>
#include <hw.h>
#include <TaskScheduler.h>
#include <watchdog.h>
#include <NativeEthernet.h>
#include <singleLEDLibraryMod.h>
#include <shellFunctor.hpp>
#include <UdpNtpClient.hpp>
#include <udpTerminal.hpp>
#include <etherUtilities.h>
#include <logger.hpp>
#include <persistent.hpp>
#include <networking.hpp>
#include <humidityContr.hpp>
#include <cmd.h>
#include <utility.h>
#include <board.h>
#include <tcpTerminal.hpp>

#define NO_NETWORK_LED_INT 1500000
#define OK_NETWORK_LED_INT 500000
#define WATCHDOG_INT TASK_SECOND * 2
using namespace std;
Scheduler ts;

#define Rev "0.62"


int main() {
    pinMode(WATCHDOG_LED, OUTPUT);
    LoggerSerialDev.begin(LoggerSerialBaudRate);
    ShellFunctor& cshell = ShellFunctor::getInstance();
    loggerInit((Stream&)LoggerSerialDev);
    sllibMod watchDogLed(WATCHDOG_LED, false);
    logger().warn(logger().printHeader,  __FILE__, __LINE__, "\n\r*************** REBOOT ******************** %d");
    String revBuild = String(Rev) + "" + __DATE__ + "-" + __TIME__;
    logger().info(logger().printHeader, __FILE__, __LINE__, "House Automation Controller %s. Build Time <%s-%s>", Rev, __DATE__, __TIME__);
    logger().setLogLevel(logLevel::kLogInfo);

    Task watchdogTaskHw(WATCHDOG_INT, TASK_FOREVER, function<void()> ([](void) -> void {
        watchdog();
    }), &ts, false);

    watchDogLed.begin();
    watchDogLed.ledNoEtherlink();
    watchdogTaskHw.enable();
    enableWatchdog();
    persistentDataInit();
    boardInit();
    initNetworking(ts, watchDogLed);

    SerialTerminal serialTerminal(cshell, ts);
    Serial.begin(115200);
    serialTerminal.begin(&Serial, true);
    TCPTerminal tCPTerminal{cshell, 23, ts};
    tCPTerminal.begin();
    UdpTerminal udpTerminal(cshell, 4111, ts);
    udpTerminal.begin();

#if 0
    shellFunc getdevicedata = [&](int arg_cnt, char **args, Stream & stream) -> int {
        if(!checkArgument(2, arg_cnt, args, (char*) "\t{ \"cmd\": \"%s\", \"desc\": \"returns device data\" },\n\r", stream)) {
            return 1;
        }


        int tag = atoi(args[1]);
        HumidityContr& thermostat = *allControllers[tag];

        JsonDocument  doc;
        JsonObject  jsonDocument = doc.to<JsonObject>();
        jsonDocument["cmd"] = args[0];
        jsonDocument["sensorOk"] = thermostat.getSensorStatus();
        jsonDocument["name"] = thermostat.getDevName();
        jsonDocument["tag"] = tag;
        jsonDocument["isOn"] = thermostat.isOn();
        jsonDocument["kwattused"] = thermostat.getKwattUsed();
        jsonDocument["humidity"] = thermostat.getHumidity();
        jsonDocument["lastPowerResetDateTime"] = thermostat.getLastPowerResetDateTime();
        jsonDocument["temp"] = thermostat.getTemp();
        serializeJsonPretty(jsonDocument, stream);
        return 1;
    };


    shellFunc sethumidsettings = [&](int arg_cnt, char **args, Stream & stream) -> int {
        if(!checkArgument(8, arg_cnt, args, (char*) "\t{ \"cmd\": \"%s\",  \"arg\": \"device [0 - 3] int(Device Watt) deviceName sensorIp sensorPort valuemin valueMax\", \"desc\": \"set humidity device name, power in Watt, sensor Ip in dot to notation, sensorPort, min and max humidity, loggerIP, loggingInterval\" },\n\r", stream)) {
            return 1;
        }


        int tag = atoi(args[1]);

        if((tag > 3) || (tag < 0)) {
            stream.printf("\t{\"cmd\": \"%s\", \"status\": \"false\", \"error\": \"invalid device index\" }\n\r", args[0]);
            return 1;
        }

        TDevice& deviceData = allControllers[tag]->getDeviceData();


        strcpy(deviceData.name, args[3]);
        deviceData.devicePowerWatt = atoi(args[2]);
        deviceData.tag = tag;
        deviceData.sensorIp = str2Ip(args[4]);
        deviceData.sensorPort  = atoi(args[5]);
        deviceData.minValue = atoi(args[6]);
        deviceData.maxValue = atoi(args[7]);
        devicedata2eeprom(deviceData, tag);
        stream.printf("\t{\"cmd\": \"%s\", \"device\": %d, \"status\": true }\n\r", args[0], tag);

        return 1;
    };
#endif

    while(true) {
        ts.execute();
    }

    return 1;
}



