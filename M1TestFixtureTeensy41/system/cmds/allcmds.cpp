#include <shellFunctor.hpp>
#include <shellFunctor.hpp>
#include <TaskSchedulerDeclarations.h>
#include <TimeLib.h>
#include <datatypes.h>
#include <base64.hpp>
#include <logger.hpp>
#include <watchdog.h>
#include <ioEngine.hpp>
#include <Cmd.h>

extern Scheduler* pts;
extern ShellFunctor* pshell;
extern TioEngine* pioEngine;
extern SerialTerminal* pserialTerminal;

inline const char * const BoolToString(bool b) {
    return b ? "true" : "false";
}

bool checkArgument(int expectedArgCnt, int arg_cnt, char **args, char* helpFormat, Stream & stream) {
    if(arg_cnt == 0xFF && helpFormat) {
        stream.printf(helpFormat, args[0]);
        return false;
    }

    if(arg_cnt != expectedArgCnt) {
        plogger->error(plogger->printHeader, (char*) __FILE__, __LINE__, "%s invalid argument", args[0]);
        plogger->error(plogger->printHeader, (char*) __FILE__, __LINE__, "%d  %d ", arg_cnt, expectedArgCnt);
        stream.printf("\t{ \"cmd\": \"%s\", \"status\": false, \"error\": \"invalid argument\" }\n\r", args[0]);
        return false;
    }

    return true;
};

shellFunc getio = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(1, arg_cnt, args, (char*) "{ \"cmd\": \"%s\", \"desc\": \"print io ports configuration\" },\n\r", stream)) {
        return 1;
    }

    pioEngine->getIoJson(stream);
    stream.printf("\n\r");
    return 1;
};

shellFunc printio = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(1, arg_cnt, args, (char*) "{ \"cmd\": \"%s\", \"desc\": \"print io ports configuration\" },\n\r", stream)) {
        return 1;
    }

    pioEngine->printIO(stream);
    return 1;
};

shellFunc getIoPin = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(2, arg_cnt, args, (char*) "{ \"cmd\": \"%s\",  \"arg\": \"pinId\"\"desc\": \"get io pin\" },\n\r", stream)) {
        return 1;
    }

    pinIdDefinition pinId = pioEngine->isPinValid(atoi(args[1]));

    if(pinId == pinIdDefinition::END_OF_LIST) {
        stream.printf("\t{\"cmd\": \"%s\", \"status\": \"false\", \"error\": \"invalid argument - pinId\" }\n\r", args[0]);
        return 1;
    }

    if(!pioEngine->isIoInput(pinId)) {
        stream.printf("\t{\"cmd\": \"%s\", \"status\": \"false\", \"error\": \"invalid pin type\" }\n\r", args[0]);
        return 1;
    }

    float vdouble = pioEngine->getIoPinValue(int2pinIdDefinition(pinId));
    stream.printf("\t{\"cmd\": \"%s\", \"status\": true, \"value\": %llf  }\n\r", args[0], vdouble);
    return 1;
};

shellFunc testTestpoints = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(2, arg_cnt, args, (char*) "{ \"cmd\": \"%s\",  \"arg\": \"[testpoints group] \", \"desc\": \"test all analog test points\" },\n\r", stream)) {
        return 1;
    }

    groupdDefinition group = int2groupdDefinition(atoi(args[1]));
    pioEngine->testTestpoints(group, stream);
    stream.printf("\n\r");
    return 1;
};

shellFunc testTestpoint = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(2, arg_cnt, args, (char*) "{ \"cmd\": \"%s\",  \"arg\": \"[pinId] \", \"desc\": \"test all analog test points\" },\n\r", stream)) {
        return 1;
    }

    pinIdDefinition pinId = int2pinIdDefinition(atoi(args[1]));
    pioEngine->testTestpoint(pinId, stream);
    stream.printf("\n\r");
    return 1;
};

shellFunc setIoPin = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(3, arg_cnt, args, (char*) "{ \"cmd\": \"%s\",  \"arg\": \"state [ 0 or 1]\", \"desc\": \"set io pin\" },\n\r", stream)) {
        return 1;
    }

    int state = atoi((args[2]));

    if(state < 0 || state > 1) {
        stream.printf("\t{\"cmd\": \"%s\", \"status\": \"false\", \"error\": \"invalid argument - state\" }\n\r", args[0]);
        return 1;
    }

    pinIdDefinition pinId = pioEngine->isPinValid(atoi(args[1]));

    if(pinId == pinIdDefinition::END_OF_LIST) {
        stream.printf("\t{\"cmd\": \"%s\", \"status\": \"false\", \"error\": \"invalid argument - pinId\" }\n\r", args[0]);
        return 1;
    }

    if(!pioEngine->isIoOutput(int2pinIdDefinition(pinId))) {
        stream.printf("\t{\"cmd\": \"%s\", \"status\": false, \"error\": \"invalid pin type for output\" }\n\r", args[0]);
        return 1;
    }

    pioEngine->setIoPin(int2pinIdDefinition(pinId), state);

    stream.printf("\t{\"cmd\": \"%s\", \"status\": true }\n\r", args[0]);
    return 1;
};

shellFunc configiopin = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(3, arg_cnt, args, (char*) "{ \"cmd\": \"%s\",  \"arg\": \"pinId pinconfig [ from 0, In, Out, IN_PUP, IN_PDW, OUT_OD, In_Dis]\", \"desc\": \"config io pin for input or output\" },\n\r", stream)) {
        return 1;
    }

    int pinConfig = atoi((args[2]));

    pinIdDefinition pinId = pioEngine->isPinValid(atoi(args[1]));

    if(pinId == pinIdDefinition::END_OF_LIST) {
        stream.printf("\t{\"cmd\": \"%s\", \"status\": \"false\", \"error\": \"invalid argument - pinId\" }\n\r", args[0]);
        return 1;
    }
    pioEngine->configIoPin(int2pinIdDefinition(pinId), pinConfig);

    stream.printf("\t{\"cmd\": \"%s\", \"status\": true }\n\r", args[0]);
    return 1;
};


shellFunc batteryon = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(1, arg_cnt, args, (char*) "{ \"cmd\": \"%s\", \"arg\": \"null\"},\n\r", stream)) {
        return 1;
    }

    pioEngine->setIoPin(pinIdDefinition::BatEnable, 1);
    stream.printf("\t{\"cmd\": \"%s\", \"status\": true }\n\r", args[0]);
    return 1;
};

shellFunc batteryoff = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(1, arg_cnt, args, (char*) "{ \"cmd\": \"%s\", \"arg\": \"null\"},\n\r", stream)) {
        return 1;
    }

    pioEngine->setIoPin(pinIdDefinition::BatEnable, 0);
    stream.printf("\t{\"cmd\": \"%s\", \"status\": true }\n\r", args[0]);

    return 1;
};

shellFunc batteryloadon = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(1, arg_cnt, args, (char*) "{ \"cmd\": \"%s\", \"arg\": \"null\"},\n\r", stream)) {
        return 1;
    }

    pioEngine->setIoPin(pinIdDefinition::BattLoadEn, 1);
    stream.printf("\t{\"cmd\": \"%s\", \"status\": true }\n\r", args[0]);
    return 1;
};

shellFunc batteryloadoff = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(1, arg_cnt, args, (char*) "{ \"cmd\": \"%s\", \"arg\": \"null\"},\n\r", stream)) {
        return 1;
    }

    pioEngine->setIoPin(pinIdDefinition::BattLoadEn, 0);
    stream.printf("\t{\"cmd\": \"%s\", \"status\": true }\n\r", args[0]);

    return 1;
};

shellFunc targetpoweron = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(1, arg_cnt, args, (char*) "{ \"cmd\": \"%s\", \"arg\": \"null\"},\n\r", stream)) {
        return 1;
    }

    pioEngine->setIoPin(pinIdDefinition::TargetPwrControl, 1);
    stream.printf("\t{\"cmd\": \"%s\", \"status\": true }\n\r", args[0]);
    return 1;
};

shellFunc targetpoweroff = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(1, arg_cnt, args, (char*) "{ \"cmd\": \"%s\", \"arg\": \"null\"},\n\r", stream)) {
        return 1;
    }

    pioEngine->setIoPin(pinIdDefinition::TargetPwrControl, 0);
    stream.printf("\t{\"cmd\": \"%s\", \"status\": true }\n\r", args[0]);

    return 1;
};

shellFunc settime = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(3, arg_cnt, args, (char*) "{ \"cmd\": \"%s\", \"arg\": \"epochTime\", \"desc\": \"<epoch unix epoch time>\" },\n\r", stream)) {
        return 1;
    }

    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "executing %s %s %s", args[0], args[1], args[2]);
    time_t timeNow = atoi(args[1]);
    time_t timeRef = timeNow;
    Teensy3Clock.set(timeRef);
    setTime(timeRef);
    stream.printf("\t{ \"cmd\": \"%s\", \"status\": true }\n\r", args[0]);
    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "new time is set");
    return 1;
};


shellFunc uptime = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(arg_cnt == 0xFF) {
        stream.printf("{ \"cmd\": \"%s\", \"desc\": \"display system uptime\" },\n\r", args[0]);
        return 1;
    }

    long day = 86400000; // 86400000 milliseconds in a day
    long hour = 3600000; // 3600000 milliseconds in an hour
    long minute = 60000; // 60000 milliseconds in a minute
    long second =  1000; // 1000 milliseconds in a second
    long timeNow = millis();

    int days = timeNow / day ;                                //number of days
    int hours = (timeNow % day) / hour;                       //the remainder from days division (in milliseconds) divided by hours, this gives the full hours
    int minutes = ((timeNow % day) % hour) / minute ;         //and so on...
    int seconds = (((timeNow % day) % hour) % minute) / second;
    stream.printf("\t{ \"cmd\": \"%s\", \"status\": true, \"days\": %d, \"hours\": %2d, \"minutes\": %2d,  \"seconds\": %2d }\n\r",  args[0], days, hours, minutes, seconds);
    return 1;
};

shellFunc date = [](int arg_cnt, char **args, Stream & stream) -> int {
    (void)arg_cnt;

    if(arg_cnt == 0xFF) {
        stream.printf("{ \"cmd\": \"%s\", \"desc\": \"display system date and time\" },\n\r", args[0]);
        return 1;
    }

    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "%s %2d %2d:%2d:%2d UTC %d", monthShortStr(month()), day(), hour(), minute(), second(), year());
    stream.printf("\t{ \"cmd\": \"%s\", \"status\": true, \"year\": %d, \"day\": %d, \"month\": %d, \"hour\": %d, \"minute\": %d,  \"second\": %d }\n\r", args[0], year(), day(), month(), hour(), minute(), second());
    return 1;
};

shellFunc epoch = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(1, arg_cnt, args, (char*) "{ \"cmd\": \"%s\", \"desc\": \"display apoch time\" },\n\r", stream)) {
        return 1;
    }

    stream.printf("\t{ \"cmd\": \"%s\", \"status\": true, \"epoch\": \"%d\"}\n\r", args[0], now());
    return 1;
};

shellFunc watchdogtest = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(1, arg_cnt, args, (char*) "{ \"cmd\": \"%s\", \"arg\": \"null\", \"desc\": \"watchdog test\" },\n\r", stream)) {
        return 1;
    }

    while(1);

    return 1;
};

shellFunc help = [](int arg_cnt, char **args, Stream & stream) -> int {
    (void) arg_cnt;
    (void) args;
    char tmps[32] = {0};

    if(arg_cnt == 2) {
        strcpy(tmps, args[1]);
    }

    pshell->help(stream, tmps);
    return 1;
};

shellFunc buzzerBeep = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(2, arg_cnt, args, (char*) "{ \"cmd\": \"%s\", \"arg\": \"0 beep success, 1 beep error\", \"desc\": \"beep buzzer\" },\n\r", stream)) {
        return 1;
    }

    pioEngine->buzzerBeep(atoi(args[1]));
    stream.printf("\t{ \"cmd\": \"%s\", \"status\": true }\n\r", args[0]);

    return 1;
};


shellFunc rebootCntr = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(arg_cnt == 0xFF) {
        stream.printf("{ \"cmd\": \"%s\" },\n\r", args[0]);
        return 1;
    }

    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "executing %s", args[0]);
    stream.printf("\t{  \"cmd\": \"%s\",\"status\": true }\n\r", args[0]);
    static Task rebootTask(TASK_IMMEDIATE, TASK_ONCE, "Reboot Task", [](void) -> void {
        watchdogReboot();
    }, pts, false, NULL, NULL);
    rebootTask.enableDelayed(TASK_SECOND);
    return 1;
};

shellFunc setll = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(!checkArgument(2, arg_cnt, args, (char*) "{ \"cmd\": \"%s\",  \"arg\": \"1 - 5\", \"desc\": \"set log level, error = 1 .. trace = 5\" },\n\r", stream)) {
        return 1;
    }

    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "executing %s %s", args[0], args[1]);

    int ll = atoi(args[1]);

    if(ll < 1 ||  ll > 5) {
        stream.printf("\t{ \"cmd\": \"%s\", \"status\": false, \"error\": \"invalid argument\" }\n\r", args[0], plogger->getLogLevel());
        return 1;
    }

    plogger->setLogLevel((logLevel)atoi(args[1]));
    stream.printf("\t{ \"cmd\": \"%s\", \"status\": true }\n\r", args[0]);
    return 1;
};


shellFunc getfwrev = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(arg_cnt == 0xFF) {
        stream.printf("{ \"cmd\": \"%s\", \"desc\": \"get fw revision\" },\n\r", args[0]);
        return 1;
    }

    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "executing %s", args[0]);

    stream.printf("\t{ \"cmd\": \"%s\", \"status\": true, \"fwrev\": %s , \"cpu\": \"teensy4.1\"}\n\r", args[0], Rev);
    return 1;
};

shellFunc ll = [](int arg_cnt, char **args, Stream & stream) -> int {
    if(arg_cnt == 0xFF) {
        stream.printf("{ \"cmd\": \"%s\", \"desc\": \"get or set log level, error = 1 .. trace = 5\" },\n\r", args[0]);
        return 1;
    }

    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "executing %s", args[0]);

    if(arg_cnt == 1) {
        stream.printf("\t{ \"loglevel\": %d}\n\r", plogger->getLogLevel());
        return 1;
    }

    plogger->setLogLevel((logLevel)atoi(args[1]));
    stream.printf("\t{ \"cmd\": \"%s\", \"status\": true, \"loglevel\": %d }\n\r", args[0], plogger->getLogLevel());
    return 1;
};

shellFunc setpromt =  [](int arg_cnt, char **args, Stream & stream) -> int {
    if(arg_cnt == 0xFF) {
        stream.printf("{ \"cmd\": \"%s\" \"desc\": \"enable or disable terminal prompt\" },\n\r", args[0]);
        return 1;
    }


    pserialTerminal->setPrompt(true);
    plogger->debug(plogger->printHeader, (char*) __FILE__, __LINE__, "executing %s arg = %s", args[0], args[1]);
    stream.printf("\t{ \"cmd\": \"%s\", \"status\": true }\n\r", args[0]);
    return 1;
};

shellFunc dmesg = [](int arg_cnt, char **args, Stream & stream) -> int {
    (void)arg_cnt;

    if(arg_cnt == 0xFF) {
        stream.printf("{ \"cmd\": \"%s\", \"desc\": \"display system log buffer\" },\n\r", args[0]);
        return 1;
    }

    const char* str = nullptr;
    int count = 0;

    while(plogger->getLog(&str)) {
        int cntr = strlen(str);
        char* pstr = (char*) &str[0];
        pstr[cntr - 1] = 0;
        stream.print(str);
        stream.print('\r');
        free((char*) str);
        count += 1;
    }

    if(!count) {
        stream.print("\n\r");
    }

    return 1;
};

std::map<String, shellFunc> shellFunctions = {
    { "setiopin", setIoPin },
    { "configiopin", configiopin },
    { "date", date },
    { "dmesg", dmesg },
    { "epoch", epoch },
    { "help", help },
    { "ll", ll },
    { "printio", printio },
    { "getiopin", getIoPin },
    { "iodef", getio },
    { "reboot", rebootCntr },
    { "term", setpromt },
    { "settime", settime },
    { "uptime", uptime },
    { "watchdogtest", watchdogtest },
    { "testtestpoints", testTestpoints },
    { "testtestpoint", testTestpoint },
    { "targetpoweron", targetpoweron },
    { "targetpoweroff", targetpoweroff },
    { "buzzerbeep", buzzerBeep},
    { "batteryon", batteryon },
    { "batteryoff", batteryoff },
    { "batteryloadon", batteryloadon },
    { "batteryloadoff", batteryloadoff },
    { "getfwrev", getfwrev }
};

