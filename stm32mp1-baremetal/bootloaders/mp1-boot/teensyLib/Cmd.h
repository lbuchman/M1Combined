/*******************************************************************
    Copyright (C) 2009 FreakLabs
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.
    3. Neither the name of the the copyright holder nor the names of its contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
    ARE DISCLAIMED.  IN NO EVENT SHALL THE INSTITUTE OR CONTRIBUTORS BE LIABLE
    FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
    OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
    SUCH DAMAGE.

    Originally written by Christopher Wang aka Akiba.
    Please post support questions to the FreakLabs forum.

*******************************************************************/
/*!
    \file
    \ingroup


*/
/**************************************************************************/
#ifndef CMD_H
#define CMD_H

#include "serial.h"
#include <string.h>
#include <stdio.h>
#include <argvp.h>

#define MAX_MSG_SIZE    128
#include <stdint.h>

// command line structure
typedef struct _cmd_t {
    char *cmd;
    char *cmd_dsc;
    void (*func)(int argc, char **argv);
    struct _cmd_t *next;
} cmd_t;

class SerialTerminal {
public:
    SerialTerminal() {
        // msgSaved[0] = 0;
        // init the msg ptr
        msg_ptr = msg;
        // init the command table
        cmd_tbl_list = NULL;
        memset(msg, 0, sizeof(msg));
    };

    void setPromtOff() {
       promtOn = false; 
    }
    
    void setPromtOn() {
       promtOn = true; 
    }
    
    void begin(Stream* Serial) {
        stream = Serial;
        help();
        cmd_display();
    }

    void cmdPoll() {
        while(stream->available()) {
            cmd_handler();
        }
    }

    void cmdAdd(const char *name, const char *desc, void (*func)(int argc, char **argv)) {
        // alloc memory for command struct
        char defaultStr[] = "not specified";
        cmd_tbl = (cmd_t *)malloc(sizeof(cmd_t));

        if(!name) {
            name = defaultStr;
            print("Out of memory to add command");
        }

        char *cmd_name = (char *)malloc(strlen(name) + 1);
        // copy command name
        strcpy(cmd_name, name);

        if(!desc) {
            desc = defaultStr;
        }

        char *cmd_dsc = (char *)malloc(strlen(desc) + 1);
        // copy command dsc
        strcpy(cmd_dsc, desc);
        cmd_tbl->cmd = cmd_name;
        cmd_tbl->cmd_dsc = cmd_dsc;
        cmd_tbl->func = func;
        cmd_tbl->next = cmd_tbl_list;
        cmd_tbl_list = cmd_tbl;

    }

    void help() {
        for(cmd_t *cmd_entry = cmd_tbl; cmd_entry != NULL; cmd_entry = cmd_entry->next) {
            if(!cmd_entry->cmd || !cmd_entry->cmd_dsc || !strlen(cmd_entry->cmd_dsc)) {
                continue;
            }
            stream->printf("%10s - %s\n\r", cmd_entry->cmd, cmd_entry->cmd_dsc);
        }
    }

private:
    Stream* stream;
    uint8_t msg[MAX_MSG_SIZE];
    uint8_t *msg_ptr;
    cmd_t *cmd_tbl_list, *cmd_tbl;
#ifndef TEST_FIXTURE
    bool promtOn = true;
#else
   bool promtOn = false;  
#endif
    int lastCommandIndex = -1;

    void cmd_display() {
        if (!promtOn) return;
        char buf[] = "M1-3200:";
        stream->printf("%s", buf);
    }
    
    void cmd_parse(char *cmd) {
        cmd_t *cmd_entry;

        // parse the command line statement and break it up into space-delimited
        // strings. the array of strings will be saved in the argv array.

        constexpr size_t argc_MAX = 16;
        char* argv[argc_MAX] = { 0 };
        int argc = 0;
        parseStrToArgcArgvInsitu((char*) cmd, argc_MAX, &argc, argv);
        
        // parse the command table for valid command. used argv[0] which is the
        // actual command name typed in at the prompt
        for(cmd_entry = cmd_tbl; cmd_entry != NULL; cmd_entry = cmd_entry->next) {
            if(!strcmp(argv[0], cmd_entry->cmd)) {
                cmd_entry->func(argc, argv);
                cmd_display();
                return;
            }
        }
        stream->printf("{ \"status\": false, \"error\": \"invalid command\"}\n\r");
        cmd_display();
    }

    void cmd_handler() {
        char c = stream->readChar();
        switch(c) {
        case '\r': {
            // terminate the msg and reset the msg ptr. then send
            // it to the handler for processing.
            *msg_ptr = '\0';

            if(!strlen((char*)msg)) {
                stream->printf("\r\n");
                cmd_display();
                msg_ptr = msg;
                return;
            }
            if (promtOn) stream->printf("\r\n");
            cmd_parse((char *)msg);
            msg_ptr = msg;
            break;
        }

        case '\n':
            break;

        default:
            // normal character entered. add it to the buffer
            if (promtOn) stream->printf("%c", c);
            *msg_ptr++ = c;
            break;
        }
    }
};
#endif //CMD_H
