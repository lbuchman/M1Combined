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

#ifndef __SERIALL_H__
#define __SERIALL_H__

#ifndef ARDUINO
#include <cstdint>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include "stm32disco_conf.hh"
#include "drivers/uart.hh"

namespace Board = STM32MP1Disco;
int vdprintf(int fd, const char *fmt, va_list args);

class Stream {
public:
    Stream() {
        
    };
    int readChar() {
        return Uart<Board::ConsoleUART>::readChar();
    };
    int available() {
        return Uart<Board::ConsoleUART>::available();
    }
    virtual int printf(const char *format, ...) = 0;
};

class Serial: public Stream {
public:
    Serial() {};
    virtual int printf(const char *format, ...) override {
        char buff[256];
        va_list args;
        va_start(args, format);
        int ret = vsprintf(buff, format, args);
        va_end(args);
        for (int  count = 0; count < ret; count++) {
            writeChar(buff[count]);
        }
        return ret;
    };

    void writeChar(char c) {
        Uart<Board::ConsoleUART>::putchar(c);
    }


    void end() {};

private:

};


#endif



#endif //__SERIALL_H__
