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

#ifndef HW_CONFIG_H
#define HW_CONFIG_H
//------------------------- DEBUG ------------------------------------------

#define DISABLE_WD_NO
#define REPORT_TASK_SWITCH_NO /* delete _NO no enable */

//------------------------- END DEBUG ------------------------------------------

#define LoggerSerialDev       Serial1
#define LoggerSerialBaudRate  115200

#define WATCHDOG_LED          13

#define MAX_TERMINAL_STRING_OUT_PACKET_SIZE 128

#define FULL_CLOCK 600000000
#define IDLE_CLOCK 200000000

#define FACTORY_RESET_PIN 16
#define Rev "0.10"
#define ADChSel0 36
#define ADChSel1 37
#define ADChSel2 38


#endif
