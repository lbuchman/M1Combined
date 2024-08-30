/*
 * init.cpp
 *
 *  Created on: Aug 12, 2024
 *      Author: lbuchman
 */

#include "Arduino.h"
#include "init.h"
#include "main.h"
#include <TaskScheduler.h>

Scheduler ts;
unsigned long test = 0;

/*
 *
 */
void setup(bool engMode) {
    static Task watchdogTaskHw(TASK_SECOND, TASK_FOREVER, std::function<void()> ([](void) -> void {
    	static int ledStatus = 0;
    	ledStatus ^= 1;
        test = micros();
        if (ledStatus) {
            HAL_GPIO_WritePin(LedCPU_GPIO_Port, LedCPU_Pin, GPIO_PIN_SET);
        }
        else {
            HAL_GPIO_WritePin(LedCPU_GPIO_Port, LedCPU_Pin, GPIO_PIN_RESET);
        }
    }), &ts, true);
}
/*
 *
 */
void run(bool engMode) {
	 ts.execute();
}



