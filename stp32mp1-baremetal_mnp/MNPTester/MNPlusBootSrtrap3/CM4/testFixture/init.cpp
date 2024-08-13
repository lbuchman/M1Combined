/*
 * init.cpp
 *
 *  Created on: Aug 12, 2024
 *      Author: lbuchman
 */

#include "Arduino.h"
#include "init.h"
#include <TaskScheduler.h>

Scheduler ts;
unsigned long test = 0;

/*
 *
 */
void setup(bool engMode) {
    static Task watchdogTaskHw(TASK_SECOND, TASK_FOREVER, std::function<void()> ([](void) -> void {
        test = micros();
    }), &ts, true);
}
/*
 *
 */
void run(bool engMode) {
	 ts.execute();
}



