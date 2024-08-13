/*
 * Arduino.cpp
 *
 *  Created on: Aug 12, 2024
 *      Author: lbuchman
 */

#include "Arduino.h"
#include "stm32mp1xx_hal.h"

static uint32_t getCurrentMicros(void)
{
  /* Ensure COUNTFLAG is reset by reading SysTick control and status register */
  uint32_t m = HAL_GetTick();
  const uint32_t tms = SysTick->LOAD + 1;
  __IO uint32_t u = tms - SysTick->VAL;
  return (m * 1000 + (u * 1000) / tms);
}

/*
 *
 */
extern "C" unsigned long micros(void){
    return getCurrentMicros();
}

/*
 *
 */
extern "C" unsigned long millis(void) {
	return HAL_GetTick();
}

