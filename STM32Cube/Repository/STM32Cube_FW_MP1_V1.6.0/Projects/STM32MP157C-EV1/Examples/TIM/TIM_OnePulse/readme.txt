/**
  @page TIM_OnePulse TIM One Pulse example

  @verbatim
  ******************** (C) COPYRIGHT 2019 STMicroelectronics *******************
  * @file    TIM/TIM_OnePulse/readme.txt
  * @author  MCD Application Team
  * @brief   Description of the TIM One Pulse example.
  ******************************************************************************
  *
  * Copyright (c) 2021 STMicroelectronics.
  * All rights reserved.
  *
  * This software is licensed under terms that can be found in the LICENSE file
  * in the root directory of this software component.
  * If no LICENSE file comes with this software, it is provided AS-IS.
  *
  ******************************************************************************
  @endverbatim

@par Example Description

This example shows how to use the TIMER peripheral to generate a single pulse when
 a rising edge of an external signal is received on the TIMER Input pin.

Clock setup for TIM8
================================

  TIM8CLK = SystemCoreClock = 209 MHz.

  Prescaler = (TIM8CLK /TIM8 counter clock) - 1

  The prescaler value is computed in order to have TIM8 counter clock
  set at 1000000 Hz.

  The Autoreload value is 65535 (TIM8->ARR), so the maximum frequency value to
  trigger the TIM8 input is 1000000/65535 [Hz].

Configuration of TIM8 in One Pulse Mode
===================================================

  - The external signal is connected to TIM8_CH2 pin (PI6),
    and a rising edge on this input is used to trigger the Timer.
  - The One Pulse signal is output on TIM8_CH1 (PI5).

  The delay value is fixed to:
   - Delay =  CCR1/TIM8 counter clock
           = 16383 / 1000000 [sec]

  The pulse value is fixed to :
   - Pulse value = (TIM_Period - TIM_Pulse)/TIM8 counter clock
                 = (65535 - 16383) / 1000000 [sec]

  The one pulse waveform can be displayed using an oscilloscope and it looks
  like this.

                               ____
                               |   |
  CH2 _________________________|   |__________________________________________

                                             ___________________________
                                            |                           |
  CH1 ______________________________________|                           |_____
                               <---Delay----><------Pulse--------------->

@note The delay and pulse values mentioned above are theoretical (obtained when the system clock frequency is exactly 209 MHz).
      They might be slightly different depending on system clock frequency precision.

@par Directory contents

  - TIM/TIM_OnePulse/Inc/stm32mp1xx_hal_conf.h    HAL configuration file
  - TIM/TIM_OnePulse/Inc/stm32mp1xx_it.h          Interrupt handlers header file
  - TIM/TIM_OnePulse/Inc/main.h                  Header for main.c module
  - TIM/TIM_OnePulse/Src/stm32mp1xx_it.c          Interrupt handlers
  - TIM/TIM_OnePulse/Src/main.c                  Main program
  - TIM/TIM_OnePulse/Src/stm32mp1xx_hal_msp.c     HAL MSP file
  - TIM/TIM_OnePulse/Src/system_stm32mp1xx.c      STM32MP1xx system source file


@par Hardware and Software environment

   - This example runs on STM32MP157CAAx devices.
   - In this example, the clock is set to 209 MHz.

  - This example has been tested with STM32MP157C-EV1 board and can be
    easily tailored to any other supported device and development board.

  - STM32MP157C-EV1 Set-up
   - Use MB1262C motherboard and make sure solder bridges SB66 and SB73 are replaced by 0 ohm resitor.
   - Connect the external signal to the TIM8_CH2 pin (PI6) (pin 38 in CN21 connector)
   - Connect the TIM8_CH1 pin(PI5) (pin 12 in CN21 connector) to an oscilloscope to monitor the waveform.


@par How to use it ?

In order to make the program work, you must do the following :
 - Open your preferred toolchain
 - Rebuild all files: Project->Rebuild all
 - Load project image: Project->Download and Debug
 - Run program: Debug->Go(F5)

 * <h3><center>&copy; COPYRIGHT STMicroelectronics</center></h3>
 */
