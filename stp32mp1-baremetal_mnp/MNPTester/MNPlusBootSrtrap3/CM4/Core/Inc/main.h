/* USER CODE BEGIN Header */
/**
  ******************************************************************************
  * @file           : main.h
  * @brief          : Header for main.c file.
  *                   This file contains the common defines of the application.
  ******************************************************************************
  * @attention
  *
  * Copyright (c) 2024 STMicroelectronics.
  * All rights reserved.
  *
  * This software is licensed under terms that can be found in the LICENSE file
  * in the root directory of this software component.
  * If no LICENSE file comes with this software, it is provided AS-IS.
  *
  ******************************************************************************
  */
/* USER CODE END Header */

/* Define to prevent recursive inclusion -------------------------------------*/
#ifndef __MAIN_H
#define __MAIN_H

#ifdef __cplusplus
extern "C" {
#endif

/* Includes ------------------------------------------------------------------*/
#include "stm32mp1xx_hal.h"

/* Private includes ----------------------------------------------------------*/
/* USER CODE BEGIN Includes */

/* USER CODE END Includes */

/* Exported types ------------------------------------------------------------*/
/* USER CODE BEGIN ET */

/* USER CODE END ET */

/* Exported constants --------------------------------------------------------*/
/* USER CODE BEGIN EC */

/* USER CODE END EC */

/* Exported macro ------------------------------------------------------------*/
/* USER CODE BEGIN EM */

/* USER CODE END EM */

/* Exported functions prototypes ---------------------------------------------*/
void Error_Handler(void);

/* USER CODE BEGIN EFP */

/* USER CODE END EFP */

/* Private defines -----------------------------------------------------------*/
#define nPoEP_PSE_Pin GPIO_PIN_5
#define nPoEP_PSE_GPIO_Port GPIOD
#define RP1_Beeper_Pin GPIO_PIN_11
#define RP1_Beeper_GPIO_Port GPIOH
#define RP1_Wg_D1_Pin GPIO_PIN_3
#define RP1_Wg_D1_GPIO_Port GPIOK
#define Ldat_Pin GPIO_PIN_14
#define Ldat_GPIO_Port GPIOJ
#define RP2_Beeper_Pin GPIO_PIN_12
#define RP2_Beeper_GPIO_Port GPIOJ
#define RD2_OSDP_rx_Pin GPIO_PIN_0
#define RD2_OSDP_rx_GPIO_Port GPIOE
#define LAddr2_Pin GPIO_PIN_5
#define LAddr2_GPIO_Port GPIOK
#define RP2_Wg_D1_Pin GPIO_PIN_15
#define RP2_Wg_D1_GPIO_Port GPIOJ
#define LED_POEP_Pin GPIO_PIN_4
#define LED_POEP_GPIO_Port GPIOD
#define RD2_OSDP_Tx_Pin GPIO_PIN_1
#define RD2_OSDP_Tx_GPIO_Port GPIOE
#define nPoE_PSE_Pin GPIO_PIN_0
#define nPoE_PSE_GPIO_Port GPIOK
#define RP2_OSDP_Te_Pin GPIO_PIN_12
#define RP2_OSDP_Te_GPIO_Port GPIOE
#define LED_POE_Pin GPIO_PIN_6
#define LED_POE_GPIO_Port GPIOC
#define ConsoleA7_Tx_Pin GPIO_PIN_8
#define ConsoleA7_Tx_GPIO_Port GPIOD
#define ConsoleA7_Rx_Pin GPIO_PIN_9
#define ConsoleA7_Rx_GPIO_Port GPIOD
#define LedCPU_Pin GPIO_PIN_3
#define LedCPU_GPIO_Port GPIOC
#define Adc1_pin6_Pin GPIO_PIN_12
#define Adc1_pin6_GPIO_Port GPIOF
#define LDDR0_Pin GPIO_PIN_10
#define LDDR0_GPIO_Port GPIOF
#define STRIKE2_KICKER_EN_Pin GPIO_PIN_15
#define STRIKE2_KICKER_EN_GPIO_Port GPIOF
#define Adc1_Pin2_Pin GPIO_PIN_11
#define Adc1_Pin2_GPIO_Port GPIOF
#define RP1_OSDP_Tx_Pin GPIO_PIN_6
#define RP1_OSDP_Tx_GPIO_Port GPIOB
#define RP2_Wg_D0_Pin GPIO_PIN_2
#define RP2_Wg_D0_GPIO_Port GPIOC
#define LAddr1_Pin GPIO_PIN_7
#define LAddr1_GPIO_Port GPIOG
#define RP1_OSDP_Te_Pin GPIO_PIN_12
#define RP1_OSDP_Te_GPIO_Port GPIOB
#define RP1_OSDP_Rx_Pin GPIO_PIN_5
#define RP1_OSDP_Rx_GPIO_Port GPIOB
#define LEN_L_Pin GPIO_PIN_10
#define LEN_L_GPIO_Port GPIOG
#define M4_RUN_L_Pin GPIO_PIN_6
#define M4_RUN_L_GPIO_Port GPIOF
#define STRIKE1_KICKER_EN_Pin GPIO_PIN_8
#define STRIKE1_KICKER_EN_GPIO_Port GPIOB

/* USER CODE BEGIN Private defines */

/* USER CODE END Private defines */

#ifdef __cplusplus
}
#endif

#endif /* __MAIN_H */
