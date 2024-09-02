#pragma once
#include "drivers/i2c_conf.hh"
#include "drivers/leds.hh"
#include <singleLEDLibrary.h>

#define M1_3200

namespace STM32MP1Disco
{
//   PA13 USER2 user button (B4)
//    PA14 USER1 user button (B3)

#ifndef M1_3200    
using LED_RUN = sllib<GPIO::H, PinNum::_7, PinPolarity::Normal>;
//using LD7 = cpuRun;


using LED_DBG_LED1 = sllib<GPIO::D, PinNum::_11, PinPolarity::Normal>;
//using LD8 = BlueLED;

// Note: Green and Red LEDs share a pin with User1Button and User2Button.
using LED_DBG_LED2 = sllib<GPIO::A, PinNum::_14, PinPolarity::Normal>;
//using LD5 = GreenLED;

using LED_DBG_LED3 = sllib<GPIO::A, PinNum::_13, PinPolarity::Normal>;
//using LD6 = RedLED;

using LED_DBG_LED4 = sllib<GPIO::F, PinNum::_3, PinPolarity::Normal>;
//using LD6 = RedLED;


using LED_CPU = sllib<GPIO::F, PinNum::_6, PinPolarity::Normal>;
//using LD6 = RedLED;

//using RedLED2 = cpuRun; // For compatibility with OSD32BRK board
//using GreenLED2 = BlueLED; // For compatibility with OSD32BRK board

constexpr uint32_t ConsoleUART = UART4_BASE;
constexpr PinConf UartRX{GPIO::B, PinNum::_2, PinAF::AF_8};
constexpr PinConf UartTX{GPIO::G, PinNum::_11, PinAF::AF_6};
#else
constexpr PinConf nPoEP_PSE{GPIO::D, PinNum::_5};
constexpr PinConf RP1_Beeper_Pin{GPIO::H, PinNum::_11};

constexpr PinConf PCB_ID0{GPIO::H, PinNum::_2};
constexpr PinConf PCB_ID1{GPIO::D, PinNum::_11};
constexpr PinConf PCB_ID2{GPIO::J, PinNum::_4};
constexpr PinConf PCB_ID3{GPIO::G, PinNum::_4};
constexpr PinConf PCB_ID4{GPIO::I, PinNum::_11};
constexpr PinConf PCB_ID5{GPIO::G, PinNum::_2};

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

#define RD2_OSDP_Tx_Pin GPIO_PIN_1
#define RD2_OSDP_Tx_GPIO_Port GPIOE
#define nPoE_PSE_Pin GPIO_PIN_0
#define nPoE_PSE_GPIO_Port GPIOK
#define RP2_OSDP_Te_Pin GPIO_PIN_12
#define RP2_OSDP_Te_GPIO_Port GPIOE
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

#define STRIKE1_KICKER_EN_Pin GPIO_PIN_8
#define STRIKE1_KICKER_EN_GPIO_Port GPIOB

/*
constexpr uint32_t ConsoleUART = UART7_BASE;
constexpr PinConf UartRX{GPIO::E, PinNum::_7, PinAF::AF_7};
constexpr PinConf UartTX{GPIO::E, PinNum::_8, PinAF::AF_7};

constexpr uint32_t Rx422UART = USART3_BASE;
constexpr PinConf Rx422UartRX{GPIO::D, PinNum::_9, PinAF::AF_7};
constexpr PinConf Rx422UartTX{GPIO::D, PinNum::_8, PinAF::AF_7};
*/


constexpr uint32_t ConsoleUART = USART3_BASE;
constexpr PinConf UartRX{GPIO::D, PinNum::_9, PinAF::AF_7};
constexpr PinConf UartTX{GPIO::D, PinNum::_8, PinAF::AF_7};


// M1-3200
constexpr uint32_t Rx422UART = UART7_BASE;
constexpr PinConf Rx422UartRX{GPIO::E, PinNum::_7, PinAF::AF_7};
constexpr PinConf Rx422UartTX{GPIO::E, PinNum::_8, PinAF::AF_7};


constexpr uint32_t Rx485UART = UART8_BASE;
constexpr PinConf Rx485UartRX{GPIO::E, PinNum::_0, PinAF::AF_8};
constexpr PinConf Rx485UartTX{GPIO::E, PinNum::_1, PinAF::AF_8};
constexpr PinConf Rx485UartDE{GPIO::E, PinNum::_14, PinAF::AF_8};


// MNP
constexpr uint32_t MNP_Rx485UART = UART4_BASE;
constexpr PinConf MNP_Rx485UartDE{GPIO::A, PinNum::_15, PinAF::AF_8};
constexpr PinConf MNP_Rx485UartRX{GPIO::D, PinNum::_0, PinAF::AF_8};
constexpr PinConf MNP_Rx485UartTX{GPIO::D, PinNum::_1, PinAF::AF_8};

constexpr uint32_t MNP_Rd1UART = UART5_BASE;
constexpr PinConf MNP_Rd1UARTDE{GPIO::C, PinNum::_8, PinAF::AF_8};
constexpr PinConf MNP_Rd1UARTRX{GPIO::B, PinNum::_5, PinAF::AF_12};
constexpr PinConf MNP_Rd1UARTTX{GPIO::B, PinNum::_6, PinAF::AF_12};
constexpr PinConf MNP_Rd1Te{GPIO::B, PinNum::_12};

constexpr uint32_t MNP_Rd2UART = UART8_BASE;
constexpr PinConf MNP_Rd2UARTDE{GPIO::E, PinNum::_14, PinAF::AF_8};
constexpr PinConf MNP_Rd2UARTRX{GPIO::E, PinNum::_0, PinAF::AF_8};
constexpr PinConf MNP_Rd2UARTTX{GPIO::E, PinNum::_1, PinAF::AF_8};
constexpr PinConf MNP_Rd2Te{GPIO::E, PinNum::_12};


using LED_CPU = sllib<GPIO::C, PinNum::_3, PinPolarity::Normal>;
using LED_RUN = sllib<GPIO::E, PinNum::_4, PinPolarity::Normal>;
using LED_DBG_LED1 = sllib<GPIO::A, PinNum::_13, PinPolarity::Normal>;
using LED_DBG_LED2 = sllib<GPIO::E, PinNum::_2, PinPolarity::Normal>;
using LED_DBG_LED3 = sllib<GPIO::B, PinNum::_7, PinPolarity::Normal>;
using LED_DBG_LED4 = sllib<GPIO::G, PinNum::_3, PinPolarity::Normal>;
using M4_RUN = sllib<GPIO::F, PinNum::_6, PinPolarity::Normal>;
using LED_POEP = sllib<GPIO::D, PinNum::_4, PinPolarity::Normal>;
using LED_POE = sllib<GPIO::C, PinNum::_6, PinPolarity::Normal>;


using OrangeLED = LED_RUN;
#endif


constexpr PinConf EEPROM_Wp{GPIO::B, PinNum::_0};

namespace PMIC
{
constexpr bool HasSTPMIC = true;
constexpr I2C_Config I2C_config{
	.periph = I2C_Periph::I2C4_,
	.sda_pin = {GPIO::D, PinNum::_13, PinAF::AF_4},
	.scl_pin = {GPIO::D, PinNum::_12, PinAF::AF_4},
};
} // namespace PMIC


constexpr uint32_t HSE_Clock_Hz = 24000000;
constexpr uint32_t MPU_MHz = 650;
} // namespace STM32MP1Disco
