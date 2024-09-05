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
constexpr PinConf RP1_Beeper_Pin{GPIO::H, PinNum::_11};

constexpr PinConf PCB_ID0{GPIO::H, PinNum::_2};
constexpr PinConf PCB_ID1{GPIO::D, PinNum::_11};
constexpr PinConf PCB_ID2{GPIO::J, PinNum::_4};
constexpr PinConf PCB_ID3{GPIO::G, PinNum::_4};
constexpr PinConf PCB_ID4{GPIO::I, PinNum::_11};
constexpr PinConf PCB_ID5{GPIO::G, PinNum::_2};

constexpr PinConf RP1_Wg_D1{GPIO::K, PinNum::_3};
constexpr PinConf RP1_Wg_D0{GPIO::F, PinNum::_9};
constexpr PinConf RP2_Beeper{GPIO::J, PinNum::_12};
constexpr PinConf RP1_Beeper{GPIO::H, PinNum::_11};
constexpr PinConf RP2_Wg_D1{GPIO::J, PinNum::_15};
constexpr PinConf nPoE_PSE{GPIO::K, PinNum::_0};
constexpr PinConf nPoEP_PSE{GPIO::D, PinNum::_5};
constexpr PinConf STRIKE2_KICKER_EN{GPIO::F, PinNum::_15};

constexpr PinConf RP2_Wg_D0{GPIO::C, PinNum::_2};
constexpr PinConf STRIKE1_KICKER_EN{GPIO::B, PinNum::_8};



constexpr PinConf Adc1_In1{GPIO::A, PinNum::_0};
constexpr PinConf Adc1_In2{GPIO::A, PinNum::_1};
constexpr PinConf Adc1_In3{GPIO::F, PinNum::_11};
constexpr PinConf Adc1_In4{GPIO::F, PinNum::_12};

constexpr PinConf LADR0{GPIO::F, PinNum::_0};
constexpr PinConf LADR1{GPIO::G, PinNum::_7};
constexpr PinConf LADR2{GPIO::K, PinNum::_5};
constexpr PinConf LEN_L{GPIO::G, PinNum::_10};
constexpr PinConf Ldat{GPIO::J, PinNum::_14};


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
