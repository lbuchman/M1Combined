#pragma once

#include "delay.h"
#include "drivers/i2c.hh"
#include "drivers/i2c_conf.hh"
#include "print_messages.hh"

// TODO: use Board Conf to set the voltage values
// It just so happens that the OSD32-BRK and STM Disco boards use the same values
class EEPROM24aa02 {

    public:
        EEPROM24aa02(const I2C_Config &conf)
            : i2c{EEPROM_I2C_Address, conf} {
        }

        bool writeData(uint8_t* data, int datalength, int address) {
            for(int count = address; count < address + datalength; count++) {
                if(!i2c.write_register_byte(count, data[count - address])) {
                    return false;
                }
                udelay(10000);
            }

            return true;
        }



        bool readData(uint8_t* data, int datalength, int address) {
            for(int count = address; count < address + datalength; count++) {
                uint8_t dataChar[1];
                bool val = i2c.read_register(count, dataChar);
                               
                if (!val) return false;
                data[count - address] = dataChar[0];
            }
            return true;
        }

    private:
        I2C_Controller i2c;

        constexpr static uint32_t EEPROM_I2C_Address = 0x50;
        enum RegisterMain : uint8_t {
            ID = 0x06,
            BUCK1_CR = 0x20,
            BUCK2_CR = 0x21,
            BUCK3_CR = 0x22,
            BUCK4_CR = 0x23,
            REFDDR_CR = 0x24,
            LDO1_CR = 0x25,
            LDO2_CR = 0x26,
            LDO3_CR = 0x27,
            LDO4_CR = 0x28,
        };

        enum RegisterAlt : uint8_t {

        };

        union BUCKx_CR {
            struct {
                uint8_t enable : 1;
                uint8_t low_power : 1;
                uint8_t vout : 6;
            } bits;
            uint8_t val;
        };

        union LDOx_CR {
            struct {
                uint8_t enable : 1;
                uint8_t _resv : 1;
                uint8_t vout : 5;
                uint8_t bypass : 1;
            } bits;
            uint8_t val;
        };

        union REFDDRx_CR {
            struct {
                uint8_t enable : 1;
                uint8_t _resv : 7;
            } bits;
            uint8_t val;
        };

        constexpr static uint8_t LDO3_VOUT2DIV2 = 31; // See datasheet, Table 9
        constexpr static uint8_t BUCK2_1350mV = 30;   // See datasheet, Table 10

        constexpr static uint8_t BUCKx_1200mV = 24; // See datasheet, Table 10
};
