CMAKE_MINIMUM_REQUIRED(VERSION 3.20) 

SET(CMAKE_SYSTEM_NAME Linux)
SET(CMAKE_SYSTEM_VERSION 1)# Define the cross compiler locations


SET(CMAKE_SYSTEM_NAME Generic)
SET(CMAKE_SYSTEM_PROCESSOR arm)
SET(CMAKE_CROSSCOMPILING 1)
get_filename_component(ARDUINO_INST "~/arduino-ide" REALPATH BASE_DIR)

SET(TOOLSPATH ${ARDUINO_INST}/hardware/tools)
SET(COMPILERPATH  ${TOOLSPATH}/arm/bin)
SET(CROSS ${COMPILERPATH}/arm-none-eabi)
#SET(CMAKE_SYSROOT, ${TOOLSPATH}/arm)
#SET(TEENSY_ROOT ${TOOLSPATH}/arm)
SET (CORE_PATH ${ARDUINO_INST}/hardware/teensy/avr/cores/teensy4)


SET(MCU IMXRT1062)
SET(MCU_LOADER TEENSY40)
SET(MCU_LD ${CORE_PATH}/imxrt1062.ld)
SET(MCU_DEF ARDUINO_TEENSY40)
SET(teensyStaticLib teensy40)
SET(MATH_LIB arm_cortexM7lfsp_math)

SET(OPTIONS -DF_CPU=600000000 -DUSB_SERIAL -DLAYOUT_US_ENGLISH -D__${MCU}__ -DARDUINO=10813 -DTEENSYDUINO=154 -D${MCU_DEF})
SET(CPUOPTIONS  -mcpu=cortex-m7 -mfloat-abi=hard -mfpu=fpv5-d16 -mthumb)



SET(CPPFLAGS ${CPUOPTIONS} ${OPTIONS} -ffunction-sections -fdata-sections -DHW=40 -I${CORE_PATH})
SET(CXXFLAGS ${CPPFLAGS} -felide-constructors -fno-exceptions -fpermissive -fno-rtti -Wno-error=narrowing)

# linker options
SET(LDFLAGS -Wl,--gc-sections,--relax ${SPECS} ${CPUOPTIONS} -T${MCU_LD})

#without this cmake will not pass test compile
SET(LIBS -lteensy40 -larm_cortexM7lfsp_math)


add_compile_options(
    "$<$<COMPILE_LANGUAGE:C>:${CPPFLAGS}>"
    "$<$<COMPILE_LANGUAGE:CXX>:${CXXFLAGS}>"
)

add_link_options(${LDFLAGS} ${LIBS})

SET(OBJCOPY ${CROSS}-objcopy) 
set(SIZE  ${CROSS}-size) 
SET(CMAKE_C_COMPILER ${CROSS}-gcc)
SET(CMAKE_CXX_COMPILER ${CROSS}-gcc)
# Define the sysroot path for the RaspberryPi distribution in our tools folder 
SET(CMAKE_FIND_ROOT_PATH ${TEENSY_ROOT}/)# 
SET(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
# Search for libraries and headers in the target directories only
SET(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
SET(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
