#Copyright (c) Microsoft. All rights reserved.
#Lic`sed under the MIT license. See LICENSE file in the project root for full license information.

#cmake -DRPI2=1 -DCMAKE_TOOLCHAIN_FILE=../arm-linux-gnueabihf.cmake ..

cmake_minimum_required(VERSION 3.20)
set(PROJECT_NAME M1Teensy41)

include(ExternalProject)

project (M1Teensy41)

get_filename_component(buildDirRelFilePath "${myFile}"
                       REALPATH BASE_DIR "${CMAKE_BINARY_DIR}")
set(CMAKE_VERBOSE_MAKEFILE ON)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/inc)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

message(STATUS -----------------------------Cross Compiling --------------------)
#file(GLOB_RECURSE target_source_cpp_files ${CORE_PATH}/*.cpp)
#file(GLOB_RECURSE target_source_c_files ${CORE_PATH}/hardware/teensy/avr/cores/${TEENSY_TYPE}/*.c)
#file(GLOB_RECURSE target_source_h_files ${CORE_PATH}/hardware/teensy/avr/cores/${TEENSY_TYPE}/*.h)

SET(MY_DEBUG_OPTIONS -O0 -Wall  -I${CORE_PATH} -D_TASK_STD_FUNCTION -D_TASK_WDT_IDS -D_TASK_MICRO_RES)
SET(MY_RELEASE_OPTIONS  -s -Os -Wall -I${CORE_PATH} -D_TASK_STD_FUNCTION -D_TASK_WDT_IDS -D_TASK_MICRO_RES)
message(STATUS "Running cmake for Tensy ${MCU_DEF}")

# System Libraries from Arduino install folder


file(GLOB_RECURSE singleLEDLibrary ${ARDUINO_INST}/hardware/teensy/avr/libraries/singleLEDLibrary/*.cpp)
file(GLOB_RECURSE eeprom_lib ${ARDUINO_INST}/hardware/teensy/avr/libraries/EEPROM/*.cpp)
include_directories(${ARDUINO_INST}/hardware/teensy/avr/libraries/EEPROM/)
include_directories(${ARDUINO_INST}/hardware/teensy/avr/libraries/WDT_T4/)
include_directories(${ARDUINO_INST}/hardware/teensy/avr/libraries/TaskScheduler/src/)
include_directories(${ARDUINO_INST}/hardware/teensy/avr/libraries/CRCx)
include_directories(${ARDUINO_INST}/hardware/teensy/avr/libraries/ArduinoJson/src/) 
include_directories(${ARDUINO_INST}/hardware/teensy/avr/libraries/Ring-Buffer/) 
file(GLOB_RECURSE teensy4_Queue ${ARDUINO_INST}/hardware/teensy/avr/libraries/Queue/src/*.cpp)
include_directories(${ARDUINO_INST}/hardware/teensy/avr/libraries/Queue/src)
file(GLOB_RECURSE teensy4_AD ${ARDUINO_INST}/hardware/teensy/avr/libraries/ADC/*.cpp)
include_directories(${ARDUINO_INST}/hardware/teensy/avr/libraries/ADC)
include_directories(${ARDUINO_INST}/hardware/teensy/avr/libraries/singleLEDLibrary)
# project files BSP related
include_directories(./system) 
file(GLOB_RECURSE system ./system/*.cpp)

file(GLOB_RECURSE teensyIoport ./libs/teensyIoport/*.cpp)
include_directories(./libs/teensyIoport)
file(GLOB_RECURSE teensyIoport ./libs/ioEngine/*.cpp)
include_directories(./libs/ioEngine)
#file(GLOB_RECURSE TCPStream ./libs/TCPStream/*.cpp)
#include_directories(./libs/TCPStream)
#file(GLOB_RECURSE WebServer ./libs/RestServer/*.cpp)
#include_directories(./libs/RestServer)
#file(GLOB_RECURSE RestServer ./libs/WebServer/*.cpp)
#include_directories(./libs/WebServer)
#file(GLOB_RECURSE arduino-restserver ./libs//arduino-restserver/*.cpp)
#include_directories(./libs/arduino-restserver)
file(GLOB_RECURSE time_lib ./libs/Time/*.cpp)
include_directories(./libs/Time/)

add_executable(${PROJECT_NAME} ${eeprom_lib} ${time_lib}  ${singleLEDLibrary} ${board_support} ${wiegand_support} ${teensyIoport} ${system} ${teensy4_AD} ${teensy4_Queue})

IF(NOT ${CMAKE_CROSSCOMPILING} MATCHES "1")
target_link_libraries(${PROJECT_NAME}
     m
     pthread
)
ELSE()
target_link_libraries(${PROJECT_NAME}
     ${MATH_LIB}
      stdc++ 
     ${teensyStaticLib}
     m
)
ENDIF(NOT ${CMAKE_CROSSCOMPILING} MATCHES "1")

install(CODE "MESSAGE(\"installing target\")") 
 
 
add_custom_target(format
    COMMAND
        ../format.sh
    VERBATIM
)

add_custom_target(size
    COMMAND
        ${SIZE} ${PROJECT_NAME}
    VERBATIM
)

add_custom_command(TARGET ${PROJECT_NAME}
    COMMAND
        ${OBJCOPY} -O ihex -R .eeprom -R .fuse -R .lock -R .signature ${PROJECT_NAME} ${PROJECT_NAME}.hex
        
    VERBATIM
)

add_custom_target(flash
    COMMAND
        ${TOOLSPATH}/teensy_loader_cli  -mmcu=${MCU_LOADER} -w -s -v  ${PROJECT_NAME}.hex
        #${TOOLSPATH}/teensy_post_compile -board=${MCU_LOADER} -file=${PROJECT_NAME} -path=${CMAKE_BINARY_DIR} -tools=${TOOLSPATH} -reboot
       ## ${PROJECT_SOURCE_DIR}/program.sh ${MCU_LOADER} ${PROJECT_NAME}.hex ${PROJECT_SOURCE_DIR}
    VERBATIM
)
 
set_target_properties(${PROJECT_NAME}
    PROPERTIES
        CXX_STANDARD 14
        CXX_STANDARD_REQUIRED YES
        CXX_EXTENSIONS YES
)

target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:DEBUG>:${MY_DEBUG_OPTIONS}>")
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:RELEASE>:${MY_RELEASE_OPTIONS}>")


# echo "1" > /sys/class/gpio/gpio204/value
# echo "20" > /sys/class/gpio/export
# echo "out" > /sys/class/gpio/gpio20/direction
# sudo sshfs -o allow_other,default_permissions ubuntu@192.168.1.102:/home/ubuntu  pi
