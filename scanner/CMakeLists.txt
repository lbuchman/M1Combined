#Copyright (c) Microsoft. All rights reserved.
#Lic`sed under the MIT license. See LICENSE file in the project root for full license information.

#cmake -DRPI2=1 -DCMAKE_TOOLCHAIN_FILE=../arm-linux-gnueabihf.cmake ..

cmake_minimum_required(VERSION 3.16)
set(PROJECT_NAME scanner)

include(ExternalProject)

project (scanner)

get_filename_component(buildDirRelFilePath "${myFile}"
                       REALPATH BASE_DIR "${CMAKE_BINARY_DIR}")
set(CMAKE_VERBOSE_MAKEFILE ON)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/inc)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_INSTALL_PREFIX, /usr)

SET(MY_DEBUG_OPTIONS -O0 -Wall  -I${CORE_PATH} -D_TASK_STD_FUNCTION -D_TASK_WDT_IDS -D_TASK_MICRO_RES)
SET(MY_RELEASE_OPTIONS  -s -Os -Wall -I${CORE_PATH} -D_TASK_STD_FUNCTION -D_TASK_WDT_IDS -D_TASK_MICRO_RES)


include_directories(./src) 
file(GLOB_RECURSE sources ./src/*.c)


add_executable(${PROJECT_NAME}  ${sources})

target_link_libraries(${PROJECT_NAME}
     m
     pthread
)


install(TARGETS ${PROJECT_NAME} DESTINATION bin) 

 
add_custom_target(format
    COMMAND
        ../format.sh
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

