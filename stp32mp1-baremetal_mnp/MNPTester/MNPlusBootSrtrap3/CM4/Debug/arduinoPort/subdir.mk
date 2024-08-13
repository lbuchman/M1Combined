################################################################################
# Automatically-generated file. Do not edit!
# Toolchain: GNU Tools for STM32 (12.3.rel1)
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
CPP_SRCS += \
../arduinoPort/argvp.cpp \
../arduinoPort/logger.cpp \
../arduinoPort/singleLEDLibrary.cpp \
../arduinoPort/singleLEDLibraryMod.cpp 

C_SRCS += \
../arduinoPort/fcvt.c \
../arduinoPort/vsprintf.c 

C_DEPS += \
./arduinoPort/fcvt.d \
./arduinoPort/vsprintf.d 

OBJS += \
./arduinoPort/argvp.o \
./arduinoPort/fcvt.o \
./arduinoPort/logger.o \
./arduinoPort/singleLEDLibrary.o \
./arduinoPort/singleLEDLibraryMod.o \
./arduinoPort/vsprintf.o 

CPP_DEPS += \
./arduinoPort/argvp.d \
./arduinoPort/logger.d \
./arduinoPort/singleLEDLibrary.d \
./arduinoPort/singleLEDLibraryMod.d 


# Each subdirectory must supply rules for building sources it contributes
arduinoPort/%.o arduinoPort/%.su arduinoPort/%.cyclo: ../arduinoPort/%.cpp arduinoPort/subdir.mk
	arm-none-eabi-g++ "$<" -mcpu=cortex-m4 -std=c++17 -g3 -DDEBUG -DCORE_CM4 -DUSE_HAL_DRIVER -DSTM32MP151Fxx -c -I../Core/Inc -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/STM32MP1xx_HAL_Driver/Inc -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/STM32MP1xx_HAL_Driver/Inc/Legacy -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/CMSIS/Device/ST/STM32MP1xx/Include -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/CMSIS/Include -O0 -ffunction-sections -fdata-sections -fno-exceptions -fno-rtti -fno-use-cxa-atexit -Wall -fstack-usage -fcyclomatic-complexity -MMD -MP -MF"$(@:%.o=%.d)" -MT"$@" --specs=nano.specs -mfpu=fpv4-sp-d16 -mfloat-abi=hard -mthumb -o "$@"
arduinoPort/%.o arduinoPort/%.su arduinoPort/%.cyclo: ../arduinoPort/%.c arduinoPort/subdir.mk
	arm-none-eabi-gcc "$<" -mcpu=cortex-m4 -std=c17 -g3 -DDEBUG -DCORE_CM4 -DUSE_HAL_DRIVER -DSTM32MP151Fxx -c -I../Core/Inc -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/STM32MP1xx_HAL_Driver/Inc -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/STM32MP1xx_HAL_Driver/Inc/Legacy -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/CMSIS/Device/ST/STM32MP1xx/Include -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/CMSIS/Include -O0 -ffunction-sections -fdata-sections -Wall -fstack-usage -fcyclomatic-complexity -MMD -MP -MF"$(@:%.o=%.d)" -MT"$@" --specs=nano.specs -mfpu=fpv4-sp-d16 -mfloat-abi=hard -mthumb -o "$@"

clean: clean-arduinoPort

clean-arduinoPort:
	-$(RM) ./arduinoPort/argvp.cyclo ./arduinoPort/argvp.d ./arduinoPort/argvp.o ./arduinoPort/argvp.su ./arduinoPort/fcvt.cyclo ./arduinoPort/fcvt.d ./arduinoPort/fcvt.o ./arduinoPort/fcvt.su ./arduinoPort/logger.cyclo ./arduinoPort/logger.d ./arduinoPort/logger.o ./arduinoPort/logger.su ./arduinoPort/singleLEDLibrary.cyclo ./arduinoPort/singleLEDLibrary.d ./arduinoPort/singleLEDLibrary.o ./arduinoPort/singleLEDLibrary.su ./arduinoPort/singleLEDLibraryMod.cyclo ./arduinoPort/singleLEDLibraryMod.d ./arduinoPort/singleLEDLibraryMod.o ./arduinoPort/singleLEDLibraryMod.su ./arduinoPort/vsprintf.cyclo ./arduinoPort/vsprintf.d ./arduinoPort/vsprintf.o ./arduinoPort/vsprintf.su

.PHONY: clean-arduinoPort

