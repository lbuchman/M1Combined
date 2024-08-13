################################################################################
# Automatically-generated file. Do not edit!
# Toolchain: GNU Tools for STM32 (12.3.rel1)
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
CPP_SRCS += \
../testFixture/init.cpp 

OBJS += \
./testFixture/init.o 

CPP_DEPS += \
./testFixture/init.d 


# Each subdirectory must supply rules for building sources it contributes
testFixture/%.o testFixture/%.su testFixture/%.cyclo: ../testFixture/%.cpp testFixture/subdir.mk
	arm-none-eabi-g++ "$<" -mcpu=cortex-m4 -std=c++17 -g3 -DDEBUG -DCORE_CM4 -DUSE_HAL_DRIVER -DSTM32MP151Fxx -c -I../Core/Inc -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/STM32MP1xx_HAL_Driver/Inc -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/STM32MP1xx_HAL_Driver/Inc/Legacy -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/CMSIS/Device/ST/STM32MP1xx/Include -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/CMSIS/Include -O0 -ffunction-sections -fdata-sections -fno-exceptions -fno-rtti -fno-use-cxa-atexit -Wall -fstack-usage -fcyclomatic-complexity -MMD -MP -MF"$(@:%.o=%.d)" -MT"$@" --specs=nano.specs -mfpu=fpv4-sp-d16 -mfloat-abi=hard -mthumb -o "$@"

clean: clean-testFixture

clean-testFixture:
	-$(RM) ./testFixture/init.cyclo ./testFixture/init.d ./testFixture/init.o ./testFixture/init.su

.PHONY: clean-testFixture

