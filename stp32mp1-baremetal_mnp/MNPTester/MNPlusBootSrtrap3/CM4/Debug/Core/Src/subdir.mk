################################################################################
# Automatically-generated file. Do not edit!
# Toolchain: GNU Tools for STM32 (12.3.rel1)
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../Core/Src/main.c \
../Core/Src/stm32mp1xx_hal_msp.c \
../Core/Src/stm32mp1xx_it.c \
../Core/Src/syscalls.c \
../Core/Src/sysmem.c 

C_DEPS += \
./Core/Src/main.d \
./Core/Src/stm32mp1xx_hal_msp.d \
./Core/Src/stm32mp1xx_it.d \
./Core/Src/syscalls.d \
./Core/Src/sysmem.d 

OBJS += \
./Core/Src/main.o \
./Core/Src/stm32mp1xx_hal_msp.o \
./Core/Src/stm32mp1xx_it.o \
./Core/Src/syscalls.o \
./Core/Src/sysmem.o 


# Each subdirectory must supply rules for building sources it contributes
Core/Src/%.o Core/Src/%.su Core/Src/%.cyclo: ../Core/Src/%.c Core/Src/subdir.mk
	arm-none-eabi-gcc "$<" -mcpu=cortex-m4 -std=c17 -g3 -DDEBUG -DCORE_CM4 -DUSE_HAL_DRIVER -DSTM32MP151Fxx -c -I../Core/Inc -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/STM32MP1xx_HAL_Driver/Inc -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/STM32MP1xx_HAL_Driver/Inc/Legacy -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/CMSIS/Device/ST/STM32MP1xx/Include -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/CMSIS/Include -O0 -ffunction-sections -fdata-sections -Wall -fstack-usage -fcyclomatic-complexity -MMD -MP -MF"$(@:%.o=%.d)" -MT"$@" --specs=nano.specs -mfpu=fpv4-sp-d16 -mfloat-abi=hard -mthumb -o "$@"

clean: clean-Core-2f-Src

clean-Core-2f-Src:
	-$(RM) ./Core/Src/main.cyclo ./Core/Src/main.d ./Core/Src/main.o ./Core/Src/main.su ./Core/Src/stm32mp1xx_hal_msp.cyclo ./Core/Src/stm32mp1xx_hal_msp.d ./Core/Src/stm32mp1xx_hal_msp.o ./Core/Src/stm32mp1xx_hal_msp.su ./Core/Src/stm32mp1xx_it.cyclo ./Core/Src/stm32mp1xx_it.d ./Core/Src/stm32mp1xx_it.o ./Core/Src/stm32mp1xx_it.su ./Core/Src/syscalls.cyclo ./Core/Src/syscalls.d ./Core/Src/syscalls.o ./Core/Src/syscalls.su ./Core/Src/sysmem.cyclo ./Core/Src/sysmem.d ./Core/Src/sysmem.o ./Core/Src/sysmem.su

.PHONY: clean-Core-2f-Src
