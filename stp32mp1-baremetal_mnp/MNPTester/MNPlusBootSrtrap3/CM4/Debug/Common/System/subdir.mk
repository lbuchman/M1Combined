################################################################################
# Automatically-generated file. Do not edit!
# Toolchain: GNU Tools for STM32 (12.3.rel1)
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
/home/lbuchman/myGitHub/M1Combined/stp32mp1-baremetal_mnp/MNPTester/MNPlusBootSrtrap3/Common/System/system_stm32mp1xx.c 

C_DEPS += \
./Common/System/system_stm32mp1xx.d 

OBJS += \
./Common/System/system_stm32mp1xx.o 


# Each subdirectory must supply rules for building sources it contributes
Common/System/system_stm32mp1xx.o: /home/lbuchman/myGitHub/M1Combined/stp32mp1-baremetal_mnp/MNPTester/MNPlusBootSrtrap3/Common/System/system_stm32mp1xx.c Common/System/subdir.mk
	arm-none-eabi-gcc "$<" -mcpu=cortex-m4 -std=c17 -g3 -DDEBUG -DCORE_CM4 -DUSE_HAL_DRIVER -DSTM32MP151Fxx -c -I../Core/Inc -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/STM32MP1xx_HAL_Driver/Inc -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/STM32MP1xx_HAL_Driver/Inc/Legacy -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/CMSIS/Device/ST/STM32MP1xx/Include -I/home/lbuchman/STM32Cube/Repository/STM32Cube_FW_MP1_V1.6.0/Drivers/CMSIS/Include -O0 -ffunction-sections -fdata-sections -Wall -fstack-usage -fcyclomatic-complexity -MMD -MP -MF"$(@:%.o=%.d)" -MT"$@" --specs=nano.specs -mfpu=fpv4-sp-d16 -mfloat-abi=hard -mthumb -o "$@"

clean: clean-Common-2f-System

clean-Common-2f-System:
	-$(RM) ./Common/System/system_stm32mp1xx.cyclo ./Common/System/system_stm32mp1xx.d ./Common/System/system_stm32mp1xx.o ./Common/System/system_stm32mp1xx.su

.PHONY: clean-Common-2f-System

