/**
  ******************************************************************************
  * @file    stm32mp1xx_hal_msp.c
  * @author  MCD Application Team
  * @brief   This file provides code for the MSP Initialization 
  *                      and de-Initialization codes.
  ******************************************************************************
  * @attention
  *
  * Copyright (c) 2021 STMicroelectronics.
  * All rights reserved.
  *
  * This software is licensed under terms that can be found in the LICENSE file
  * in the root directory of this software component.
  * If no LICENSE file comes with this software, it is provided AS-IS.
  *
  ******************************************************************************
  */
/* Includes ------------------------------------------------------------------*/
#include "main.h"

/* USER CODE BEGIN 0 */

/* USER CODE END 0 */
/**
  * Initializes the Global MSP.
  */
void HAL_MspInit(void)
{
  /* USER CODE BEGIN MspInit 0 */

  /* USER CODE END MspInit 0 */

  /* USER CODE BEGIN MspInit 1 */

  /* USER CODE END MspInit 1 */
}

void HAL_DAC_MspInit(DAC_HandleTypeDef* hdac)
{

  GPIO_InitTypeDef GPIO_InitStruct;
  if(hdac->Instance==DAC1)
  {
  /* USER CODE BEGIN DAC1_MspInit 0 */

  /* USER CODE END DAC1_MspInit 0 */
    /* Peripheral clock enable */
    __HAL_RCC_DAC12_CLK_ENABLE();
	
    /**DAC1 GPIO Configuration    
    PA4     ------> DAC1_OUT1 
    */
    GPIO_InitStruct.Pin = GPIO_PIN_4;
    GPIO_InitStruct.Mode = GPIO_MODE_ANALOG;
    PERIPH_LOCK(GPIOA);
    HAL_GPIO_Init(GPIOA, &GPIO_InitStruct);
    PERIPH_UNLOCK(GPIOA);

  /* USER CODE BEGIN DAC1_MspInit 1 */

  /* USER CODE END DAC1_MspInit 1 */
  }

}

void HAL_DAC_MspDeInit(DAC_HandleTypeDef* hdac)
{

  if(hdac->Instance==DAC1)
  {
  /* USER CODE BEGIN DAC1_MspDeInit 0 */

  /* USER CODE END DAC1_MspDeInit 0 */
    /* Peripheral clock disable */
    __HAL_RCC_DAC12_CLK_DISABLE();

    /**DAC1 GPIO Configuration    
    PA4     ------> DAC1_OUT1 
    */
    PERIPH_LOCK(GPIOA);
    HAL_GPIO_DeInit(GPIOA, GPIO_PIN_4);
    PERIPH_UNLOCK(GPIOA);

  /* USER CODE BEGIN DAC1_MspDeInit 1 */

  /* USER CODE END DAC1_MspDeInit 1 */
  }

}

/* USER CODE BEGIN 1 */

/**
  * @brief ADC MSP initialization
  *        This function configures the hardware resources used in this example:
  *          - Enable clock of ADC peripheral
  *          - Configure the GPIO associated to the peripheral channels
  *          - Configure the DMA associated to the peripheral
  *          - Configure the NVIC associated to the peripheral interruptions
  * @param hadc: ADC handle pointer
  * @retval None
  */
void HAL_ADC_MspInit(ADC_HandleTypeDef *hadc)
{
  GPIO_InitTypeDef          GPIO_InitStruct;
  static DMA_HandleTypeDef  DmaHandle;
  RCC_PeriphCLKInitTypeDef  PeriphClkInit;
  
  /*##-1- Enable peripherals and GPIO Clocks #################################*/
  /* Enable clock of GPIO associated to the peripheral channels */
  ADCx_CHANNELa_GPIO_CLK_ENABLE();
  /* Enable clock of ADCx peripheral (core clock) */
  ADCx_CLK_ENABLE();

  /* Configure ADCx clock prescaler */
  /* Caution: On STM32MP15, ADC clock frequency max is 36MHz (refer to device   */
  /*          datasheet).                                                     */
  /*          Therefore, ADC clock prescaler must be configured in function   */
  /*          of ADC clock source frequency to remain below this maximum      */
  /*          frequency.                                                      */
  if(IS_ENGINEERING_BOOT_MODE())
  {
    PeriphClkInit.PeriphClockSelection = RCC_PERIPHCLK_ADC;
    PeriphClkInit.AdcClockSelection = RCC_ADCCLKSOURCE_PER;
    HAL_RCCEx_PeriphCLKConfig(&PeriphClkInit);
  }
  /* Enable clock of DMA associated to the peripheral */
  ADCx_DMA_CLK_ENABLE();
  ADCx_DMAMUX_CLK_ENABLE();

  /*##-2- Configure peripheral GPIO ##########################################*/
  /* Configure GPIO pin of the selected ADC channel */
  GPIO_InitStruct.Pin = ADCx_CHANNELa_PIN;
  GPIO_InitStruct.Mode = GPIO_MODE_ANALOG;
  GPIO_InitStruct.Pull = GPIO_NOPULL;
  PERIPH_LOCK(ADCx_CHANNELa_GPIO_PORT);
  HAL_GPIO_Init(ADCx_CHANNELa_GPIO_PORT, &GPIO_InitStruct);
  PERIPH_UNLOCK(ADCx_CHANNELa_GPIO_PORT);

  /*##-3- Configure the DMA ##################################################*/
  /* Configure DMA parameters */
  DmaHandle.Instance = ADCx_DMA;

  DmaHandle.Init.Request             = DMA_REQUEST_ADC2;
  DmaHandle.Init.Direction           = DMA_PERIPH_TO_MEMORY;
  DmaHandle.Init.PeriphInc           = DMA_PINC_DISABLE;
  DmaHandle.Init.MemInc              = DMA_MINC_ENABLE;
  DmaHandle.Init.PeriphDataAlignment = DMA_PDATAALIGN_HALFWORD;   /* Transfer from ADC by half-word to match with ADC configuration: ADC resolution 10 or 12 bits */
  DmaHandle.Init.MemDataAlignment    = DMA_MDATAALIGN_HALFWORD;   /* Transfer to memory by half-word to match with buffer variable type: half-word */
  DmaHandle.Init.Mode                = DMA_CIRCULAR;              /* DMA in circular mode to match with ADC configuration: DMA continuous requests */
  DmaHandle.Init.Priority            = DMA_PRIORITY_HIGH;
  
  /* Deinitialize  & Initialize the DMA for new transfer */
  HAL_DMA_DeInit(&DmaHandle);
  HAL_DMA_Init(&DmaHandle);

  /* Associate the initialized DMA handle to the ADC handle */
  __HAL_LINKDMA(hadc, DMA_Handle, DmaHandle);
  
  /*##-4- Configure the NVIC #################################################*/
  /* NVIC configuration for ADC interrupt */
  /* Priority: high-priority */
  HAL_NVIC_SetPriority(ADCx_IRQn, DEFAULT_IRQ_PRIO, 0);
  HAL_NVIC_EnableIRQ(ADCx_IRQn);
  
  /* NVIC configuration for DMA interrupt (transfer completion or error) */
  /* Priority: high-priority */
  HAL_NVIC_SetPriority(ADCx_DMA_IRQn, (DEFAULT_IRQ_PRIO + 1U), 0);
  HAL_NVIC_EnableIRQ(ADCx_DMA_IRQn);
}

/**
  * @brief ADC MSP de-initialization
  *        This function frees the hardware resources used in this example:
  *          - Disable clock of ADC peripheral
  *          - Revert GPIO associated to the peripheral channels to their default state
  *          - Revert DMA associated to the peripheral to its default state
  *          - Revert NVIC associated to the peripheral interruptions to its default state
  * @param hadc: ADC handle pointer
  * @retval None
  */
void HAL_ADC_MspDeInit(ADC_HandleTypeDef *hadc)
{

  /*##-1- Reset peripherals ##################################################*/
  ADCx_FORCE_RESET();
  ADCx_RELEASE_RESET();

  /*##-2- Disable peripherals and GPIO Clocks ################################*/
  /* De-initialize GPIO pin of the selected ADC channel */
  PERIPH_LOCK(ADCx_CHANNELa_GPIO_PORT);
  HAL_GPIO_DeInit(ADCx_CHANNELa_GPIO_PORT, ADCx_CHANNELa_PIN);
  PERIPH_UNLOCK(ADCx_CHANNELa_GPIO_PORT);

  /*##-3- Disable the DMA ####################################################*/
  /* De-Initialize the DMA associated to the peripheral */
  if(hadc->DMA_Handle != NULL)
  {
    HAL_DMA_DeInit(hadc->DMA_Handle);
  }

  /*##-4- Disable the NVIC ###################################################*/
  /* Disable the NVIC configuration for ADC interrupt */
  HAL_NVIC_DisableIRQ(ADCx_IRQn);
  
  /* Disable the NVIC configuration for DMA interrupt */
  HAL_NVIC_DisableIRQ(ADCx_DMA_IRQn);
  
}


/* USER CODE END 1 */

/**
  * @}
  */

/**
  * @}
  */

