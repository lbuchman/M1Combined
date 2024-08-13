#ifndef ARDUINO_HH
#define ARDUINO_HH

#define delay HAL_Delay

#ifdef __cplusplus
 extern "C" {
#endif

unsigned long micros(void);
unsigned long millis(void);


#ifdef __cplusplus
}
#endif

#endif
