# M1-3200 ICT Test

This SW executes ICT test.
The bad-of-nails test fixture is required for this test.

## PC connections

Connect 3.3V USB/Serial cable to the 6-pin 0.1" header on the test fixture board, and find what device file is assigned to.

Connect 3.3V USB/Serial cable to the M1-3200 6-pin 0.1" header., and find what device file is assigned to.

Make sure sure M1-3200 is fitted properly into the test fixture and close the test fixture cover. 

## Execution

The program takes the following arguments:

-l <path for the output logfile> 
-d <custom testing board serial device> 
-m <m1-3200 serial device> 
-f <path to m1-3200 stm32 ICT test executable>
-s <vendor board serial number>

## Return value

The return value is Zero in the case no errors are detected, otherwise the return value is is -1.
The log file documents the ICT test progress and status of each test performed.

# Note

### Serial Number

The log file name shall be based on the PCB vendor serial number. The PCB vendor attaches the internal serial number bar-code label to the board after the PCB assembly
Since LenelS2 PCB serial number is not available as a bar-code label on PCB prior ICT and functional testing the vendor serial number is used for PCB identification for ICT test.

