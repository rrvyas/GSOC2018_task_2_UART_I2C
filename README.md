# GSOC2018_task_2_UART_I2C
Qualification task 2 for GSOC project - FPGA realtime focus peaking

repository contain 3 vhdl files : 
  (1) I2C master
  (2) UART
  (3) Top level UART-I2C bridge
  
commands and data are communicated to this bridge over UART
The custom protocol is designed in such a way that every byte of data transmited over UART has to have address of the slave I2C as well as the read/write command.

The system is driven by system clock and resets over external asynchronous reset.
