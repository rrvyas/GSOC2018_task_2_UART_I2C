----------------------------------------------------------------------------
--               UART-I2C BRIDGE 
--bridge.vhd
--
-- 
--
--  Copyright (C) 2018 Rahul Rakeshkumar Vyas
--
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version.
--
--  In this top level module I2C Master and UART modules are instantiated.
--  Hence the wholw module acts as bridge.
--  UART is used to write/read data to/from specific addressed I2C slave
--  My protocol works in a way that before every byte transmitted to the I2C slave
--  by the UART a 7 bit ADDRESS and a command bit (wr/rd operations) has to be 
--  transmitted to the I2C slave  called the identification byte
--  identification byte = address[0 to 7] and 8th bit is the command bit 

----------------------------------------------------------------------------


LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY Bridge IS
	
	PORT(
		clk_sys	    :	IN		STD_LOGIC;								  --system clock
		reset_sys	:	IN		STD_LOGIC;								  --ascynchronous reset
		
		sda_bridge  : INOUT STD_LOGIC;                                    --sdl
        scl_bridge  : OUT STD_LOGICl;                                     --scl

        UART_tx_bridge : OUT STD_LOGIC;                                   --UART tx
        UART_rx_bridge : IN STD_LOGIC);                                   --UART rx
       
END Bridge;

ARCHITECTURE logic OF Bridge IS
----------------I2C Component -------------------------------------
component i2c_master 
PORT(
    clk           : IN     STD_LOGIC;                    --system clock
    en            : IN     STD_LOGIC;                    --I2C Enable
    sda           : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl           : OUT    STD_LOGIC;                    --serial clock output of i2c bus
    new_data_recv : OUT    STD_LOGIC;                    -- FLAG FOR UART tx 

    reset         : IN     STD_LOGIC;                    --reset
    error         : OUT    STD_LOGIC;                    --ERROR
    addr          : IN     STD_LOGIC_VECTOR(6 DOWNTO 0;  --address of target slave
    command       : IN     STD_LOGIC;                    --command write/read
    i2c_data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
    i2c_data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
    busy          : OUT    STD_LOGIC);                   --busy flag
end component;
----------------UART Component -------------------------------------
component uart
PORT(
		clk		:	IN		STD_LOGIC;										--system clock
		reset	:	IN		STD_LOGIC;										--ascynchronous reset
		tx_en	:	IN		STD_LOGIC;										--initiate transmission
		tx_data	:	IN		STD_LOGIC_VECTOR(8 DOWNTO 0);                   --data to transmit
		tx_busy	:	OUT	    STD_LOGIC;  									

        rx		:	IN		STD_LOGIC;										--receive pin
		rx_busy	:	OUT	    STD_LOGIC;										--data reception in progress
		new_data:	OUT	    STD_LOGIC;										--FLAG FOR I2C TO START TRANSACTION

        rx_error:	OUT	    STD_LOGIC;										--start, parity, or stop bit error detected
		rx_data	:	OUT	    STD_LOGIC_VECTOR(8 DOWNTO 0);	                --data received
		tx		:	OUT	    STD_LOGIC);										--transmit pin
end component;


---------------------------------------------------------------------
	TYPE 	state_machine IS(address, data);										
    SIGNAL	state			:	state_machine;							-- state machine

    SIGNAL	new_data_recv_I2C_UART			:	STD_LOGIC := '0';		--FLAG for UART tx enable I2C -> bridge						
	SIGNAL	tx_en_UART			            :	STD_LOGIC := '0';		--FLAG for UART tx enable bridge -> UART					
    SIGNAL  new_data_recv_UART_I2C          :   STD_LOGIC := '0';       --FLAG for I2C transmitter UART -> bridge
	SIGNAL	I2C_en 			                :	STD_LOGIC := '0';	     	        --I2C enable 				
	SIGNAL	sys_addr			            :	STD_LOGIC_VECTOR(6 DOWNTO 0);		--I2C addr
	SIGNAL	sys_cmd			                :	STD_LOGIC;		         --I2C cmd

    SIGNAL	data_UART_I2C_rx			    :	STD_LOGIC_VECTOR(7 DOWNTO 0);		--data (UART -> bridge)
    SIGNAL	data_UART_I2C_tx			    :	STD_LOGIC_VECTOR(7 DOWNTO 0);		--data (bridge -> I2C)

    SIGNAL	data_I2C_UART_rx			    :	STD_LOGIC_VECTOR(7 DOWNTO 0);		--data (I2C -> bridge)
    SIGNAL	data_I2C_UART_tx			    :	STD_LOGIC_VECTOR(7 DOWNTO 0);		--data (bridge -> UART)

BEGIN

----------------------------------PORT MAPPING -------------------------
C1: i2c_master port map(sys_cmd => command, sys_addr => addr, scl_bridge => scl,
    sda_bridge => sda, reset_sys => reset, clk_sys => clk, I2C_en => en ,
    new_data_recv_I2C_UART   => new_data_recv, data_UART_I2C_tx   => i2c_data_wr,
    data_I2C_UART_rx  => i2c_data_rd) ;
C2: uart port map(new_data_recv_UART_I2C => new_data, UART_tx_bridge => tx,
    UART_rx_bridge => rx, reset_sys => reset, clk_sys => clk, tx_en_UART => tx_en,
    data_UART_I2C_rx => rx_data, data_I2C_UART_tx => tx_data);
-----------------------------------------------------------------------


process(sys_clk,reset_sys)
BEGIN
    IF (reset_sys <= '1') THEN
        state <= address;                                                   --go so state address and check for UART enabled data
        I2C_en <= '0';                                                      -- set I2C enable as 0

    IF(rising_edge(sys_clk)) THEN
    BEGIN

        CASE state IS
            WHEN address =>	
                IF(new_data_recv_UART_I2C = '1') THEN                       -- New data arrived from UART
                    sys_addr <= data_UART_I2C_rx(7 DOWNTO 1);               -- store address
                    sys_cmd  <= data_UART_I2C_rx(0);                        -- read/write I2C command
                    state <= data;
                    I2C_en <= '1';                                          -- enable I2C module to execut transaction
                ELSE
                    state <= address;
                    I2C_en <= '0';
                END IF;
                
            WHEN data =>
                IF (sys_cmd = '0')  THEN                                     -- I2C read transaction command
                    IF (new_data_recv_I2C_UART = '1') THEN                   -- do reading put data from i2c reg to uart reg when tx enable active from i2c
                        I2C_en <= '1';
                        data_I2C_UART_tx <= data_I2C_UART_rx;
                        state <= address;
                    ELSE
                        state <= data;
                        I2C_en <= '0';    
                    END IF;
                ELSE                                                         -- I2C write transaction command
                    IF (new_data_recv_UART_I2C = '1')   THEN                 --New data arrived from UART
                        I2C_en <= '1';
                        data_UART_I2C_tx <= data_UART_I2C_rx;                --data send to input reg of I2C
                        state <= address;
                    ELSE
                        state <= data;
                        I2C_en <= '0';
                    END IF;
                END IF;        

		END CASE;		
    END IF;
END PROCESS;


END LOGIC;    
