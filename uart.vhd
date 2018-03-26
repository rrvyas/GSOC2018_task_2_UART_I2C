----------------------------------------------------------------------------
-- uart.vhd
--
--  
--
--  Copyright (C) 2018 Rahul Rakeshkumar Vyas
--
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version. 

----------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY uart IS
	GENERIC(

		baud_rate	:	INTEGER		:= 9600;		-- baud rate in bits/second
		rc_rate		:	INTEGER		:= 16;			-- oversampling rate to find center of receive bits (in samples per baud period)
		clk_freq        : 	INTEGER		:= 10000000;	--frequency of system clock in Hertz
                d_width         :	INTEGER		:= 8; 			-- data bus width
		parity		:	INTEGER		:= 1;		    --0 for no parity, 1 for parity
		parity_type	:	STD_LOGIC	:= '0');		--'0' for even, '1' for odd parity

	PORT(
		clk	:	IN		STD_LOGIC;										--system clock
		reset	:	IN		STD_LOGIC;										--ascynchronous reset
		tx_en	:	IN		STD_LOGIC;										--initiate transmission
		tx_data	:	IN		STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);           --data to transmit
		tx_busy	:	OUT	        STD_LOGIC;  									

        rx		:	IN	    STD_LOGIC;										--receive pin
		rx_busy	:	OUT	    STD_LOGIC;										--data reception in progress
		new_data:	OUT	    STD_LOGIC;										--FLAG FOR I2C TO START TRANSACTION

                rx_error:	OUT	    STD_LOGIC;						        --start, parity, or stop bit error detected
		rx_data	:	OUT	    STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);	        --data received
		tx		:	OUT	    STD_LOGIC);										--transmit pin
END uart;

ARCHITECTURE logic OF uart IS
	TYPE 	tx_state_machine IS(idle, transmit);										
	TYPE 	rx_state_machine IS(idle, receive);											
	
    SIGNAL	tx_state			:	tx_state_machine;							--transmit state machine
	SIGNAL	tx_parity			:	STD_LOGIC_VECTOR(d_width DOWNTO 0);  	    --calculation of transmit parity
	SIGNAL	tx_data_buffer		        :	STD_LOGIC_VECTOR(d_width+1 + parity DOWNTO 0) := (OTHERS => '1');	--values to be transmitted

    SIGNAL	rx_state			:	rx_state_machine;							--receive state machine
	SIGNAL	parity_error		        :	STD_LOGIC;									--receive parity error flag
	SIGNAL	rx_parity			:	STD_LOGIC_VECTOR(d_width DOWNTO 0);		    --calculation of receive parity
	SIGNAL	rx_data_buffer		        :	STD_LOGIC_VECTOR(d_width + parity DOWNTO 0) := (OTHERS => '0');  	--values received

	SIGNAL	baud_tick			:	STD_LOGIC := '0';							 --periodic ticks that occurs at the baud rate
	SIGNAL	rc_tick 			:	STD_LOGIC := '0'; 							 --periodic ticks that occurs at the oversampling rate
BEGIN

--------------------------- UART transmitter state machine ---------------------------------------------------------------
PROCESS(reset, clk)
    VARIABLE tx_data_count		:	INTEGER RANGE 0 TO parity+d_width+3 := 0;  --transmitted bits counter
BEGIN
    IF(reset = '1') THEN																--reset
		tx_data_count := 0;																--reset bit counter
	    tx <= '1';																		--set tx pin to idle value of high
		tx_busy <= '1';																	--tx system is busy
		tx_state <= idle;																--set tx state machine to idle state
	
    ELSIF(rising_edge(clk)) THEN
        CASE tx_state IS
            WHEN idle =>																	--idle state
                IF(tx_en = '1') THEN														--new transaction 
                    tx_data_buffer(d_width+1 DOWNTO 0) <=   '0' & tx_data & '1';			        --start bit data and stop bit
                    IF(parity = 1) THEN															--if parity is used
                        tx_data_buffer(parity+d_width+1) <= tx_parity(d_width);					--ADD parity bit
                    END IF;
                    tx_busy <= '1';																--tx link busy
                    tx_data_count := 0;															--init data counter
                    tx_state <= transmit;														--set state to transmit state
                ELSE																		    --no TRANSACTION initiated
                    tx_busy <= '0';																--RESET busy flag
                    tx_state <= idle;														    --remain in idle state
                END IF;
            WHEN transmit =>																--transmit state
                IF(baud_tick = '1') THEN													--beginning of bit
                    tx_data_count := tx_data_count + 1;										--increment tx bit counter
                    tx_data_buffer <= '1' & tx_data_buffer(parity+d_width+1 DOWNTO 1);	    --shift tx bits
                END IF;
                IF(tx_data_count < parity+d_width+3) THEN									--not all bits transmitted
                    tx_state <= transmit;													--remain in transmit state
                ELSE																		
                    tx_state <= idle;													    --idle state
                END IF;
        END CASE;
    tx <= tx_buffer(0);	  															        --transmit the LSB from the buffer
    
END PROCESS;

---------------------------- UART transmitter parity calculation-----------------------------------------------------------------
tx_parity(0) <= parity_type;
tx_parity_calculation: FOR i IN 0 to d_width-1
    GENERATE
		tx_parity(i+1) <= tx_parity(i) XOR tx_data(i);
	END GENERATE;

---------------------------- UART receiver state machine ---------------------------------------------------------------------
PROCESS(reset, clk)
	VARIABLE	rc_count	:	INTEGER RANGE 0 TO rc_rate-1 := 0;				    --oversampling rate ticks counter
	VARIABLE    rx_data_count	:	INTEGER RANGE 0 TO parity+d_width+2 := 0;		--data bits counter received
BEGIN

    IF(reset = '1') THEN															--RESET
		rc_count := 0;															--clear oversampling tick counter
		rx_data_count := 0;														--clear receive data bit counter
		rx_busy <= '0';															-- not busy
		rx_error <= '0';																		
		rx_data <= (OTHERS => '0');												--initialize receive buffer
		rx_state <= idle;														--start idle state

    ELSIF(rising_edge(clk) AND rc_tick = '1') THEN
        CASE rx_state IS
            WHEN idle =>														--idle state
				rx_busy <= '0';
                new_data = '0';                                                          														
																	
				IF(rx = '0') THEN 												--start bit 
					IF(rc_count < rc_count/2) THEN								--oversampling tick counter is not at start bit center
						rc_count := rc_count + 1;								--increment oversampling pulse counter
						rx_state <= idle;										--remain in idle state
					ELSE														--oversampling tick counter is at bit center
						rc_count := 0;											
						rx_data_count := 0;										--RESET data receive bit counter
						rx_busy <= '1';										    --rx busy
						rx_state <= receive;									--change to receive state
					END IF;
				ELSE															--start bit not present
					rc_count := 0;												
					rx_state <= idle;											
				END IF;

            WHEN receive =>																	 --receive state
				IF(rc_count < (rc_count/2) THEN												 --not center of bit
					rc_count := rc_count + 1;												 --increment oversampling tick counter
					rx_state <= receive;													 --remain in receive state
				ELSIF(rx_data_count < parity+d_width) THEN									 --center of the bit but not all bits received
					rc_count := 0;  														 --reset oversampling tick counter		
					rx_data_count := rx_data_count + 1;										 --increment receive data counter
					rx_data_buffer <= rx_data_buffer(parity+d_width DOWNTO 1) & rx;			 --shift and add new bit to receive data buffer
					rx_state <= receive;													 --remain in receive state
				ELSE																		 --center of stop bit and all bits read
					rx_data <= rx_data_buffer(d_width DOWNTO 1);							 --output buffered received data
    				rx_error <=  parity_error ;
					rx_busy <= '0';
                    new_data = '1';                                                          -- new data arrived														
					rx_state <= idle;														 --return to idle state
				END IF;

        END CASE;
    END IF;
END PROCESS;

---------------------------- UART receiver parity calculation-----------------------------------------------------------------
rx_parity(0) <= parity_type;
rx_parity_logic: FOR i IN 0 to d_width-1 GENERATE
	rx_parity(i+1) <= rx_parity(i) XOR rx_data_buffer(i+1);
	END GENERATE;

    ----------- check perity
WITH parity SELECT  
	parity_error <= rx_parity(d_width) XOR rx_data_buffer(parity+d_width) WHEN 1,	--using parity
	'0' WHEN OTHERS;	                                                            --not using parity

-----------------------------generate clock ticks at the baud rate and the receiver oversampling rate-----------------------
PROCESS(reset, clk)
	VARIABLE baud_rate_cnt	    :	INTEGER RANGE 0 TO clk_freq/baud_rate-1 := 0;			--counter to determine baud rate period
	VARIABLE rc_rate_cnt		:	INTEGER RANGE 0 TO clk_freq/baud_rate/rc_rate-1 := 0;	--counter to determine oversampling period
BEGIN

	IF(reset = '1') THEN											    -- reset asserted
		baud_tick <= '0';												--reset baud rate tick
		rc_tick <= '0';													--reset oversampling rate tick
		baud_rate_cnt := 0;												--reset baud  counter
		rc_rate_cnt := 0;												--reset oversampling  counter
    ELSIF(rising_edge(clk)) THEN
    -----------create baud rate ticks---------------------
		IF(baud_rate_cnt < clk_freq/baud_rate-1) THEN			            --baud rate period not reached
			baud_rate_cnt := baud_rate_cnt + 1;								--increment baud tick counter
			baud_tick <= '0';											    --no action
		ELSE																--baud period reached
			baud_rate_cnt := 0;												--reset baud period counter
			baud_tick <= '1';											    -- create impulse baud rate tick
			rc_rate_cnt := 0;												--reset oversampling period counter to avoid cumulative error
		END IF;

	-----------create oversampling rate ticks ---------------
		IF(rc_rate_cnt < clk_freq/baud_rate/rc_rate-1) THEN	                --oversampling period not reached
			rc_rate_cnt := rc_rate_cnt + 1;									--increment oversampling tick counter
			rc_rate <= '0';												    --no action	on oversampling tick generator	
		ELSE																--oversampling period reached
			rc_rate_cnt := 0;												--reset oversampling tick counter
			rc_rate <= '1';												    --create impulse oversampling rate tick
		END IF;
    
    END IF;    

END PROCESS;

END LOGIC;
