
----------------------------------------------------------------------------
--  i2c_master.vhd
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
USE ieee.std_logic_unsigned.all;

ENTITY i2c_master IS
  GENERIC(
    factor : INTEGER := 1000); --clock division factor

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

END i2c_master;

ARCHITECTURE logic OF i2c_master IS

TYPE machine IS(idle, start, command, addr, slave_addr_ack, read, write, slave_data_ack, master_data_ack, stop); --needed states
  SIGNAL state              : machine;                               --state machine
  SIGNAL count              : INTEGER RANGE 0 TO 7 := 7;             --tracks bit number in transaction
  SIGNAL count_clk          : INTEGER RANGE 0 TO 1023 := 0;          --clock divider counter  
  SIGNAL scl_enable := '0'  :	STD_LOGIC;	                           --i2c clock enable
  SIGNAL I2C_clk := '0'     :	STD_LOGIC;			                       --i2c clock (Standard Frequency)

BEGIN
---------------------------- CLOCK DIVISION (I2C) ---------------------
PROCESS(clk)
BEGIN
    IF rising_edge(clk) THEN
        IF (RESET = '1') THEN
            I2C_clk = '0';
            count_clk <= 0;
        
        ELSE
            IF (count_clk = ((factor/2)-1)) THEN
                I2C_clk = (not I2C_clk);
                count_clk <= 0;
            ELSE
                count_clk  <= count_clk+1;
            END IF;     
        END IF;
    END IF;
END PROCESS;
----------------------------- SCL GENERATION ---------------------------

scl <= (not  I2C_clk) WHEN scl_enable = '1' ELSE '1';           --running scl clock w.r.t clk on scl_enable flag

---------------------------- SDA GENERATION and SCL control-------------------
PROCESS(I2C_clk)
BEGIN
    -------------------------------- SDA GENERATION ----------------------------
    IF rising_edge(I2C_clk) THEN

        IF (RESET = '1') THEN
            sda <= '1';
            state <= idle;
            busy  <= '0';
        ELSE
        CASE state IS
            WHEN idle =>
                new_data_recv <= '1';   -- FLAG FOR UART tx 
                
                IF(en = '1') THEN
                    sda   <= '1';
                    state <= start;
                    busy  <= '1';
                ELSE
                    sda   <= '1';
                    state <= idle;
                END IF;
            WHEN start
                sda <= '0';             --set sda to LOW to indicate start of the transaction
                state <= addr;
                count <= 6;

            WHEN addr
                sda <= addr(count);
                IF(count = 0) THEN
                    state <= command;
                ELSE count <= count-1;
                END IF;
            WHEN command
                sda <= command;
                state <= slave_addr_ack;

            WHEN slave_addr_ack
                IF(sda = '1') THEN          --received nack
                    error = '1';            -- error flag becomes HIGH
                    state <= stop;
                ELSE                        --receive ack
                    IF(command = '1') THEN  --READ command
                        state <= read;
                    ELSE                    --WRITE command
                        state <= write;
                    END IF;
                END IF;    
                count <= 7;

            WHEN write
                sda <= i2c_data_wr(count);  --load data onto the sda std_logic_unsigned
                IF(count = 0) THEN
                    state <= slave_data_ack;
                ELSE count <= count-1;
                END IF;      

            WHEN read
                i2c_data_rd(count) <= sda;  --read data from the sda std_logic_unsigned
                IF(count = 0) THEN
                    state <= master_data_ack;
                ELSE count <= count-1;
                END IF;  

            WHEN slave_data_ack
                IF(sda = '1') THEN          -- received nack
                    error = '1';            -- error flag becomes HIGH
                    state <= stop;

                ELSE
                    state <= stop;
                END IF;  

            WHEN master_data_ack
                IF(sda = '1') THEN          -- received nack
                    error = '1';            -- error flag becomes HIGH
                    state <= stop;
                ELSE
                    state <= stop;
                END IF;  
                new_data_recv <= '1';   -- FLAG FOR UART tx 
            WHEN stop
                sda <= '1';
                state <= idle;
                error  <= '0';       
                data_recv_flag <= '0';
                busy <= '0';
        
        END CASE;
    
        END IF

    END IF;
    ------------------------------- SCL ENABLE/DISABLE -------------------------
    IF falling_edge(I2C_clk) THEN                                         --scl works at Falling edge

        IF (RESET = '1') THEN
        scl_enable <= '0';

        ELSE
            IF((state = idle) OR  (state = start) OR (state = stop)) THEN  --scl disable in IDLE, START and STOP states
                scl_enable <= '0';
            ELSE
                scl_enable <= '1';
            END IF    
        END IF

    END IF;

END PROCESS;  

END logic;
