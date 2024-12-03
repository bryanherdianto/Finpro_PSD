-- Copyright 2018 Jonas Fuhrmann. All rights reserved.
--
-- This project is dual licensed under GNU General Public License version 3
-- and a commercial license available on request.
---------------------------------------------------------------------------
-- For non commercial use only:
-- This file is part of tinyTPU.
-- 
-- tinyTPU is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- tinyTPU is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with tinyTPU. If not, see <http://www.gnu.org/licenses/>.

-- Compiled with VHDL-2008

USE WORK.TPU_pack.ALL;
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;

ENTITY TB_MMU IS
END ENTITY TB_MMU;

ARCHITECTURE BEH OF TB_MMU IS
    COMPONENT DUT IS
        GENERIC (
            MATRIX_WIDTH : NATURAL := 14
        );
        PORT (
            CLK, RESET : IN STD_LOGIC;
            ENABLE : IN STD_LOGIC;

            WEIGHT_DATA : IN BYTE_ARRAY_TYPE(0 TO MATRIX_WIDTH - 1);
            WEIGHT_SIGNED : IN STD_LOGIC;
            SYSTOLIC_DATA : IN BYTE_ARRAY_TYPE(0 TO MATRIX_WIDTH - 1);
            SYSTOLIC_SIGNED : IN STD_LOGIC;

            ACTIVATE_WEIGHT : IN STD_LOGIC; -- Activates the loaded weights sequentially
            LOAD_WEIGHT : IN STD_LOGIC; -- Preloads one column of weights with WEIGHT_DATA
            WEIGHT_ADDRESS : IN BYTE_TYPE; -- Addresses up to 256 columns of preweights

            RESULT_DATA : OUT WORD_ARRAY_TYPE(0 TO MATRIX_WIDTH - 1)
        );
    END COMPONENT DUT;
    FOR ALL : DUT USE ENTITY WORK.MMU(BEH);

    CONSTANT MATRIX_WIDTH : NATURAL := 4;

    SIGNAL CLK, RESET : STD_LOGIC;
    SIGNAL ENABLE : STD_LOGIC;

    SIGNAL WEIGHT_DATA : BYTE_ARRAY_TYPE(0 TO MATRIX_WIDTH - 1);
    SIGNAL WEIGHT_SIGNED : STD_LOGIC;
    SIGNAL SYSTOLIC_DATA : BYTE_ARRAY_TYPE(0 TO MATRIX_WIDTH - 1) := (OTHERS => (OTHERS => '0'));
    SIGNAL SYSTOLIC_SIGNED : STD_LOGIC;

    SIGNAL ACTIVATE_WEIGHT : STD_LOGIC;
    SIGNAL LOAD_WEIGHT : STD_LOGIC;
    SIGNAL WEIGHT_ADDRESS : BYTE_TYPE;

    SIGNAL RESULT_DATA : WORD_ARRAY_TYPE(0 TO MATRIX_WIDTH - 1);

    -- for clock gen
    CONSTANT clock_period : TIME := 10 ns;
    SIGNAL stop_the_clock : BOOLEAN := false;

    SIGNAL START : BOOLEAN;
    SIGNAL EVALUATE : BOOLEAN;

    -- Unsigned
    -- Tested input data
    CONSTANT INPUT_MATRIX : INTEGER_ARRAY_2D_TYPE :=
    (
    (40, 76, 19, 192),
    (3, 84, 12, 8),
    (54, 18, 255, 120),
    (30, 84, 122, 2)
    );

    -- Tested weight data
    CONSTANT WEIGHT_MATRIX : INTEGER_ARRAY_2D_TYPE :=
    (
    (13, 89, 178, 9),
    (84, 184, 245, 18),
    (255, 73, 14, 3),
    (98, 212, 78, 29)
    );

    -- Result of matrix multiply
    CONSTANT RESULT_MATRIX : INTEGER_ARRAY_2D_TYPE :=
    (
    (30565, 59635, 40982, 7353),
    (10939, 18295, 21906, 1807),
    (78999, 52173, 26952, 5055),
    (38752, 27456, 27784, 2206)
    );
    -- Signed
    -- Tested input data
    CONSTANT INPUT_MATRIX_SIGNED : INTEGER_ARRAY_2D_TYPE :=
    (
    (74, 91, 64, 10),
    (5, 28, 26, 9),
    (56, 9, 72, 127),
    (94, 26, 92, 8)
    );

    -- Tested weight data
    CONSTANT WEIGHT_MATRIX_SIGNED : INTEGER_ARRAY_2D_TYPE :=
    (
    (-13, 89, 92, 9),
    (-84, 104, 86, 18),
    (-128, 73, 14, 3),
    (-98, 127, 78, 29)
    );

    -- Result of matrix multiply
    CONSTANT RESULT_MATRIX_SIGNED : INTEGER_ARRAY_2D_TYPE :=
    (
    (-17778, 21992, 16310, 2786),
    (-6627, 6398, 3934, 888),
    (-23146, 27305, 16840, 4565),
    (-15966, 18802, 12796, 1822)
    );

    SIGNAL CURRENT_INPUT : INTEGER_ARRAY_2D_TYPE(0 TO MATRIX_WIDTH - 1, 0 TO MATRIX_WIDTH - 1);
    SIGNAL CURRENT_RESULT : INTEGER_ARRAY_2D_TYPE(0 TO MATRIX_WIDTH - 1, 0 TO MATRIX_WIDTH - 1);
    SIGNAL CURRENT_SIGN : STD_LOGIC;

    SIGNAL QUIT_CLOCK0 : BOOLEAN;
    SIGNAL QUIT_CLOCK1 : BOOLEAN;
BEGIN
    DUT_i : DUT
    GENERIC MAP(
        MATRIX_WIDTH => MATRIX_WIDTH
    )
    PORT MAP(
        CLK => CLK,
        RESET => RESET,
        ENABLE => ENABLE,
        WEIGHT_DATA => WEIGHT_DATA,
        WEIGHT_SIGNED => WEIGHT_SIGNED,
        SYSTOLIC_DATA => SYSTOLIC_DATA,
        SYSTOLIC_SIGNED => SYSTOLIC_SIGNED,
        ACTIVATE_WEIGHT => ACTIVATE_WEIGHT,
        LOAD_WEIGHT => LOAD_WEIGHT,
        WEIGHT_ADDRESS => WEIGHT_ADDRESS,
        RESULT_DATA => RESULT_DATA
    );

    STIMULUS :
    PROCESS IS
        PROCEDURE LOAD_WEIGHTS(
            MATRIX : IN INTEGER_ARRAY_2D_TYPE;
            SIGNED_NOT_UNSIGNED : IN STD_LOGIC
        ) IS
        BEGIN
            START <= false;
            RESET <= '0';
            ENABLE <= '0';
            WEIGHT_DATA <= (OTHERS => (OTHERS => '0'));
            ACTIVATE_WEIGHT <= '0';
            LOAD_WEIGHT <= '0';
            WEIGHT_ADDRESS <= (OTHERS => '0');
            WEIGHT_SIGNED <= '0';
            WAIT UNTIL '1' = CLK AND CLK'event;
            -- RESET
            RESET <= '1';
            WAIT UNTIL '1' = CLK AND CLk'event;
            RESET <= '0';
            WEIGHT_SIGNED <= SIGNED_NOT_UNSIGNED;
            -- Load weight 0
            WEIGHT_ADDRESS <= STD_LOGIC_VECTOR(to_unsigned(0, BYTE_WIDTH));
            FOR i IN 0 TO MATRIX_WIDTH - 1 LOOP
                WEIGHT_DATA(i) <= STD_LOGIC_VECTOR(to_signed(MATRIX(0, i), BYTE_WIDTH));
            END LOOP;
            LOAD_WEIGHT <= '1';
            WAIT UNTIL '1' = CLK AND CLK'event;
            -- Load weight 1
            WEIGHT_ADDRESS <= STD_LOGIC_VECTOR(to_unsigned(1, BYTE_WIDTH));
            FOR i IN 0 TO MATRIX_WIDTH - 1 LOOP
                WEIGHT_DATA(i) <= STD_LOGIC_VECTOR(to_signed(MATRIX(1, i), BYTE_WIDTH));
            END LOOP;
            LOAD_WEIGHT <= '1';
            WAIT UNTIL '1' = CLK AND CLK'event;
            -- Load weight 2
            WEIGHT_ADDRESS <= STD_LOGIC_VECTOR(to_unsigned(2, BYTE_WIDTH));
            FOR i IN 0 TO MATRIX_WIDTH - 1 LOOP
                WEIGHT_DATA(i) <= STD_LOGIC_VECTOR(to_signed(MATRIX(2, i), BYTE_WIDTH));
            END LOOP;
            LOAD_WEIGHT <= '1';
            WAIT UNTIL '1' = CLK AND CLK'event;
            -- Load weight 3
            WEIGHT_ADDRESS <= STD_LOGIC_VECTOR(to_unsigned(3, BYTE_WIDTH));
            FOR i IN 0 TO MATRIX_WIDTH - 1 LOOP
                WEIGHT_DATA(i) <= STD_LOGIC_VECTOR(to_signed(MATRIX(3, i), BYTE_WIDTH));
            END LOOP;
            LOAD_WEIGHT <= '1';
            WAIT UNTIL '1' = CLK AND CLK'event;
            --
            LOAD_WEIGHT <= '0';
            WEIGHT_SIGNED <= '0';
            ACTIVATE_WEIGHT <= '1';
            ENABLE <= '1';
            --
            START <= true;
            WAIT UNTIL '1' = CLK AND CLK'event;
            START <= false;
            ACTIVATE_WEIGHT <= '0';
            FOR i IN 0 TO 3 * MATRIX_WIDTH - 1 LOOP
                WAIT UNTIL '1' = CLK AND CLK'event;
            END LOOP;
        END PROCEDURE LOAD_WEIGHTS;
    BEGIN
        QUIT_CLOCK0 <= false;
        CURRENT_SIGN <= '0';
        CURRENT_INPUT <= INPUT_MATRIX;
        CURRENT_RESULT <= RESULT_MATRIX;
        LOAD_WEIGHTS(WEIGHT_MATRIX, '0');
        CURRENT_SIGN <= '1';
        CURRENT_INPUT <= INPUT_MATRIX_SIGNED;
        CURRENT_RESULT <= RESULT_MATRIX_SIGNED;
        LOAD_WEIGHTS(WEIGHT_MATRIX_SIGNED, '1');
        QUIT_CLOCK0 <= true;
        WAIT;
    END PROCESS STIMULUS;

    PROCESS_INPUT0 :
    PROCESS IS
    BEGIN
        EVALUATE <= false;
        WAIT UNTIL START = true;
        FOR i IN 0 TO MATRIX_WIDTH - 1 LOOP
            SYSTOLIC_DATA(0) <= STD_LOGIC_VECTOR(to_signed(CURRENT_INPUT(i, 0), BYTE_WIDTH));
            WAIT UNTIL '1' = CLK AND CLK'event;
        END LOOP;
        SYSTOLIC_DATA(0) <= (OTHERS => '0');
        EVALUATE <= true;
        WAIT UNTIL '1' = CLK AND CLK'event;
        EVALUATE <= false;
    END PROCESS;

    PROCESS_INPUT1 :
    PROCESS IS
    BEGIN
        WAIT UNTIL START = true;
        WAIT UNTIL '1' = CLK AND CLK'event;
        FOR i IN 0 TO MATRIX_WIDTH - 1 LOOP
            SYSTOLIC_DATA(1) <= STD_LOGIC_VECTOR(to_signed(CURRENT_INPUT(i, 1), BYTE_WIDTH));
            WAIT UNTIL '1' = CLK AND CLK'event;
        END LOOP;
        SYSTOLIC_DATA(1) <= (OTHERS => '0');
    END PROCESS;

    PROCESS_INPUT2 :
    PROCESS IS
    BEGIN
        WAIT UNTIL START = true;
        WAIT UNTIL '1' = CLK AND CLK'event;
        WAIT UNTIL '1' = CLK AND CLK'event;
        FOR i IN 0 TO MATRIX_WIDTH - 1 LOOP
            SYSTOLIC_DATA(2) <= STD_LOGIC_VECTOR(to_signed(CURRENT_INPUT(i, 2), BYTE_WIDTH));
            WAIT UNTIL '1' = CLK AND CLK'event;
        END LOOP;
        SYSTOLIC_DATA(2) <= (OTHERS => '0');
    END PROCESS;

    PROCESS_INPUT3 :
    PROCESS IS
    BEGIN
        SYSTOLIC_SIGNED <= '0';
        WAIT UNTIL START = true;
        SYSTOLIC_SIGNED <= CURRENT_SIGN;
        WAIT UNTIL '1' = CLK AND CLK'event;
        WAIT UNTIL '1' = CLK AND CLK'event;
        WAIT UNTIL '1' = CLK AND CLK'event;
        FOR i IN 0 TO MATRIX_WIDTH - 1 LOOP
            SYSTOLIC_DATA(3) <= STD_LOGIC_VECTOR(to_signed(CURRENT_INPUT(i, 3), BYTE_WIDTH));
            WAIT UNTIL '1' = CLK AND CLK'event;
            SYSTOLIC_SIGNED <= '0';
        END LOOP;
        SYSTOLIC_DATA(3) <= (OTHERS => '0');
    END PROCESS;

    EVALUATE_RESULT :
    PROCESS IS
    BEGIN
        QUIT_CLOCK1 <= false;
        WAIT UNTIL EVALUATE = true;
        WAIT UNTIL '1' = CLK AND CLK'event;
        WAIT UNTIL '1' = CLK AND CLK'event;
        FOR i IN 0 TO MATRIX_WIDTH - 1 LOOP
            WAIT UNTIL '1' = CLK AND CLK'event;

            IF RESULT_DATA(0) /= STD_LOGIC_VECTOR(to_signed(CURRENT_RESULT(i, 0), 4 * BYTE_WIDTH)) THEN
                REPORT "Test failed! Result should be " & to_hstring(to_signed(CURRENT_RESULT(0, i), 4 * BYTE_WIDTH)) & " but was " & to_hstring(RESULT_DATA(0)) & "." SEVERITY ERROR;
                QUIT_CLOCK1 <= true;
                WAIT;
            END IF;

            IF RESULT_DATA(1) /= STD_LOGIC_VECTOR(to_signed(CURRENT_RESULT(i, 1), 4 * BYTE_WIDTH)) THEN
                REPORT "Test failed! Result should be " & to_hstring(to_signed(CURRENT_RESULT(1, i), 4 * BYTE_WIDTH)) & " but was " & to_hstring(RESULT_DATA(1)) & "." SEVERITY ERROR;
                QUIT_CLOCK1 <= true;
                WAIT;
            END IF;

            IF RESULT_DATA(2) /= STD_LOGIC_VECTOR(to_signed(CURRENT_RESULT(i, 2), 4 * BYTE_WIDTH)) THEN
                REPORT "Test failed! Result should be " & to_hstring(to_signed(CURRENT_RESULT(2, i), 4 * BYTE_WIDTH)) & " but was " & to_hstring(RESULT_DATA(2)) & "." SEVERITY ERROR;
                QUIT_CLOCK1 <= true;
                WAIT;
            END IF;

            IF RESULT_DATA(3) /= STD_LOGIC_VECTOR(to_signed(CURRENT_RESULT(i, 3), 4 * BYTE_WIDTH)) THEN
                REPORT "Test failed! Result should be " & to_hstring(to_signed(CURRENT_RESULT(3, i), 4 * BYTE_WIDTH)) & " but was " & to_hstring(RESULT_DATA(3)) & "." SEVERITY ERROR;
                QUIT_CLOCK1 <= true;
                WAIT;
            END IF;
        END LOOP;
        REPORT "Test was successful!" SEVERITY NOTE;
    END PROCESS EVALUATE_RESULT;
    stop_the_clock <= QUIT_CLOCK0 OR QUIT_CLOCK1;

    CLOCK_GEN :
    PROCESS
    BEGIN
        WHILE NOT stop_the_clock LOOP
            CLK <= '0', '1' AFTER clock_period / 2;
            WAIT FOR clock_period;
        END LOOP;
        WAIT;
    END PROCESS CLOCK_GEN;

END ARCHITECTURE BEH;