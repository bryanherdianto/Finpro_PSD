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

USE WORK.TPU_pack.ALL;
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;

ENTITY MMU IS
    GENERIC (
        MATRIX_WIDTH : NATURAL := 14
    );
    PORT (
        CLK, RESET : IN STD_LOGIC;
        ENABLE : IN STD_LOGIC;

        WEIGHT_DATA : IN BYTE_ARRAY_TYPE(0 TO MATRIX_WIDTH - 1); --!< Input for the weights, connected to the MACC's weight input.
        WEIGHT_SIGNED : IN STD_LOGIC; --!< Determines if the weight input is signed or unsigned.
        SYSTOLIC_DATA : IN BYTE_ARRAY_TYPE(0 TO MATRIX_WIDTH - 1); --!< The diagonally feeded input data.
        SYSTOLIC_SIGNED : IN STD_LOGIC; --!< Determines if the systolic input is signed or unsigned.

        ACTIVATE_WEIGHT : IN STD_LOGIC; --!< Activates the loaded weights sequentially.
        LOAD_WEIGHT : IN STD_LOGIC; --!< Preloads one column of weights with WEIGHT_DATA.
        WEIGHT_ADDRESS : IN BYTE_TYPE; --!< Addresses up to 256 columns of preweights.

        RESULT_DATA : OUT WORD_ARRAY_TYPE(0 TO MATRIX_WIDTH - 1) --!< The result of the matrix multiply.
    );
END ENTITY MMU;

--! @brief Architecture of the matrix multiply unit.
ARCHITECTURE BEH OF MMU IS
    COMPONENT MACC IS
        GENERIC (
            -- The width of the last sum input
            LAST_SUM_WIDTH : NATURAL := 0;
            -- The width of the output register
            PARTIAL_SUM_WIDTH : NATURAL := 2 * EXTENDED_BYTE_WIDTH
        );
        PORT (
            CLK, RESET : IN STD_LOGIC;
            ENABLE : IN STD_LOGIC;
            -- Weights - current and preload
            WEIGHT_INPUT : IN EXTENDED_BYTE_TYPE;
            PRELOAD_WEIGHT : IN STD_LOGIC;
            LOAD_WEIGHT : IN STD_LOGIC;
            -- Input
            INPUT : IN EXTENDED_BYTE_TYPE;
            LAST_SUM : IN STD_LOGIC_VECTOR(LAST_SUM_WIDTH - 1 DOWNTO 0);
            -- Output
            PARTIAL_SUM : OUT STD_LOGIC_VECTOR(PARTIAL_SUM_WIDTH - 1 DOWNTO 0)
        );
    END COMPONENT MACC;
    FOR ALL : MACC USE ENTITY WORK.MACC(BEH);

    SIGNAL INTERIM_RESULT : WORD_ARRAY_2D_TYPE(0 TO MATRIX_WIDTH - 1, 0 TO MATRIX_WIDTH - 1) := (OTHERS => (OTHERS => (OTHERS => '0')));

    -- For address conversion
    SIGNAL LOAD_WEIGHT_MAP : STD_LOGIC_VECTOR(0 TO MATRIX_WIDTH - 1);

    SIGNAL ACTIVATE_CONTROL_cs : STD_LOGIC_VECTOR(0 TO MATRIX_WIDTH - 1 - 1) := (OTHERS => '0');
    SIGNAL ACTIVATE_CONTROL_ns : STD_LOGIC_VECTOR(0 TO MATRIX_WIDTH - 1 - 1);

    SIGNAL ACTIVATE_MAP : STD_LOGIC_VECTOR(0 TO MATRIX_WIDTH - 1);

    -- For sign extension
    SIGNAL EXTENDED_WEIGHT_DATA : EXTENDED_BYTE_ARRAY(0 TO MATRIX_WIDTH - 1);
    SIGNAL EXTENDED_SYSTOLIC_DATA : EXTENDED_BYTE_ARRAY(0 TO MATRIX_WIDTH - 1);

    -- For result sign extension
    SIGNAL SIGN_CONTROL_cs : STD_LOGIC_VECTOR(0 TO 2 + MATRIX_WIDTH - 1) := (OTHERS => '0'); -- Register delay of 2 caused by MACC
    SIGNAL SIGN_CONTROL_ns : STD_LOGIC_VECTOR(0 TO 2 + MATRIX_WIDTH - 1);
BEGIN

    -- Linear shift register
    ACTIVATE_CONTROL_ns(1 TO MATRIX_WIDTH - 1 - 1) <= ACTIVATE_CONTROL_cs(0 TO MATRIX_WIDTH - 2 - 1);
    ACTIVATE_CONTROL_ns(0) <= ACTIVATE_WEIGHT;

    SIGN_CONTROL_ns(1 TO 2 + MATRIX_WIDTH - 1) <= SIGN_CONTROL_cs(0 TO 2 + MATRIX_WIDTH - 2);
    SIGN_CONTROL_ns(0) <= SYSTOLIC_SIGNED;

    ACTIVATE_MAP <= ACTIVATE_CONTROL_ns(0) & ACTIVATE_CONTROL_cs;

    LOAD : -- Address conversion
    PROCESS (LOAD_WEIGHT, WEIGHT_ADDRESS) IS
        VARIABLE LOAD_WEIGHT_v : STD_LOGIC;
        VARIABLE WEIGHT_ADDRESS_v : BYTE_TYPE;

        VARIABLE LOAD_WEIGHT_MAP_v : STD_LOGIC_VECTOR(0 TO MATRIX_WIDTH - 1);
    BEGIN
        LOAD_WEIGHT_v := LOAD_WEIGHT;
        WEIGHT_ADDRESS_v := WEIGHT_ADDRESS;

        LOAD_WEIGHT_MAP_v := (OTHERS => '0');
        IF LOAD_WEIGHT_v = '1' THEN
            LOAD_WEIGHT_MAP_v(to_integer(unsigned(WEIGHT_ADDRESS_v))) := '1';
        END IF;

        LOAD_WEIGHT_MAP <= LOAD_WEIGHT_MAP_v;
    END PROCESS LOAD;

    SIGN_EXTEND :
    PROCESS (WEIGHT_DATA, SYSTOLIC_DATA, WEIGHT_SIGNED, SIGN_CONTROL_ns) IS
    BEGIN
        FOR i IN 0 TO MATRIX_WIDTH - 1 LOOP
            IF WEIGHT_SIGNED = '1' THEN
                EXTENDED_WEIGHT_DATA(i) <= WEIGHT_DATA(i)(BYTE_WIDTH - 1) & WEIGHT_DATA(i);
            ELSE
                EXTENDED_WEIGHT_DATA(i) <= '0' & WEIGHT_DATA(i);
            END IF;

            IF SIGN_CONTROL_ns(i) = '1' THEN
                EXTENDED_SYSTOLIC_DATA(i) <= SYSTOLIC_DATA(i)(BYTE_WIDTH - 1) & SYSTOLIC_DATA(i);
            ELSE
                EXTENDED_SYSTOLIC_DATA(i) <= '0' & SYSTOLIC_DATA(i);
            END IF;
        END LOOP;
    END PROCESS SIGN_EXTEND;

    MACC_GEN :
    FOR i IN 0 TO MATRIX_WIDTH - 1 GENERATE
        MACC_2D :
        FOR j IN 0 TO MATRIX_WIDTH - 1 GENERATE
            UPPER_LEFT_ELEMENT :
            IF i = 0 AND j = 0 GENERATE
                MACC_i0 : MACC
                GENERIC MAP(
                    LAST_SUM_WIDTH => 0,
                    PARTIAL_SUM_WIDTH => 2 * EXTENDED_BYTE_WIDTH
                )
                PORT MAP(
                    CLK => CLK,
                    RESET => RESET,
                    ENABLE => ENABLE,
                    WEIGHT_INPUT => EXTENDED_WEIGHT_DATA(j),
                    PRELOAD_WEIGHT => LOAD_WEIGHT_MAP(i),
                    LOAD_WEIGHT => ACTIVATE_MAP(i),
                    INPUT => EXTENDED_SYSTOLIC_DATA(i),
                    LAST_SUM => (OTHERS => '0'),
                    PARTIAL_SUM => INTERIM_RESULT(i, j)(2 * EXTENDED_BYTE_WIDTH - 1 DOWNTO 0)
                );
            END GENERATE UPPER_LEFT_ELEMENT;

            FIRST_COLUMN :
            IF i = 0 AND j > 0 GENERATE
                MACC_i1 : MACC
                GENERIC MAP(
                    LAST_SUM_WIDTH => 0,
                    PARTIAL_SUM_WIDTH => 2 * EXTENDED_BYTE_WIDTH
                )
                PORT MAP(
                    CLK => CLK,
                    RESET => RESET,
                    ENABLE => ENABLE,
                    WEIGHT_INPUT => EXTENDED_WEIGHT_DATA(j),
                    PRELOAD_WEIGHT => LOAD_WEIGHT_MAP(i),
                    LOAD_WEIGHT => ACTIVATE_MAP(i),
                    INPUT => EXTENDED_SYSTOLIC_DATA(i),
                    LAST_SUM => (OTHERS => '0'),
                    PARTIAL_SUM => INTERIM_RESULT(i, j)(2 * EXTENDED_BYTE_WIDTH - 1 DOWNTO 0)
                );
            END GENERATE FIRST_COLUMN;

            LEFT_FULL_ELEMENTS :
            IF i > 0 AND i <= 2 * (BYTE_WIDTH - 1) AND j = 0 GENERATE
                MACC_i2 : MACC
                GENERIC MAP(
                    LAST_SUM_WIDTH => 2 * EXTENDED_BYTE_WIDTH + i - 1,
                    PARTIAL_SUM_WIDTH => 2 * EXTENDED_BYTE_WIDTH + i
                )
                PORT MAP(
                    CLK => CLK,
                    RESET => RESET,
                    ENABLE => ENABLE,
                    WEIGHT_INPUT => EXTENDED_WEIGHT_DATA(j),
                    PRELOAD_WEIGHT => LOAD_WEIGHT_MAP(i),
                    LOAD_WEIGHT => ACTIVATE_MAP(i),
                    INPUT => EXTENDED_SYSTOLIC_DATA(i),
                    LAST_SUM => INTERIM_RESULT(i - 1, j)(2 * EXTENDED_BYTE_WIDTH + i - 2 DOWNTO 0),
                    PARTIAL_SUM => INTERIM_RESULT(i, j)(2 * EXTENDED_BYTE_WIDTH + i - 1 DOWNTO 0)
                );
            END GENERATE LEFT_FULL_ELEMENTS;

            FULL_COLUMNS :
            IF i > 0 AND i <= 2 * (BYTE_WIDTH - 1) AND j > 0 GENERATE
                MACC_i3 : MACC
                GENERIC MAP(
                    LAST_SUM_WIDTH => 2 * EXTENDED_BYTE_WIDTH + i - 1,
                    PARTIAL_SUM_WIDTH => 2 * EXTENDED_BYTE_WIDTH + i
                )
                PORT MAP(
                    CLK => CLK,
                    RESET => RESET,
                    ENABLE => ENABLE,
                    WEIGHT_INPUT => EXTENDED_WEIGHT_DATA(j),
                    PRELOAD_WEIGHT => LOAD_WEIGHT_MAP(i),
                    LOAD_WEIGHT => ACTIVATE_MAP(i),
                    INPUT => EXTENDED_SYSTOLIC_DATA(i),
                    LAST_SUM => INTERIM_RESULT(i - 1, j)(2 * EXTENDED_BYTE_WIDTH + i - 2 DOWNTO 0),
                    PARTIAL_SUM => INTERIM_RESULT(i, j)(2 * EXTENDED_BYTE_WIDTH + i - 1 DOWNTO 0)
                );
            END GENERATE FULL_COLUMNS;

            LEFT_CUTTED_ELEMENT :
            IF i > 2 * BYTE_WIDTH AND j = 0 GENERATE
                MACC_i4 : MACC
                GENERIC MAP(
                    LAST_SUM_WIDTH => 4 * BYTE_WIDTH,
                    PARTIAL_SUM_WIDTH => 4 * BYTE_WIDTH
                )
                PORT MAP(
                    CLK => CLK,
                    RESET => RESET,
                    ENABLE => ENABLE,
                    WEIGHT_INPUT => EXTENDED_WEIGHT_DATA(j),
                    PRELOAD_WEIGHT => LOAD_WEIGHT_MAP(i),
                    LOAD_WEIGHT => ACTIVATE_MAP(i),
                    INPUT => EXTENDED_SYSTOLIC_DATA(i),
                    LAST_SUM => INTERIM_RESULT(i - 1, j),
                    PARTIAL_SUM => INTERIM_RESULT(i, j)
                );
            END GENERATE LEFT_CUTTED_ELEMENT;

            CUTTED_COLUMNS :
            IF i > 2 * BYTE_WIDTH AND j > 0 GENERATE
                MACC_i5 : MACC
                GENERIC MAP(
                    LAST_SUM_WIDTH => 4 * BYTE_WIDTH,
                    PARTIAL_SUM_WIDTH => 4 * BYTE_WIDTH
                )
                PORT MAP(
                    CLK => CLK,
                    RESET => RESET,
                    ENABLE => ENABLE,
                    WEIGHT_INPUT => EXTENDED_WEIGHT_DATA(j),
                    PRELOAD_WEIGHT => LOAD_WEIGHT_MAP(i),
                    LOAD_WEIGHT => ACTIVATE_MAP(i),
                    INPUT => EXTENDED_SYSTOLIC_DATA(i),
                    LAST_SUM => INTERIM_RESULT(i - 1, j),
                    PARTIAL_SUM => INTERIM_RESULT(i, j)
                );
            END GENERATE CUTTED_COLUMNS;
        END GENERATE MACC_2D;
    END GENERATE MACC_GEN;

    RESULT_ASSIGNMENT :
    PROCESS (INTERIM_RESULT, SIGN_CONTROL_cs(2 + MATRIX_WIDTH - 1)) IS
        VARIABLE RESULT_DATA_v : STD_LOGIC_VECTOR(2 * EXTENDED_BYTE_WIDTH + MATRIX_WIDTH - 2 DOWNTO 0);
        VARIABLE EXTEND_v : STD_LOGIC_VECTOR(4 * BYTE_WIDTH - 1 DOWNTO 2 * EXTENDED_BYTE_WIDTH + MATRIX_WIDTH - 1);
    BEGIN
        FOR i IN MATRIX_WIDTH - 1 DOWNTO 0 LOOP
            RESULT_DATA_v := INTERIM_RESULT(MATRIX_WIDTH - 1, i)(2 * EXTENDED_BYTE_WIDTH + MATRIX_WIDTH - 2 DOWNTO 0);
            IF SIGN_CONTROL_cs(2 + MATRIX_WIDTH - 1) = '1' THEN
                EXTEND_v := (OTHERS => INTERIM_RESULT(MATRIX_WIDTH - 1, i)(2 * EXTENDED_BYTE_WIDTH + MATRIX_WIDTH - 2));
            ELSE
                EXTEND_v := (OTHERS => '0');
            END IF;

            RESULT_DATA(i) <= EXTEND_v & RESULT_DATA_v;
        END LOOP;
    END PROCESS RESULT_ASSIGNMENT;

    SEQ_LOG :
    PROCESS (CLK) IS
    BEGIN
        IF CLK'event AND CLK = '1' THEN
            IF RESET = '1' THEN
                ACTIVATE_CONTROL_cs <= (OTHERS => '0');
                SIGN_CONTROL_cs <= (OTHERS => '0');
            ELSE
                ACTIVATE_CONTROL_cs <= ACTIVATE_CONTROL_ns;
                SIGN_CONTROL_cs <= SIGN_CONTROL_ns;
            END IF;
        END IF;
    END PROCESS SEQ_LOG;
END ARCHITECTURE BEH;