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

ENTITY MACC IS
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
        WEIGHT_INPUT : IN EXTENDED_BYTE_TYPE; --!< Input of the first weight register.
        PRELOAD_WEIGHT : IN STD_LOGIC; --!< First weight register enable or 'preload'.
        LOAD_WEIGHT : IN STD_LOGIC; --!< Second weight register enable or 'load'.
        -- Input
        INPUT : IN EXTENDED_BYTE_TYPE; --!< Input for the multiply-add operation.
        LAST_SUM : IN STD_LOGIC_VECTOR(LAST_SUM_WIDTH - 1 DOWNTO 0); --!< Input for accumulation.
        -- Output
        PARTIAL_SUM : OUT STD_LOGIC_VECTOR(PARTIAL_SUM_WIDTH - 1 DOWNTO 0) --!< Output of partial sum register.
    );
END ENTITY MACC;

--! @brief Architecture of the MACC component.
ARCHITECTURE BEH OF MACC IS

    -- Alternating weight registers
    SIGNAL PREWEIGHT_cs : EXTENDED_BYTE_TYPE := (OTHERS => '0');
    SIGNAL PREWEIGHT_ns : EXTENDED_BYTE_TYPE;

    SIGNAL WEIGHT_cs : EXTENDED_BYTE_TYPE := (OTHERS => '0');
    SIGNAL WEIGHT_ns : EXTENDED_BYTE_TYPE;

    -- Input register
    SIGNAL INPUT_cs : EXTENDED_BYTE_TYPE := (OTHERS => '0');
    SIGNAL INPUT_ns : EXTENDED_BYTE_TYPE;

    SIGNAL PIPELINE_cs : MUL_HALFWORD_TYPE := (OTHERS => '0');
    SIGNAL PIPELINE_ns : MUL_HALFWORD_TYPE;

    -- Result register
    SIGNAL PARTIAL_SUM_cs : STD_LOGIC_VECTOR(PARTIAL_SUM_WIDTH - 1 DOWNTO 0) := (OTHERS => '0');
    SIGNAL PARTIAL_SUM_ns : STD_LOGIC_VECTOR(PARTIAL_SUM_WIDTH - 1 DOWNTO 0);

    ATTRIBUTE use_dsp : STRING;
    ATTRIBUTE use_dsp OF PARTIAL_SUM_ns : SIGNAL IS "yes";

BEGIN

    INPUT_ns <= INPUT;

    PREWEIGHT_ns <= WEIGHT_INPUT;
    WEIGHT_ns <= PREWEIGHT_cs;

    MUL_ADD :
    PROCESS (INPUT_cs, WEIGHT_cs, PIPELINE_cs, LAST_SUM) IS
        VARIABLE INPUT_v : EXTENDED_BYTE_TYPE;
        VARIABLE WEIGHT_v : EXTENDED_BYTE_TYPE;
        VARIABLE PIPELINE_cs_v : MUL_HALFWORD_TYPE;
        VARIABLE PIPELINE_ns_v : MUL_HALFWORD_TYPE;
        VARIABLE LAST_SUM_v : STD_LOGIC_VECTOR(LAST_SUM_WIDTH - 1 DOWNTO 0);
        VARIABLE PARTIAL_SUM_v : STD_LOGIC_VECTOR(PARTIAL_SUM_WIDTH - 1 DOWNTO 0);
    BEGIN
        INPUT_v := INPUT_cs;
        WEIGHT_v := WEIGHT_cs;
        PIPELINE_cs_v := PIPELINE_cs;
        LAST_SUM_v := LAST_SUM;

        PIPELINE_ns_v := STD_LOGIC_VECTOR(signed(INPUT_v) * signed(WEIGHT_v));

        -- Only ONE case will get synthesized!
        IF LAST_SUM_WIDTH > 0 AND LAST_SUM_WIDTH < PARTIAL_SUM_WIDTH THEN
            PARTIAL_SUM_v := STD_LOGIC_VECTOR(signed(PIPELINE_cs_v(PIPELINE_cs_v'HIGH) & PIPELINE_cs_v) + signed(LAST_SUM_v(LAST_SUM_v'HIGH) & LAST_SUM_v));
        ELSIF LAST_SUM_WIDTH > 0 AND LAST_SUM_WIDTH = PARTIAL_SUM_WIDTH THEN
            PARTIAL_SUM_v := STD_LOGIC_VECTOR(signed(PIPELINE_cs_v) + signed(LAST_SUM_v));
        ELSE -- LAST_SUM_WIDTH = 0
            PARTIAL_SUM_v := PIPELINE_cs_v;
        END IF;

        PIPELINE_ns <= PIPELINE_ns_v;
        PARTIAL_SUM_ns <= PARTIAL_SUM_v;
    END PROCESS MUL_ADD;

    PARTIAL_SUM <= PARTIAL_SUM_cs;

    SEQ_LOG :
    PROCESS (CLK) IS
    BEGIN
        IF CLK'event AND CLK = '1' THEN
            IF RESET = '1' THEN
                PREWEIGHT_cs <= (OTHERS => '0');
                WEIGHT_cs <= (OTHERS => '0');
                INPUT_cs <= (OTHERS => '0');
                PIPELINE_cs <= (OTHERS => '0');
                PARTIAL_SUM_cs <= (OTHERS => '0');
            ELSE
                IF PRELOAD_WEIGHT = '1' THEN
                    PREWEIGHT_cs <= PREWEIGHT_ns;
                END IF;

                IF LOAD_WEIGHT = '1' THEN
                    WEIGHT_cs <= WEIGHT_ns;
                END IF;

                IF ENABLE = '1' THEN
                    INPUT_cs <= INPUT_ns;
                    PIPELINE_cs <= PIPELINE_ns;
                    PARTIAL_SUM_cs <= PARTIAL_SUM_ns;
                END IF;
            END IF;
        END IF;
    END PROCESS SEQ_LOG;
END ARCHITECTURE BEH;