LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY decoder IS
    PORT (
        cpu_clk : IN STD_LOGIC;
        cpu_enable : IN STD_LOGIC;
        instruction : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
        opcode : OUT STD_LOGIC_VECTOR (3 DOWNTO 0);
        address_flag : OUT STD_LOGIC;
        negative_flag : OUT STD_LOGIC;
        operand1 : OUT STD_LOGIC_VECTOR (3 DOWNTO 0);
        operand2 : OUT STD_LOGIC_VECTOR (5 DOWNTO 0)
    );
END decoder;

ARCHITECTURE Behavioral OF decoder IS

BEGIN
    PROCESS (cpu_clk)
        VARIABLE temp_address_flag : STD_LOGIC;
    BEGIN
        IF rising_edge(cpu_clk) THEN
            IF cpu_enable = '1' THEN
                opcode <= instruction(15 DOWNTO 12);
                address_flag <= instruction(11);
                temp_address_flag := instruction(11);
                negative_flag <= instruction(10);
                operand1 <= instruction(9 DOWNTO 6);

                IF temp_address_flag = '1' THEN
                    operand2 <= instruction(5 DOWNTO 2) & "00";
                ELSE
                    operand2 <= instruction(5 DOWNTO 0);
                END IF;
            ELSE
                opcode <= (OTHERS => '0');
                address_flag <= '0';
                negative_flag <= '0';
                operand1 <= (OTHERS => '0');
                operand2 <= (OTHERS => '0');
            END IF;
        END IF;
    END PROCESS;

END Behavioral;