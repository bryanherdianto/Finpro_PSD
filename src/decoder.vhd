LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY DECODER IS
    PORT (
        -- Inputs
        CLK             : IN STD_LOGIC;
        ENABLE          : IN STD_LOGIC;
        INSTRUCTION     : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        -- Outputs
        OPCODE          : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
        ADDRESS_FLAG    : OUT STD_LOGIC
    );
END ENTITY DECODER;

ARCHITECTURE RTL OF DECODER IS
BEGIN
    PROCESS (CLK)
    BEGIN
        IF RISING_EDGE(CLK) THEN

            -- Opcode determines the kernel operation
            -- Address flag determines which unit to use
            IF ENABLE = '1' THEN

                OPCODE          <= INSTRUCTION(2 DOWNTO 0);
                ADDRESS_FLAG    <= INSTRUCTION(3);

            ELSE

                OPCODE          <= (OTHERS => '0');
                ADDRESS_FLAG    <= '0';

            END IF;
        END IF;
    END PROCESS;
END RTL;