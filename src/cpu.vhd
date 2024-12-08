LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY CPU IS
    PORT (
        -- Inputs
        CLK             : IN STD_LOGIC;
        ENABLE          : IN STD_LOGIC;
        RESET           : IN STD_LOGIC;
        INSTRUCTION_RAW : IN STD_LOGIC_VECTOR(3 DOWNTO 0)
    );
END ENTITY CPU;

ARCHITECTURE RTL OF CPU IS

    -- Components
    COMPONENT DECODER IS
        PORT (
            CLK             : IN STD_LOGIC;
            ENABLE          : IN STD_LOGIC;
            INSTRUCTION     : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
            OPCODE          : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
            ADDRESS_FLAG    : OUT STD_LOGIC
        );
    END COMPONENT DECODER;

    COMPONENT ALU IS
        PORT (
            CLK         : IN STD_LOGIC;
            ENABLE_ALU  : IN STD_LOGIC;
            OPCODE      : IN STD_LOGIC_VECTOR(2 DOWNTO 0)
        );
    END COMPONENT ALU;

    COMPONENT MMU IS
        PORT (
            CLK         : IN STD_LOGIC;
            ENABLE_MMU  : IN STD_LOGIC;
            OPCODE      : IN STD_LOGIC_VECTOR(2 DOWNTO 0)
        );
    END COMPONENT MMU;

    -- Types
    TYPE state_type IS (IDLE, FETCH, DECODE, EXECUTE, COMPLETE);

    -- Signals
    SIGNAL state        : state_type := IDLE;
    SIGNAL PC           : INTEGER := 0;
    SIGNAL ENABLE_ALU   : STD_LOGIC := '0';
    SIGNAL ENABLE_MMU   : STD_LOGIC := '0';
    SIGNAL ADDRESS_FLAG : STD_LOGIC := '0';
    SIGNAL OPCODE       : STD_LOGIC_VECTOR(2 DOWNTO 0) := (OTHERS => '0');
    SIGNAL INSTRUCTION  : STD_LOGIC_VECTOR(3 DOWNTO 0) := (OTHERS => '0');

BEGIN

    -- Instantiate components and do port mapping
    DECODER_COMP : DECODER PORT MAP(
        CLK             => CLK,
        ENABLE          => ENABLE,
        INSTRUCTION     => INSTRUCTION,
        OPCODE          => OPCODE,
        ADDRESS_FLAG    => ADDRESS_FLAG
    );

    ALU_COMP : ALU PORT MAP(
        CLK         => CLK,
        ENABLE_ALU  => ENABLE_ALU,
        OPCODE      => OPCODE
    );

    MMU_COMP : MMU PORT MAP(
        CLK         => CLK,
        ENABLE_MMU  => ENABLE_MMU,
        OPCODE      => OPCODE
    );

    PROCESS (CLK, RESET) IS
    BEGIN
        IF RESET = '1' THEN

            -- Reset all signals
            PC          <= 0;
            INSTRUCTION <= (OTHERS => '0');
            ENABLE_ALU  <= '0';
            ENABLE_MMU  <= '0';
            state       <= IDLE;

        ELSIF RISING_EDGE(CLK) THEN
            IF ENABLE = '1' THEN
                CASE state IS
                    WHEN IDLE =>

                        PC <= PC + 1;

                        IF PC = 1 THEN
                            state <= FETCH;
                        END IF;

                    WHEN FETCH =>

                        PC          <= PC + 1;
                        INSTRUCTION <= INSTRUCTION_RAW;

                        IF PC = 2 THEN
                            state <= DECODE;
                        END IF;

                    WHEN DECODE =>

                        PC <= PC + 1;

                        IF PC = 3 THEN
                            state <= EXECUTE;
                        END IF;

                    WHEN EXECUTE =>

                        PC <= PC + 1;

                        IF ADDRESS_FLAG = '1' THEN
                            ENABLE_MMU <= '1';
                        ELSE
                            ENABLE_ALU <= '1';
                        END IF;

                        IF PC = 5 THEN
                            state       <= COMPLETE;
                            ENABLE_ALU  <= '0';
                            ENABLE_MMU  <= '0';
                        END IF;

                    WHEN COMPLETE =>

                        PC      <= 0;
                        state   <= IDLE;

                END CASE;
            END IF;
        END IF;
    END PROCESS;
END ARCHITECTURE RTL;