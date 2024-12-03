library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;

entity ConvolutionCore is
    Port ( 
        clk : in STD_LOGIC;
        reset : in STD_LOGIC;
        start : in STD_LOGIC;
        data_in : in STD_LOGIC_VECTOR(15 downto 0);
        kernel : in STD_LOGIC_VECTOR(15 downto 0);
        conv_result : out STD_LOGIC_VECTOR(31 downto 0);
        done : out STD_LOGIC
    );
end ConvolutionCore;

architecture Behavioral of ConvolutionCore is
    signal data_reg, kernel_reg : STD_LOGIC_VECTOR(15 downto 0);
    signal result : integer;
    signal state : std_logic_vector(1 downto 0) := "00"; -- States: 00 = idle, 01 = processing, 10 = done
begin
    process(clk, reset)
    begin
        if reset = '1' then
            state <= "00";
            result <= 0;
        elsif rising_edge(clk) then
            case state is
                when "00" =>
                    if start = '1' then
                        --  mengload data kepada register saat start
                        data_reg <= data_in;
                        kernel_reg <= kernel;
                        state <= "01";
                        result <= 0; -- Reset result
                    end if;
                when "01" =>
                    --  Melakukan multiplication dan akumulasi
                    result <= result + to_integer(signed(data_reg)) * to_integer(signed(kernel_reg));
                    state <= "10";
                when "10" =>
                    -- Operasi selesai
                    conv_result <= std_logic_vector(to_signed(result, 32));
                    done <= '1';
                    state <= "00";
            end case;
        end if;
    end process;
end Behavioral;
