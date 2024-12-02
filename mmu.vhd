library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;

entity MMU is
    Port ( clk : in STD_LOGIC;
           start : in STD_LOGIC;
           A : in integer;
           B : in integer;
           P : out integer;
           done : out STD_LOGIC);
end MMU;

architecture Behavioral of MMU is
begin
    process(clk)
    variable tmp_result : integer := 0;
    begin
        if rising_edge(clk) then
            if start = '1' then
                tmp_result := A * B;  
                done <= '0';
            else
                P <= tmp_result;
                done <= '1';
            end if;
        end if;
    end process;
end Behavioral;
