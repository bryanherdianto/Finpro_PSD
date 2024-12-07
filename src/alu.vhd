LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.STD_LOGIC_TEXTIO.ALL;
USE STD.TEXTIO.ALL;

ENTITY ALU IS
    PORT (
        CLK         : IN STD_LOGIC;
        ENABLE_ALU  : IN STD_LOGIC;
        OPCODE      : IN STD_LOGIC_VECTOR(2 DOWNTO 0)
    );
END ENTITY ALU;

ARCHITECTURE BEH OF ALU IS

    TYPE header_type IS ARRAY (0 TO 53) OF CHARACTER;

    TYPE pixel_type IS RECORD
        red     : STD_LOGIC_VECTOR(7 DOWNTO 0);
        green   : STD_LOGIC_VECTOR(7 DOWNTO 0);
        blue    : STD_LOGIC_VECTOR(7 DOWNTO 0);
    END RECORD;

    TYPE row_type IS ARRAY (INTEGER RANGE <>) OF pixel_type;
    TYPE row_pointer IS ACCESS row_type;

    TYPE image_type IS ARRAY (INTEGER RANGE <>) OF row_pointer;
    TYPE image_pointer IS ACCESS image_type;
    
    TYPE REAL_ARRAY_2D_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF REAL;

    CONSTANT KERNEL_SIZE : INTEGER := 3;

BEGIN
    PROCESS (CLK) IS
        TYPE char_file IS FILE OF CHARACTER;
        FILE bmp_file           : char_file;
        FILE out_file           : char_file;
        FILE kernel_file        : TEXT; -- Open file for reading
        VARIABLE header         : header_type;
        VARIABLE image_width    : INTEGER;
        VARIABLE image_height   : INTEGER;
        VARIABLE padding        : INTEGER;
        VARIABLE red_result     : INTEGER;
        VARIABLE green_result   : INTEGER;
        VARIABLE blue_result    : INTEGER;
        VARIABLE row            : row_pointer;
        VARIABLE temp_row       : row_pointer;
        VARIABLE image          : image_pointer;
        VARIABLE temp_image     : image_pointer;
        VARIABLE char           : CHARACTER;
        VARIABLE line_in        : LINE;
        VARIABLE value_in       : REAL;
        VARIABLE denominator    : REAL;
        VARIABLE file_name      : STRING(1 TO 20);
        VARIABLE kernel         : REAL_ARRAY_2D_TYPE(0 TO KERNEL_SIZE - 1, 0 TO KERNEL_SIZE - 1);
    BEGIN
        IF RISING_EDGE(CLK) THEN
            IF ENABLE_ALU = '1' THEN
                -- Select the input file name based on OPCODE
                CASE OPCODE IS
                    WHEN "000" =>
                        file_name := "box_blur.txt        ";
                    WHEN "001" =>
                        file_name := "horizontal_edge.txt ";
                    WHEN "010" =>
                        file_name := "vertical_edge.txt   ";
                    WHEN "011" =>
                        file_name := "laplacian.txt       ";
                    WHEN "100" =>
                        file_name := "sharpening.txt      ";
                    WHEN "101" =>
                        file_name := "sobel_horizontal.txt";
                    WHEN "110" =>
                        file_name := "sobel_vertical.txt  ";
                    WHEN "111" =>
                        file_name := "custom.txt          ";
                    WHEN OTHERS =>
                        file_name := "custom.txt          ";
                END CASE;

                -- Open the files dynamically
                file_open(kernel_file, file_name, read_mode);
                file_open(bmp_file, "input.bmp", read_mode);
                file_open(out_file, "output.bmp", write_mode);

                -- Read entire header
                FOR i IN header_type'RANGE LOOP
                    read(bmp_file, header(i));
                END LOOP;

                -- Check ID field
                ASSERT header(0) = 'B' AND header(1) = 'M'
                REPORT "First two bytes are not ""BM"". This is not a BMP file" SEVERITY failure;

                -- Check that the pixel array offset is as expected
                ASSERT CHARACTER'pos(header(10)) = 54 AND
                       CHARACTER'pos(header(11)) = 0 AND
                       CHARACTER'pos(header(12)) = 0 AND
                       CHARACTER'pos(header(13)) = 0
                REPORT "Pixel array offset in header is not 54 bytes" SEVERITY failure;

                -- Check that DIB header size is 40 bytes,
                -- meaning that the BMP is of type BITMAPINFOHEADER
                ASSERT CHARACTER'pos(header(14)) = 40 AND
                       CHARACTER'pos(header(15)) = 0 AND
                       CHARACTER'pos(header(16)) = 0 AND
                       CHARACTER'pos(header(17)) = 0
                REPORT "DIB headers size is not 40 bytes, is this a Windows BMP?" SEVERITY failure;

                -- Check that the number of color planes is 1
                ASSERT CHARACTER'pos(header(26)) = 1 AND CHARACTER'pos(header(27)) = 0
                REPORT "Color planes is not 1" SEVERITY failure;

                -- Check that the number of bits per pixel is 24
                ASSERT CHARACTER'pos(header(28)) = 24 AND CHARACTER'pos(header(29)) = 0
                REPORT "Bits per pixel is not 24" SEVERITY failure;

                -- Read image width
                image_width := CHARACTER'pos(header(18)) +
                               CHARACTER'pos(header(19)) * 2 ** 8 +
                               CHARACTER'pos(header(20)) * 2 ** 16 +
                               CHARACTER'pos(header(21)) * 2 ** 24;

                -- Read image height
                image_height := CHARACTER'pos(header(22)) +
                                CHARACTER'pos(header(23)) * 2 ** 8 +
                                CHARACTER'pos(header(24)) * 2 ** 16 +
                                CHARACTER'pos(header(25)) * 2 ** 24;

                REPORT "WIDTH: " & INTEGER'image(image_width) & ", HEIGHT: " & INTEGER'image(image_height);

                -- Number of bytes needed to pad each row to 32 bits
                padding := (4 - image_width * 3 MOD 4) MOD 4;

                -- Read the kernel values from the file
                FOR row_txt IN 0 TO KERNEL_SIZE - 1 LOOP
                    readline(kernel_file, line_in); -- Read one line from the file

                    FOR col_txt IN 0 TO KERNEL_SIZE - 1 LOOP
                        read(line_in, value_in);

                        IF line_in'length > 0 THEN
                            read(line_in, char); -- Read the slash or space

                            IF char = '/' THEN
                                read(line_in, denominator); -- Read denominator
                                value_in := value_in / denominator;
                                kernel(row_txt, col_txt) := value_in;
                                NEXT;
                            END IF;

                        END IF;

                        kernel(row_txt, col_txt) := value_in;

                    END LOOP;
                END LOOP;

                -- Close the file after reading
                file_close(kernel_file);

                -- Create a new image type in dynamic memory
                temp_image  := NEW image_type(0 TO image_height + 1);
                image       := NEW image_type(0 TO image_height + 1);

                FOR i IN 0 TO image_height + 1 LOOP

                    -- Create a new row type in dynamic memory
                    temp_row    := NEW row_type(0 TO image_width + 1);
                    row         := NEW row_type(0 TO image_width + 1);

                    FOR j IN 0 TO image_width + 1 LOOP

                        IF i = 0 OR i = image_height + 1 OR j = 0 OR j = image_width + 1 THEN
                            temp_row(j).red     := (OTHERS => '0');
                            temp_row(j).green   := (OTHERS => '0');
                            temp_row(j).blue    := (OTHERS => '0');
                            NEXT;
                        END IF;

                        -- Read blue pixel
                        read(bmp_file, char);
                        temp_row(j).blue := STD_LOGIC_VECTOR(to_unsigned(CHARACTER'pos(char), 8));

                        -- Read green pixel
                        read(bmp_file, char);
                        temp_row(j).green := STD_LOGIC_VECTOR(to_unsigned(CHARACTER'pos(char), 8));

                        -- Read red pixel
                        read(bmp_file, char);
                        temp_row(j).red := STD_LOGIC_VECTOR(to_unsigned(CHARACTER'pos(char), 8));

                    END LOOP;

                    -- Read and discard padding
                    FOR i IN 1 TO padding LOOP
                        IF i = 0 OR i = image_height + 1 THEN
                            NEXT;
                        END IF;
                        read(bmp_file, char);
                    END LOOP;

                    -- Assign the row pointer to the image vector of rows
                    temp_image(i) := temp_row;
                    image(i) := row;

                END LOOP;

                FOR i IN 1 TO image_height LOOP
                    row := image(i);

                    FOR j IN 1 TO image_width LOOP
                        -- Compute the red channel with clamping
                        red_result := INTEGER(
                            kernel(0, 0) * REAL(to_integer(unsigned(temp_image(i - 1)(j - 1).red))) +
                            kernel(0, 1) * REAL(to_integer(unsigned(temp_image(i - 1)(j).red))) +
                            kernel(0, 2) * REAL(to_integer(unsigned(temp_image(i - 1)(j + 1).red))) +
                            kernel(1, 0) * REAL(to_integer(unsigned(temp_image(i)(j - 1).red))) +
                            kernel(1, 1) * REAL(to_integer(unsigned(temp_image(i)(j).red))) +
                            kernel(1, 2) * REAL(to_integer(unsigned(temp_image(i)(j + 1).red))) +
                            kernel(2, 0) * REAL(to_integer(unsigned(temp_image(i + 1)(j - 1).red))) +
                            kernel(2, 1) * REAL(to_integer(unsigned(temp_image(i + 1)(j).red))) +
                            kernel(2, 2) * REAL(to_integer(unsigned(temp_image(i + 1)(j + 1).red)))
                        );

                        IF red_result > 255 THEN
                            red_result := 255; -- Clamp to 255
                        ELSIF red_result < 0 THEN
                            red_result := 0; -- Clamp to 0 (in case of underflow)
                        END IF;

                        row(j).red := STD_LOGIC_VECTOR(to_unsigned(red_result, 8));

                        -- Compute the green channel with clamping
                        green_result := INTEGER(
                            kernel(0, 0) * REAL(to_integer(unsigned(temp_image(i - 1)(j - 1).green))) +
                            kernel(0, 1) * REAL(to_integer(unsigned(temp_image(i - 1)(j).green))) +
                            kernel(0, 2) * REAL(to_integer(unsigned(temp_image(i - 1)(j + 1).green))) +
                            kernel(1, 0) * REAL(to_integer(unsigned(temp_image(i)(j - 1).green))) +
                            kernel(1, 1) * REAL(to_integer(unsigned(temp_image(i)(j).green))) +
                            kernel(1, 2) * REAL(to_integer(unsigned(temp_image(i)(j + 1).green))) +
                            kernel(2, 0) * REAL(to_integer(unsigned(temp_image(i + 1)(j - 1).green))) +
                            kernel(2, 1) * REAL(to_integer(unsigned(temp_image(i + 1)(j).green))) +
                            kernel(2, 2) * REAL(to_integer(unsigned(temp_image(i + 1)(j + 1).green)))
                        );

                        IF green_result > 255 THEN
                            green_result := 255; -- Clamp to 255
                        ELSIF green_result < 0 THEN
                            green_result := 0; -- Clamp to 0
                        END IF;

                        row(j).green := STD_LOGIC_VECTOR(to_unsigned(green_result, 8));

                        -- Compute the blue channel with clamping
                        blue_result := INTEGER(
                            kernel(0, 0) * REAL(to_integer(unsigned(temp_image(i - 1)(j - 1).blue))) +
                            kernel(0, 1) * REAL(to_integer(unsigned(temp_image(i - 1)(j).blue))) +
                            kernel(0, 2) * REAL(to_integer(unsigned(temp_image(i - 1)(j + 1).blue))) +
                            kernel(1, 0) * REAL(to_integer(unsigned(temp_image(i)(j - 1).blue))) +
                            kernel(1, 1) * REAL(to_integer(unsigned(temp_image(i)(j).blue))) +
                            kernel(1, 2) * REAL(to_integer(unsigned(temp_image(i)(j + 1).blue))) +
                            kernel(2, 0) * REAL(to_integer(unsigned(temp_image(i + 1)(j - 1).blue))) +
                            kernel(2, 1) * REAL(to_integer(unsigned(temp_image(i + 1)(j).blue))) +
                            kernel(2, 2) * REAL(to_integer(unsigned(temp_image(i + 1)(j + 1).blue)))
                        );

                        IF blue_result > 255 THEN
                            blue_result := 255; -- Clamp to 255
                        ELSIF blue_result < 0 THEN
                            blue_result := 0; -- Clamp to 0
                        END IF;

                        row(j).blue := STD_LOGIC_VECTOR(to_unsigned(blue_result, 8));
                    END LOOP;
                END LOOP;

                -- Write header to output file
                FOR i IN header_type'RANGE LOOP
                    write(out_file, header(i));
                END LOOP;

                FOR i IN 1 TO image_height LOOP
                    row := image(i);

                    FOR j IN 1 TO image_width LOOP

                        -- Write blue pixel
                        write(out_file, CHARACTER'val(to_integer(unsigned(row(j).blue))));

                        -- Write green pixel
                        write(out_file, CHARACTER'val(to_integer(unsigned(row(j).green))));

                        -- Write red pixel
                        write(out_file, CHARACTER'val(to_integer(unsigned(row(j).red))));

                    END LOOP;

                    deallocate(row);

                    -- Write padding
                    FOR i IN 1 TO padding LOOP
                        write(out_file, CHARACTER'val(0));
                    END LOOP;

                END LOOP;

                -- Deallocate the image
                deallocate(image);

                -- Close the files
                file_close(bmp_file);
                file_close(out_file);

            END IF;
        END IF;
    END PROCESS;
END ARCHITECTURE BEH;