LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.STD_LOGIC_TEXTIO.ALL;
USE STD.TEXTIO.ALL;

ENTITY MMU IS
    PORT (
        -- Inputs
        CLK         : IN STD_LOGIC;
        ENABLE_MMU  : IN STD_LOGIC;
        OPCODE      : IN STD_LOGIC_VECTOR(2 DOWNTO 0)
    );
END ENTITY MMU;

ARCHITECTURE BEH OF MMU IS

    -- Make header_type to store the BMP header
    TYPE header_type IS ARRAY (0 TO 53) OF CHARACTER;

    -- Make pixel_type to store the pixel values in the image
    TYPE pixel_type IS RECORD
        red     : STD_LOGIC_VECTOR(7 DOWNTO 0);
        green   : STD_LOGIC_VECTOR(7 DOWNTO 0);
        blue    : STD_LOGIC_VECTOR(7 DOWNTO 0);
    END RECORD;

    -- Make row_type to store the row of pixels in the image
    TYPE row_type IS ARRAY (INTEGER RANGE <>) OF pixel_type;
    TYPE row_pointer IS ACCESS row_type;

    -- Make image_type to store the image as a 2D array of pixels
    -- Later, each image_pointer will point to a row_pointer
    TYPE image_type IS ARRAY (INTEGER RANGE <>) OF row_pointer;
    TYPE image_pointer IS ACCESS image_type;

    -- Make real_array_2d_type to store 2D arrays of real numbers
    -- and real_array_type will store 1D arrays of real numbers
    TYPE REAL_ARRAY_2D_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF REAL;
    TYPE REAL_ARRAY_TYPE IS ARRAY (NATURAL RANGE <>) OF REAL;
    
    -- Make a constant for the kernel size
    CONSTANT KERNEL_SIZE : INTEGER := 3;

BEGIN

    PROCESS (CLK) IS

        TYPE char_file IS FILE OF CHARACTER;
        FILE bmp_file           : char_file;
        FILE out_file           : char_file;
        FILE kernel_file        : TEXT;
        VARIABLE image_width    : INTEGER;
        VARIABLE image_height   : INTEGER;
        VARIABLE padding        : INTEGER;
        VARIABLE sum_red        : REAL;
        VARIABLE sum_blue       : REAL;
        VARIABLE sum_green      : REAL;
        VARIABLE value_in       : REAL;
        VARIABLE denominator    : REAL;
        VARIABLE kernel         : REAL_ARRAY_2D_TYPE(0 TO KERNEL_SIZE - 1, 0 TO KERNEL_SIZE - 1);
        VARIABLE kernel_flat    : REAL_ARRAY_TYPE(0 TO KERNEL_SIZE * KERNEL_SIZE - 1);
        VARIABLE header         : header_type;
        VARIABLE row            : row_pointer;
        VARIABLE temp_row       : row_pointer;
        VARIABLE row_resized    : row_pointer;
        VARIABLE image          : image_pointer;
        VARIABLE temp_image     : image_pointer;
        VARIABLE image_resized  : image_pointer;
        VARIABLE char           : CHARACTER;
        VARIABLE line_in        : LINE;
        VARIABLE file_name      : STRING(1 TO 20);

    BEGIN

        IF RISING_EDGE(CLK) THEN
            IF ENABLE_MMU = '1' THEN

                -- Select the kernel file based on the opcode
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

                -- Read entire header for image checking
                FOR i IN header_type'RANGE LOOP
                    read(bmp_file, header(i));
                END LOOP;

                -- Check ID field in BMP header to check if it is a BMP file
                ASSERT header(0) = 'B' AND header(1) = 'M'
                REPORT "First two bytes are not ""BM"". This is not a BMP file" SEVERITY failure;

                -- Check that the pixel array offset is as expected
                ASSERT CHARACTER'pos(header(10)) = 54 AND
                       CHARACTER'pos(header(11)) = 0 AND
                       CHARACTER'pos(header(12)) = 0 AND
                       CHARACTER'pos(header(13)) = 0
                REPORT "Pixel array offset in the header is not 54 bytes" SEVERITY failure;

                -- Check that DIB header size is 40 bytes,
                -- meaning that the BMP is of type BITMAPINFOHEADER
                ASSERT CHARACTER'pos(header(14)) = 40 AND
                       CHARACTER'pos(header(15)) = 0 AND
                       CHARACTER'pos(header(16)) = 0 AND
                       CHARACTER'pos(header(17)) = 0
                REPORT "DIB header size is not 40 bytes, hence not a Windows BMP file" SEVERITY failure;

                -- Check that the number of color planes is 1
                ASSERT CHARACTER'pos(header(26)) = 1 AND CHARACTER'pos(header(27)) = 0
                REPORT "The number of color planes is not 1" SEVERITY failure;

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

                -- Report image width and height
                REPORT "WIDTH: " & INTEGER'image(image_width) & ", HEIGHT: " & INTEGER'image(image_height);

                -- Number of bytes needed to pad each row to 32 bits
                padding := (4 - image_width * 3 MOD 4) MOD 4;

                -- Read the kernel values from the txt file
                FOR i IN 0 TO KERNEL_SIZE - 1 LOOP

                    readline(kernel_file, line_in); -- Read one line from the file

                    FOR j IN 0 TO KERNEL_SIZE - 1 LOOP

                        read(line_in, value_in);

                        IF line_in'length > 0 THEN

                            read(line_in, char); -- Read the slash or space

                            IF char = '/' THEN
                                read(line_in, denominator); -- Read denominator
                                value_in        := value_in / denominator;
                                kernel(i, j)    := value_in;
                                NEXT;
                            END IF;

                        END IF;

                        kernel(i, j) := value_in;

                    END LOOP;
                END LOOP;

                -- Flatten the kernel matrix
                FOR i IN 0 TO KERNEL_SIZE - 1 LOOP
                    FOR j IN 0 TO KERNEL_SIZE - 1 LOOP
                        kernel_flat(i * KERNEL_SIZE + j) := kernel(i, j);
                    END LOOP;
                END LOOP;

                -- Create a new image type in dynamic memory
                temp_image      := NEW image_type(0 TO image_height + 1);
                image           := NEW image_type(0 TO image_height + 1);
                image_resized   := NEW image_type(0 TO image_height * image_width - 1);

                -- Read the image pixel values
                -- and initialize the image vector of rows
                FOR i IN 0 TO image_height + 1 LOOP

                    -- Create a new row type in dynamic memory
                    temp_row    := NEW row_type(0 TO image_width + 1);
                    row         := NEW row_type(0 TO image_width + 1);

                    FOR j IN 0 TO image_width + 1 LOOP

                        -- Make zero padding for the first and last rows and columns
                        IF i = 0 OR i = image_height + 1 OR j = 0 OR j = image_width + 1 THEN
                            temp_row(j).red   := (OTHERS => '0');
                            temp_row(j).green := (OTHERS => '0');
                            temp_row(j).blue  := (OTHERS => '0');
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
                    image(i)      := row;

                END LOOP;

                -- Initialize all the image_resized pointers to point to row_resized
                FOR i IN 0 TO image_height * image_width - 1 LOOP
                    row_resized         := NEW row_type(0 TO KERNEL_SIZE * KERNEL_SIZE - 1);
                    image_resized(i)    := row_resized;
                END LOOP;

                FOR i IN 0 TO image_height * image_width - 1 LOOP
                    row := image_resized(i);

                    FOR j IN 0 TO KERNEL_SIZE * KERNEL_SIZE - 1 LOOP
                        row(j).red    := temp_image(i / image_width + j / KERNEL_SIZE)(i MOD image_width + j MOD KERNEL_SIZE).red;
                        row(j).green  := temp_image(i / image_width + j / KERNEL_SIZE)(i MOD image_width + j MOD KERNEL_SIZE).green;
                        row(j).blue   := temp_image(i / image_width + j / KERNEL_SIZE)(i MOD image_width + j MOD KERNEL_SIZE).blue;
                    END LOOP;
                END LOOP;

                -- Iterate over each row of resized image matrix
                FOR i IN 0 TO image_height * image_width - 1 LOOP

                    row := image(i / image_width);

                    -- Initialize the sums to 0
                    sum_red     := 0.0;
                    sum_blue    := 0.0;
                    sum_green   := 0.0;
                    
                    -- Iterate over each column of resized image matrix
                    FOR j IN 0 TO KERNEL_SIZE * KERNEL_SIZE - 1 LOOP

                        -- Compute the dot product of the kernel and the image matrix
                        sum_red     := sum_red + REAL(to_integer(unsigned(image_resized(i)(j).red))) * kernel_flat(j);
                        sum_blue    := sum_blue + REAL(to_integer(unsigned(image_resized(i)(j).blue))) * kernel_flat(j);
                        sum_green   := sum_green + REAL(to_integer(unsigned(image_resized(i)(j).green))) * kernel_flat(j);

                    END LOOP;

                    -- Clamp the values to 0-255 for red channel
                    IF sum_red < 0.0 THEN
                        sum_red := 0.0;
                    ELSIF sum_red > 255.0 THEN
                        sum_red := 255.0;
                    END IF;

                    -- Clamp the values to 0-255 for blue channel
                    IF sum_blue < 0.0 THEN
                        sum_blue := 0.0;
                    ELSIF sum_blue > 255.0 THEN
                        sum_blue := 255.0;
                    END IF;

                    -- Clamp the values to 0-255 for green channel
                    IF sum_green < 0.0 THEN
                        sum_green := 0.0;
                    ELSIF sum_green > 255.0 THEN
                        sum_green := 255.0;
                    END IF;

                    -- Assign the new pixel values to the pointer rows for output image
                    row(i MOD image_width).red    := STD_LOGIC_VECTOR(to_unsigned(INTEGER(sum_red), 8));
                    row(i MOD image_width).blue   := STD_LOGIC_VECTOR(to_unsigned(INTEGER(sum_blue), 8));
                    row(i MOD image_width).green  := STD_LOGIC_VECTOR(to_unsigned(INTEGER(sum_green), 8));

                END LOOP;

                -- Write header to output file
                FOR i IN header_type'RANGE LOOP
                    write(out_file, header(i));
                END LOOP;

                FOR i IN 0 TO image_height - 1 LOOP
                    row := image(i);

                    FOR j IN 0 TO image_width - 1 LOOP

                        -- Write blue pixel
                        write(out_file, CHARACTER'val(to_integer(unsigned(row(j).blue))));

                        -- Write green pixel
                        write(out_file, CHARACTER'val(to_integer(unsigned(row(j).green))));

                        -- Write red pixel
                        write(out_file, CHARACTER'val(to_integer(unsigned(row(j).red))));

                    END LOOP;

                    -- Deallocate memory for the row
                    deallocate(row);
                    deallocate(temp_row);
                    deallocate(row_resized);

                    -- Write padding for the bmp image
                    FOR i IN 1 TO padding LOOP
                        write(out_file, CHARACTER'val(0));
                    END LOOP;

                END LOOP;

                -- Deallocate memory for the image
                deallocate(image);
                deallocate(temp_image);
                deallocate(image_resized);

                -- Close the files
                file_close(bmp_file);
                file_close(out_file);
                file_close(kernel_file);

            END IF;
        END IF;
    END PROCESS;
END ARCHITECTURE BEH;