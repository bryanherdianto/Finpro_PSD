# Image Convolution on VHDL

| Instruction | Description |
| --- | --- |
| 0000 | box blur kernel with ALU |
| 0001 | horizontal edge detection kernel with ALU |
| 0010 | vertical edge detection kernel with ALU |
| 0011 | laplacian kernel with ALU |
| 0100 | sharpening kernel with ALU |
| 0101 | sobel horizontal kernel with ALU |
| 0110 | sobel vertical kernel with ALU |
| 0111 | custom kernel with ALU |
| 1000 | box blur kernel with MMU |
| 1001 | horizontal edge detection kernel with MMU |
| 1010 | vertical edge detection kernel with MMU |
| 1011 | laplacian kernel with MMU |
| 1100 | sharpening kernel with MMU |
| 1101 | sobel horizontal kernel with MMU |
| 1110 | sobel vertical kernel with MMU |
| 1111 | custom kernel with MMU |

Proyek akhir ini mendemonstrasikan implementasi konvulusi gambar menggunakan VHDL. Desain ini mengintegrasikan “Matrix Multiplier Unit” (MMU) dan “Arithmetic Logic Unit” (ALU) untuk mempercepat pemrosesan dengan berbagai kernel seperti box blur, sharpening, dan edge detection. Sistem ini mendukung pemilihan kernel dinamis berbasis file dan penangganan opcode khusus. Hasil pengujian memvalidasi kemampuan sistem untuk menangani pemrosesan gambar dengan presisi dan efisiensi.
Latar Belakang:
Pemrosesan gambar adalah operasi yang membutuhkan komputasi intensif, terutama untuk tugas seperti konvolusi. Dengan mempercepat proses ini menggunakan perangkat keras seperti FPGA, sistem dapat mencapai efisiensi yang lebih tinggi dibandingkan pemrosesan berbasis perangkat lunak.
Tujuan
Mengembangkan solusi berbasis FPGA untuk konvolusi gambar menggunakan VHDL,dengan integrasi antara MMU dan ALU.
Implementasi sistem konvolusi dengan dukungan pemilihan kernel dinamis melalui opcode
Pemrosesan gambar dalam format BMP
Mengoptimalkan kernel kompleks seperti Sobel dan Laplacian menggunakan MMU.
Link Laporan: https://docs.google.com/document/d/1dmlOlWl9W26csWaYqnI9CpCi0aAzKY-OyjDwy2cv5Hw/edit?usp=sharing
Link Slide: https://docs.google.com/presentation/d/1gRGA1_YoZ8G1khiH8HjUHjmyRaqO5o_jYUOknT4tw5A/edit
