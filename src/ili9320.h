/**
 * ili9320 8bit interface for AVR XMega
 *
 * TODO: 16bit interface support
 * TODO: SPI interface support
 */

#ifndef _ili9320_h_
#define _ili9320_h_

/** Standart color codes */
#define RED    0xf800
#define GREEN  0x07e0
#define BLUE   0x001f
#define WHITE  0xffff
#define BLACK  0x0000
#define YELLOW 0xFFE0

#ifndef ili9320_cs
#define ili9320_cs A, 1
#endif

#ifndef ili9320_rs
#define ili9320_rs A, 3
#endif

#ifndef ili9320_rd
#define ili9320_rd A, 4
#endif

#ifndef ili9320_wr
#define ili9320_wr A, 5
#endif

#ifndef ili9320_reset
#define ili9320_reset A, 6
#endif

void ili9320_write_register(uint regNo, uint data);
void ili9320_write_data(uint data);
void ili9320_init();

#endif

