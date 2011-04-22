//
// Simple program to verify SPI connection and
// read enc28j60 part number (testing for enc28j60-B7 revision)
//
// Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
//
// Tested on atxmega128a3 @ 2MHz internal osc:
//  SCK  (E7) -> CLK
//	MISO (E6) -> DO
//  MOSI (E5) -> DI
//  SS   (E4) -> CS
//
//  ENC28J60's RST, INT pins are not connected.
//
#include <avr/io.h> 
#include <util/delay.h>


#define EREVID (0x12 | 0x60)
#define READ_CTRL_REG 0x00
#define ADDR_MASK 0x1f
#define BANK_MASK 0x60
#define ESTAT 0x1d
#define ECON1 0x1f
#define FIELD_CLR 0xa0
#define FIELD_SET 0x80
#define BSEL1 0x02
#define BSEL0 0x01
#define SOFT_RESET 0xff
#define ESTAT_CLKRDY 0x01


#define SPIX SPIE

void spi_write(uint8_t data)
{	
	SPIX.DATA = data;
	while ((SPIX.STATUS & SPI_IF_bm) == 0) ;  // wait	
}

uint8_t spi_read(void)
{
	return SPIX.DATA;
}

#define select PORTE.OUTCLR = _BV(4)

#define deselect PORTE.OUTSET = _BV(4)

void write_op(uint8_t reg, uint8_t addr, uint8_t data)
{
	select;
	spi_write(reg | (addr & ADDR_MASK));
	spi_write(data);
	deselect;
}

uint8_t read_op(uint8_t reg, uint8_t addr)
{
	select;
	spi_write(reg | (addr & ADDR_MASK));
	spi_write(0x00);
	if (addr & 0x80) {
		spi_write(0x00);
	}
	deselect;
	
	return spi_read();
}


uint8_t set_bank(uint8_t addr)
{
	write_op(FIELD_CLR, ECON1, (BSEL1|BSEL0));
	write_op(FIELD_SET, ECON1, (addr & BANK_MASK) >> 5);
}

uint8_t read(uint8_t addr)
{
	set_bank(addr);
	return read_op(READ_CTRL_REG, addr);
}

inline void master_init(void)
{
	PORTE.DIRSET   = _BV(4); // Chip select
	PORTE.PIN4CTRL = PORT_OPC_WIREDANDPULL_gc;
    PORTE.OUTSET   = _BV(4);
	SPIX.CTRL      = SPI_ENABLE_bm |
					 SPI_PRESCALER_DIV4_gc |
					 // SPI_DORD_bm   |
					 SPI_MODE_0_gc |
					 //SPI_CLK2X_bm  |
					 SPI_MASTER_bm ;
	
	SPIX.INTCTRL = SPI_INTLVL_OFF_gc;
    PORTE.DIRSET = _BV(7); // CLK
    PORTE.DIRSET = _BV(5); // MOSI
	// Other PIN parameters SPI module sets itself.
}

int main(void)
{
    // Debug pin direction
    PORTE.DIRSET = _BV(0);
    PORTE.DIRSET = _BV(1);
    
    PORTE.OUTCLR = _BV(0);
    PORTE.OUTCLR = _BV(1);
    
    // Stabilize reset of enc28j60 & etc
    _delay_ms(100);
    
    // Master
	master_init();
	
	uint8_t data = read(EREVID);
	
#ifndef _DISPLAY_BIN
	if (data == 0b00000110) {
		PORTE.OUTSET = _BV(0); // debug leds on
		PORTE.OUTSET = _BV(1);
	}
	else {
		PORTE.OUTSET = _BV(1);
	}
#else
	// Display readed code in binary format using 2 LEDS =]
	while (1) {
		_delay_ms(1000);
		uint8_t i = 0;
		for (i = 0; i < 8; i++) {
			uint8_t bit = (data >> i) & 0x01;
			if (bit) {
				PORTE.OUTSET = _BV(0);
				PORTE.OUTSET = _BV(1);
				_delay_ms(700);
				PORTE.OUTCLR = _BV(0);
				PORTE.OUTCLR = _BV(1);
			} else {
				PORTE.OUTSET = _BV(0);
				_delay_ms(700);
				PORTE.OUTCLR = _BV(0);
			}
			_delay_ms(700);
		}
		
		PORTE.OUTCLR = _BV(0);
		PORTE.OUTCLR = _BV(1);
	}
#endif
	
    while ( 1 ) {
        // Do nothing
    }
    
    return 0;
}

