/**
 * ili9320.c - ATXMega32a4 interface to TFT display
 * drived by ili9320 IC in 16bit RGB mode
 * 
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 * GPLv3
 */

#include <avr/io.h>
#include <util/delay_basic.h>
#include "ili9320.h"


#define ili9320_pin_out(x) _ili9320_port(x).DIRSET = _ili9320_pin(x)


/**
 * ILI9320 constructor
 */
ili9320::ili9320()
{
}


void ili9320::initialize_ports(void)
{
	ili9320_lo.DIR = 0xff;
	ili9320_hi.DIR = 0xff;
	
	ili9320_pin_out(ili9320_cs);
	ili9320_pin_set(ili9320_cs);
	
	ili9320_pin_out(ili9320_rst);
	ili9320_pin_clr(ili9320_rst);
	
	ili9320_pin_out(ili9320_rs);
	ili9320_pin_clr(ili9320_rs);
	
	ili9320_pin_out(ili9320_wr);
	ili9320_pin_set(ili9320_wr);
	
	ili9320_pin_out(ili9320_rd);
	ili9320_pin_set(ili9320_rd);
	
	ili9320_data(0xff, 0xff);
}

/**
 * Reset functionality
 */
void ili9320::reset()
{
    uint32_t freq =
#ifdef F_CPU
        F_CPU;
#else
        2000000UL;
#endif
    
    if (CLK.CTRL & CLK_SCLKSEL_RC32M_gc) {
        freq = 32000000UL;
    } else if (CLK.CTRL & CLK_SCLKSEL_RC2M_gc) {
        freq = 2000000UL;
    } else if (CLK.CTRL & CLK_SCLKSEL_RC32K_gc) {
        // 32KHz internal system clock
        freq = 32768UL;
    } else if (CLK.CTRL & CLK_SCLKSE_PLL_gc) {
        // TODO: Determine freq PLL
    }
    // else --- use default external FREQ

    // We need to stay there for about 10ms

    // 1 / freq - получаем длительность одной команды
    // 0.01 
    
    // 4 cycles per iteration
    //_delay_loop_2(
}

/**
 * 
 */
void ili9320::initialize(void)
{
	this->initialize_ports();
	// Continue initialization
}

