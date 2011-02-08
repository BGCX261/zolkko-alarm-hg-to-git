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


/**
 *
 */
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
 * Reset functionality.
 */
void ili9320::reset()
{
    ili9320_pin_clr(ili9320_rst);
    
    // Hold at low state
    if (CLK.CTRL & CLK_SCLKSEL_RC32K_gc) {
		_delay_loop_2(82); // 32768 / 100 / 4 ~ 328 / 4 ~ 82
	} else if (CLK.CTRL & CLK_SCLKSEL_RC32M_gc){
		_delay_loop_2(40000);
		_delay_loop_2(40000);
	} else if (CLK.CTRL & CLK_SCLKSEL_RC2M_gc) {
		_delay_loop_2(5000);
	} else if (CLK_SCLKSEL_PLL_gc) {
		// Not implemented yet
        //
	} else if (CLK.CTRL & CLK_SCLKSEL_XOSC_gc) {
		uint32_t count = (F_CPU / 100 / 4) + 1;
		while (count > 0x0000ffff) {
			_delay_loop_2(0xffff);
			count -= 0xffff;
		}
		_delay_loop_2((uint16_t)(count & 0xffff));
	}
    
    ili9320_pin_set(ili9320_rst);
}


/**
 * 
 */
void ili9320::initialize(void)
{
	this->initialize_ports();
    this->reset();
}

