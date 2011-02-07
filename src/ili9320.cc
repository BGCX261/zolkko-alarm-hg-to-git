/**
 * ili9320.c - ATXMega32a4 interface to TFT display
 * drived by ili9320 IC in 16bit RGB mode
 * 
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 * GPLv3
 */

#include <avr/io.h>
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
 * 
 */
void ili9320::initialize(void)
{
	this->initialize_ports();
	
	// Continue initialization
}
