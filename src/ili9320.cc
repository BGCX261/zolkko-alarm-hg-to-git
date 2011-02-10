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

#define nop() __asm__ __volatile__ ("nop" ::)

#define sign(x) (x > 0) ? 1 : (x < 0) ? -1 : 0

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
 * Tune gamma
 */
void ili9320::initialize_gamma(void)
{
	this->writeRegister(ili_gamma_control1, 0x00, 0x07);
	this->writeRegister(ili_gamma_control2, 0x04, 0x03);
	this->writeRegister(ili_gamma_control3, 0x04, 0x04);
	this->writeRegister(ili_gamma_control4, 0x00, 0x02);
	this->writeRegister(ili_gamma_control5, 0x07, 0x07);
	this->writeRegister(ili_gamma_control6, 0x06, 0x06);
	this->writeRegister(ili_gamma_control7, 0x01, 0x06);
	this->writeRegister(ili_gamma_control8, 0x00, 0x07);
	this->writeRegister(ili_gamma_control9, 0x07, 0x00);
	this->writeRegister(ili_gamma_control10, 0x07, 0x07);
}

/**
 * Initialize ili9320 display
 */
void ili9320::initialize(void)
{
	this->initialize_ports();
	this->reset();
	
	this->chip_select();
	
	this->writeRegister(ili_start_oscillation, 0x00, 0x01);
	this->commandDelay();
	
	this->writeRegister(ili_display_control1, 0x00, 0x00);
	this->commandDelay();
	
	this->writeRegister(ili_driver_output_control1, 0x01, 0x00);
	this->writeRegister(ili_lcd_driving_control, 0x07, 0x00);
	
	this->writeRegister(ili_entry_mode, 0x10, 0x30);     // Entry mode 
	
	this->writeRegister(ili_resize_control, 0x00, 0x00); // Resize control
	
	// Set back and front porch of display to 2 lines.
	this->writeRegister(ili_display_control2, 0x02, 0x02);
	this->writeRegister(ili_display_control3, 0x00, 0x00);	//Display control (3)
	
	this->writeRegister(0x07, 0x01, 0x01); // power control 1 BT, AP
	
	this->writeRegister(0x10, 0x16, 0xB0); // power control 1 BT,AP
	this->writeRegister(0x11, 0x00, 0x37); // power control 2 DC,VC
	this->writeRegister(0x12, 0x01, 0x3E); // power control 3 VRH
	this->writeRegister(0x13, 0x1A, 0x00); // power control 4 vcom amplitude
	this->commandDelay();
	
	this->writeRegister(0x29, 0x000F);     // power control 7 VCOMH
	this->commandDelay();
	
	this->writeRegister(0x20, 0x00, 0x00); // Horizontal GRAM Address Set
	this->writeRegister(0x21, 0x00, 0x00); // Vertical GRAM Address Set
	
	this->window(0, 0, 319, 239);
	
	this->writeRegister(0x60, 0x27, 0x00); // Driver Output Control 2  
	this->writeRegister(0x61, 0x00, 0x03); // Base Image Display Control  
	this->writeRegister(0x6a, 0x00, 0x00); // Base Image Display Control
  	
    this->initialize_panel_interface();
	this->initialize_gamma();
	
	this->writeRegister(0x07, 0x01, 0x73);
	this->commandDelay();
	
	this->chip_deselect();
	this->commandDelay();
}

/**
 * Initialize panel interface
 */
void ili9320::initialize_panel_interface(void)
{
    this->writeRegister(0x90, 0x00, 0x10);
    this->writeRegister(0x92, 0x00, 0x00);
    this->writeRegister(0x93, 0x00, 0x00);
}

/**
 * 10ms delay
 */
void ili9320::resetDelay(void)
{
    if (CLK.CTRL & CLK_SCLKSEL_RC32K_gc) {
		_delay_loop_2(82);
	} else if (CLK.CTRL & CLK_SCLKSEL_RC32M_gc){
		_delay_loop_2(40000);
		_delay_loop_2(40000);
	} else if (CLK_SCLKSEL_RC2M_gc == (CLK.CTRL & CLK_SCLKSEL_gm)) {
		_delay_loop_2(5000);
	} else if (CLK.CTRL & CLK_SCLKSEL_PLL_gc) {
		uint32_t count;
		float factor = ((OSC.PLLCTRL & OSC_PLLFAC_gm) >> OSC_PLLFAC_gp) * 1.0;
		if (OSC.PLLCTRL & OSC_PLLSRC_RC2M_gc) {
            count = ((20000.0 * (factor / 100)) / 4) + 1;
        } else if (OSC.PLLCTRL & OSC_PLLSRC_RC32M_gc) {
            count = ((320000.0 * (factor / 100)) / 4) + 1;
        } else {
            count = (((F_CPU * factor) / 100) / 4) + 1;
        }
        
        while (count > 0x0000ffff) {
            _delay_loop_2(0xffff);
            count -= 0xffff;
        }
        
        if (count != 0) _delay_loop_2((uint16_t)(count & 0xffff));
	} else if (CLK.CTRL & CLK_SCLKSEL_XOSC_gc) {
		uint32_t count = (F_CPU / 100 / 4) + 1;
        
		while (count > 0x0000ffff) {
			_delay_loop_2(0xffff);
			count -= 0xffff;
		}
        
        if (count != 0) _delay_loop_2((uint16_t)(count & 0xffff));
	}
}

/**
 * 100us delay
 */
void ili9320::commandDelay(void)
{
    if (CLK.CTRL & CLK_SCLKSEL_RC32M_gc) {
        nop();
    } else if (CLK.CTRL & CLK_SCLKSEL_RC32M_gc) {
        _delay_loop_2(320);
    } else {
        _delay_loop_2(20);
    }
}

/**
 * Reset functionality.
 */
void ili9320::reset()
{
    ili9320_pin_clr(ili9320_rst);
    this->resetDelay();
    ili9320_pin_set(ili9320_rst);
}

/**
 * Initialize window
 */
void ili9320::window(uint16_t xStart, uint16_t yStart, uint16_t xEnd, uint16_t yEnd)
{
    this->writeRegister(ili_horizontal_address_start_position, xStart);
	this->writeRegister(ili_horizontal_address_end_position, xEnd);
	this->writeRegister(ili_vertical_address_start_position, yStart);
	this->writeRegister(ili_vertical_address_end_position, yEnd);
}

/**
 * Clear screen
 */
void ili9320::clear(void)
{
    this->setCursor(0, 0);
    this->writeRegister(ili_write_data_to_gram, COLOR_BLACK);
    for (unsigned long i = 0; i <= ili9320_screen_size - 1; i++) {
        this->writeData(COLOR_BLACK);
    }
}

/**
 *
 */
void ili9320::putPixel(uint16_t x, uint16_t y, uint16_t color)
{
    this->setCursor(x, y);
    this->writeRegister(ili_write_data_to_gram, color);
}

/**
 *
 */
void ili9320::lineTo(uint16_t x1, uint16_t y1, uint16_t x2, uint16_t y2, uint16_t color)
{
	int16_t dx, dy, sx, sy, check, e, x, y;
	
	dx = x1 - x2;
	if (dx < 0) dx = -dx;
	
	dy = y1 - y2;
	if (dy < 0) dy = -dy;
	
	sx = sign(x2 - x1);
	sy = sign(y2 - y1);
    
	x = x1;
	y = y1;
    
	check = 0;
	
	if (dy > dx) {
		dx = dx + dy;
		dy = dx - dy;
		dx = dx - dy;
		check = 1;
	}
	
	e = 2 * dy - dx;
	
	for (int16_t i = 0; i < dx; i++) {
		this->putPixel(x, y, color);
		
		if (e >= 0) {
			if (check == 1) {
				x = x + sx;
			} else {
				y = y + sy;
            }
			e = e - 2 * dx;
        }
		
		if (check == 1) {
			y = y + sy;
		} else {
			x = x + sx;
		}
		e = e + 2 * dy;
	}
}

