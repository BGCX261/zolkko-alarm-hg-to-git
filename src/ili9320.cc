/*
 * ili9320.cc - ATXMega32a4 interface to TFT display
 * drived by ili9320 IC in 16bit i80 mode
 *
 * TODO: Add interrupt support, async bufferred transfer
 * and receiving, DMA slave mode, acting in slave mode,
 * switching into slave mode and other XMega SPI module features.
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 *
 * This file is part of the SmokeHouseCTRL Firmware.
 *
 * The SmokeHouseCTRL Firmware is free software: you can redistribute it
 * and/or modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * The SmokeHouseCTRL Firmware is distributed in the hope that it will be
 * useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with SmokeHouseCTRL Firmware.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#include <avr/io.h>
#include <util/delay_basic.h>
#include "ili9320.h"


#define ili9320_pin_out(x) _ili9320_port(x).DIRSET = _ili9320_pin(x)

#define sign(x) (x > 0) ? 1 : (x < 0) ? -1 : 0

/**
 * ILI9320 constructor
 */
ili9320::ili9320()
{
}


/**
 * Initialize ports
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
	this->writeRegister(ili_gamma_control1, 0x00, 0x06);
	this->writeRegister(ili_gamma_control2, 0x01, 0x01);
	this->writeRegister(ili_gamma_control3, 0x00, 0x03);
	this->writeRegister(ili_gamma_control4, 0x01, 0x06);
	this->writeRegister(ili_gamma_control5, 0x0b, 0x02);
	this->writeRegister(ili_gamma_control6, 0x03, 0x02);
	this->writeRegister(ili_gamma_control7, 0x07, 0x07);
	this->writeRegister(ili_gamma_control8, 0x00, 0x07);
	this->writeRegister(ili_gamma_control9, 0x60, 0x00);
	this->writeRegister(ili_gamma_control10, 0x02, 0x0b);
    this->delayMs(10);
}

/**
 * Initialize power
 */
void ili9320::initialize_power(void)
{
    this->writeRegister(0x0010, 0x00, 0x00);
    this->writeRegister(0x0011, 0x00, 0x07);
    this->writeRegister(0x0012, 0x00, 0x00);
    this->writeRegister(0x0013, 0x00, 0x00);
    this->delayMs(255);
    this->delayMs(255);
    
    this->writeRegister(0x0010, 0x14, 0xB0);
    this->delayMs(255);
    this->writeRegister(0x0011, 0x00, 0x07);
    this->delayMs(255);
    this->writeRegister(0x0012, 0x00, 0x8E);
    this->writeRegister(0x0013, 0x0C, 0x00);
    this->writeRegister(0x0029, 0x00, 0x15);
    this->delayMs(255);
}

void ili9320::initialize_partial_image(void)
{
    this->writeRegister(0x80, 0x00, 0x00); // Display Position? Partial Display 1.   
    this->writeRegister(0x81, 0x00, 0x00); // RAM Address Start? Partial Display 1.   
    this->writeRegister(0x82, 0x00, 0x00); // RAM Address End-Partial Display 1.  
    
    this->writeRegister(0x83, 0x00, 0x00); // Displsy Position? Partial Display 2.
    this->writeRegister(0x84, 0x00, 0x00); // RAM Address Start? Partial Display 2.   
    this->writeRegister(0x85, 0x00, 0x00); // RAM Address End? Partial Display 2. 
}

/**
 * Initialize ili9320 display
 */
void ili9320::initialize(void)
{
	this->initialize_ports();
	this->reset();
	
	this->chip_select();
    this->delayMs(255);
    
    // Starting oscillation and stabilizing for a 50ms
	this->writeRegister(ILI9320_START_OSCILLATION, 0x00, ILI9320_OSC_START);
	this->delayMs(50);
	
	this->writeRegister(ILI9320_DISPLAY_CTRL1, 0x00, 0x00);
	this->delayMs(10);
	
	this->writeRegister(ili_driver_output_control1, 0x01, 0x00);
	this->writeRegister(ili_lcd_driving_control, 0x07, 0x00);
	
    entryMode = ILI9320_EM_65K | ILI9320_EM_HI | ILI9320_EM_VI;
	this->writeRegister(ILI9320_ENTRY_MODE, entryMode);
	
    // There is no resizing by default
	this->writeRegister(ILI9320_RESIZE_CTRL, 0x00, 0x00);
	
	// Set back and front porch of display to 2 lines.
	this->writeRegister(ILI9320_DISPLAY_CTRL2, 0x02, 0x02);
	this->writeRegister(ILI9320_DISPLAY_CTRL3, 0x04, 0x2f);
    this->writeRegister(ILI9320_DISPLAY_CTRL4, 0x00, 0x00);
    
    this->writeRegister(ili_rgb_interface_control, 0x00, 0x00);
    this->writeRegister(ili_frame_rate_and_color_control, 0x00, 0x00);
    this->writeRegister(ili_display_interface_control2, 0x00, 0x00);
    
    setCursor(0, 0);
    window(0, 0, 240, 320);
	
    this->initialize_power();
	
    // Sacnning 320 lines, from the bottom to the top
    // First gate is G0
	this->writeRegister(ILI9320_DRIVER_OUTPUT_CONTROL2, ILI9320_GSC_SCAN_DIRECTION_TOP | 0x27, 0x00);
	this->writeRegister(ILI9320_BASE_IMAGE_DISPLAY_CONTROL, 0x00, ILI9320_GSC_GRAYSCALE_INVERSION | ILI9320_GSC_NONDISPLAY_AREA_HI);
	this->writeRegister(ILI9320_VERTICAL_SCROLL_CONTROL, 0x00, 0x00);
    
	this->initialize_gamma();
    this->initialize_partial_image();
    this->initialize_panel_interface();
    
	this->delayMs(10);
	this->writeRegister(ILI9320_DISPLAY_CTRL1,
                ILI9320_DC1_D0 | ILI9320_DC1_D1 |
                ILI9320_DC1_DTE |
                ILI9320_DC1_GON
                | ILI9320_DC1_BASE_IMG_ENABLE);
    this->delayMs(10);
	
	this->chip_deselect();
}

/**
 * Initialize panel interface
 */
void ili9320::initialize_panel_interface(void)
{
    this->writeRegister(ili_panel_interface_control1, 0x00, 0x1f); // 0x001f
    this->writeRegister(ili_panel_interface_control2, 0x00, 0x00);
    this->writeRegister(ili_panel_interface_control3, 0x00, 0x00);
    this->writeRegister(ili_panel_interface_control4, 0x00, 0x00);
    this->writeRegister(ili_panel_interface_control5, 0x00, 0x00);
    this->writeRegister(ili_panel_interface_control6, 0x00, 0x00);
}

/**
 * 1 x count ms delay
 */
void ili9320::delayMs(uint8_t count)
{
    uint32_t c = 0;
    
    if (CLK.CTRL & CLK_SCLKSEL_RC32K_gc) {
        c = 9;
	} else if (CLK.CTRL & CLK_SCLKSEL_RC32M_gc){
        c = 8001;
	} else if (CLK_SCLKSEL_RC2M_gc == (CLK.CTRL & CLK_SCLKSEL_gm)) {
        c = 501;
	} else if (CLK.CTRL & CLK_SCLKSEL_PLL_gc) {
		float factor = ((OSC.PLLCTRL & OSC_PLLFAC_gm) >> OSC_PLLFAC_gp) * 1.0;
        
		if (OSC.PLLCTRL & OSC_PLLSRC_RC2M_gc) {
            c = ((2000.0 * (factor / 1000.0)) / 4) + 1;
        } else if (OSC.PLLCTRL & OSC_PLLSRC_RC32M_gc) {
            count = ((32000.0 * (factor / 1000.0)) / 4) + 1;
        } else {
            count = (((F_CPU * factor) / 1000.0) / 4) + 1;
        }
	} else if (CLK.CTRL & CLK_SCLKSEL_XOSC_gc) {
		c = (uint32_t)(F_CPU / 1000.0 / 4) + 1;
	}
    
    while (count > 0) {
        uint32_t i = c;
        while (i > 0xffff) {
            _delay_loop_2(0xffff);
            i -= 0xffff;
        }
        _delay_loop_2(i);
        count--;
    }
}

/**
 * Reset functionality.
 */
void ili9320::reset()
{
    ili9320_pin_clr(ili9320_rst);
    this->delayMs(255);
    this->delayMs(255);
    this->delayMs(255);
    this->delayMs(255);
    ili9320_pin_set(ili9320_rst);
    this->delayMs(255);
    this->delayMs(255);
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
 *
 */
void ili9320::beginUpdate(void)
{
    this->writeRegister(ILI9320_DISPLAY_CTRL1, 0x01, 0x01);
}

/**
 *
 */
void ili9320::endUpdate(void)
{
    this->writeRegister(ILI9320_DISPLAY_CTRL1, 0x01, 0x37);
}

/**
 * Clear screen
 */
void ili9320::clear(void)
{
    this->setCursor(0, 0);
    this->writeRegister(ili_write_data_to_gram, 0x0000);
    for (uint32_t i = 0; i <= ili9320_screen_size; i++)  this->writeData(0x0000);
}

/**
 * Fill only specified area using @color
 */
void ili9320::fill(uint16_t x1, uint16_t y1, uint16_t w, uint16_t h, uint16_t color)
{
    uint32_t pixelCount = w * h;
    
    this->setCursor(0, 0);
    this->window(x1, y1, x1 + w - 1, y1 + h - 1);
    this->writeRegister(ILI9320_ENTRY_MODE, this->entryMode | ILI9320_EM_ORG);
    this->writeRegister(ili_write_data_to_gram, color);
    for (uint32_t i = 1; i < pixelCount; i++) this->writeData(color);
    this->writeRegister(ILI9320_ENTRY_MODE, this->entryMode);
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
 * Bezenhem Algorithm
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

