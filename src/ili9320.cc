/**
 * ili9320.c - ATXMega32a4 interface to TFT display
 * drived by ili9320 IC in 16bit i80 mode
 * 
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 * GPLv3
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

#define ili9320Command(x, y) this->writeRegister(x, y)

#define ili9320WriteCmd(x) this->writeIndex(x)

#define Lcd_WriteReg(x, y) this->writeRegister(x, y)

#define LCD_SetReg(x, y) this->writeRegister(x, y)

#define DelayXms(x) this->delayMs(x)

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
    /*
	this->writeRegister(0x10, 0x16, 0xB0); // power control 1 BT,AP
	this->writeRegister(0x11, 0x00, 0x37); // power control 2 DC,VC
	this->writeRegister(0x12, 0x01, 0x3E); // power control 3 VRH
	this->writeRegister(0x13, 0x1A, 0x00); // power control 4 vcom amplitude
	this->delayMs(10);
	
	this->writeRegister(0x29, 0x00, 0x0f);     // power control 7 VCOMH
	this->delayMs(10);
    */
    ili9320Command(0x0010, 0x0000);     // Power Control 1 (R10h)
    ili9320Command(0x0011, 0x0007);     // Power Control 2 (R11h)  
    ili9320Command(0x0012, 0x0000);     // Power Control 3 (R12h)
    ili9320Command(0x0013, 0x0000);     // Power Control 4 (R13h)
    this->delayMs(255);
    this->delayMs(255);
    
    ili9320Command(0x0010, 0x14B0);     // Power Control 1 (R10h)
    this->delayMs(255);
    ili9320Command(0x0011, 0x0007);     // Power Control 2 (R11h)  
    this->delayMs(255);
    ili9320Command(0x0012, 0x008E);     // Power Control 3 (R12h)
    ili9320Command(0x0013, 0x0C00);     // Power Control 4 (R13h)
    ili9320Command(0x0029, 0x0015);     // NVM read data 2 (R29h)
    this->delayMs(255);
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
    
    this->writeRegister(0x00e5, 0x80, 0x00);
    
	this->writeRegister(ili_start_oscillation, 0x00, 0x01);
	this->delayMs(50);
	
	this->writeRegister(ili_display_control1, 0x00, 0x00);
	this->delayMs(10);
	
	this->writeRegister(ili_driver_output_control1, 0x01, 0x00);
	this->writeRegister(ili_lcd_driving_control, 0x07, 0x00);
	
	this->writeRegister(ili_entry_mode, ILI9320_EM_DEFAULT); // Entry mode 0x1030
	
	this->writeRegister(ili_resize_control, 0x00, 0x00);
	
	// Set back and front porch of display to 2 lines.
	this->writeRegister(ili_display_control2, 0x02, 0x02);
	this->writeRegister(ili_display_control3, 0x00, 0x00);

	this->writeRegister(ili_display_control1, 0x01, 0x01); // power control 1 BT, AP
    
    this->writeRegister(ili_rgb_interface_control, 0x00, 0x00);
    this->writeRegister(ili_frame_rate_and_color_control, 0x00, 0x00);
    this->writeRegister(ili_display_interface_control2, 0x00, 0x00);
	
    this->initialize_power();
	
    // Horizontal | Vertical GRAM Address Set
	this->setCursor(0, 0);
	this->window(0, 0, 239, 319); // 239, 319);
	
	this->writeRegister(0x60, 0x27, 0x03); // Driver Output Control 2  
	this->writeRegister(0x61, 0x00, 0x03); // Base Image Display Control  
	this->writeRegister(0x6a, 0x00, 0x00); // Base Image Display Control
  	
    this->initialize_panel_interface();
	this->initialize_gamma();
    
	this->delayMs(10);
	this->writeRegister(ili_display_control1, 0x01, 0x37);
    this->delayMs(10);
	
	this->chip_deselect();
}

/**
 * Initialize panel interface
 */
void ili9320::initialize_panel_interface(void)
{
    this->writeRegister(ili_panel_interface_control1, 0x00, 0x10);
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
    this->writeRegister(0x07, 0x01, 0x01);
}

/**
 *
 */
void ili9320::endUpdate(void)
{
    this->writeRegister(0x07, 0x01, 0x37);
}

/**
 * Clear screen
 */
void ili9320::clear(void)
{
    //this->beginUpdate();
    this->setCursor(0, 0);
    this->writeRegister(ili_write_data_to_gram, COLOR_BLACK);
    for (unsigned long i = 0; i <= ili9320_screen_size - 1; i++) {
        this->writeData(COLOR_BLACK);
    }
    //this->endUpdate();
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

