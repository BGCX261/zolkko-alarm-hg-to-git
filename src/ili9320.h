/**
 * ili9320.h - ATXMega32a4 interface to TFT display
 * drived by ili9320 IC in 16bit i80 mode
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 * GPLv3
 */

#ifndef _ILI9320_H_
#define _ILI9320_H_

#define ili9320_screen_size 76800

#ifndef ili9320_hi_byte
#define ili9320_hi_byte B
#endif

#ifndef ili9320_lo_byte
#define ili9320_lo_byte C
#endif

#ifndef ili9320_cs
#define ili9320_cs B, 3
#endif

#ifndef ili9320_rs
#define ili9320_rs D, 0
#endif

#ifndef ili9320_wr
#define ili9320_wr D, 1
#endif

#ifndef ili9320_rd
#define ili9320_rd E, 2
#endif

#ifndef ili9320_rst
#define ili9320_rst E, 3
#endif

#ifndef ili9320_lo
#define ili9320_lo PORTA
#endif

#ifndef ili9320_hi
#define ili9320_hi PORTC
#endif

#ifndef ili9320_hi_byte
#define ili9320_hi_byte B
#endif

#ifndef ili9320_lo_byte
#define ili9320_lo_byte C
#endif

#ifndef ili9320_cs
#define ili9320_cs B, 3
#endif

#ifndef ili9320_rs
#define ili9320_rs D, 0
#endif

#ifndef ili9320_wr
#define ili9320_wr D, 1
#endif

#ifndef ili9320_rd
#define ili9320_rd E, 2
#endif

#ifndef ili9320_rst
#define ili9320_rst E, 3
#endif

#ifndef ili9320_lo
#define ili9320_lo PORTA
#endif

#ifndef ili9320_hi
#define ili9320_hi PORTC
#endif

#define ili9320_port(x) _ili9320_port(x)

#define _ili9320_port(x, y) PORT##x

#define _ili9320_pin(x, y) _BV(y)

#define ili9320_pin(x) _ili9320_pin(x)

#define ili9320_pin_clr(x) _ili9320_port(x).OUTCLR = _ili9320_pin(x)

#define ili9320_pin_set(x) _ili9320_port(x).OUTSET = _ili9320_pin(x)

#define ili9320_data(hi, lo) ili9320_hi.OUT = hi; ili9320_lo.OUT = lo;

#define ili9320_hbyte(x) (((x & 0xff00) >> 8) & 0xff)

#define ili9320_lbyte(x) (x & 0xff)

#ifndef ili9320_nop
#define ili9320_nop  __asm__ __volatile__ ("nop" ::)
#endif


// ILI9320 register`s addresses
#define ili_start_oscillation 0x00
#define ili_driver_output_control1 0x01
#define ili_lcd_driving_control 0x02
#define ili_entry_mode 0x03
#define ili_resize_control 0x04
#define ili_display_control1 0x07
#define ili_display_control2 0x08
#define ili_display_control3 0x09
#define ili_display_control4 0x0a
#define ili_rgb_interface_control 0x0c
#define ili_frame_marker_position 0x0d
#define ili_display_interface_control2 0x0f

#define ili_power_control1 0x10
#define ili_power_control2 0x11
#define ili_power_control3 0x12
#define ili_power_control4 0x13
#define ili_power_control7 0x29

#define ili_horizontal_gram_address_set 0x20
#define ili_vertical_gram_address_set 0x21
#define ili_write_data_to_gram 0x22

#define ili_frame_rate_and_color_control 0x2b

#define ili_gamma_control1 0x30
#define ili_gamma_control2 0x31
#define ili_gamma_control3 0x32
#define ili_gamma_control4 0x35
#define ili_gamma_control5 0x36
#define ili_gamma_control6 0x37
#define ili_gamma_control7 0x38
#define ili_gamma_control8 0x39
#define ili_gamma_control9 0x3c
#define ili_gamma_control10 0x3d

#define ili_horizontal_address_start_position 0x50
#define ili_horizontal_address_end_position 0x51
#define ili_vertical_address_start_position 0x52
#define ili_vertical_address_end_position 0x53

#define driver_output_control2 0x60
#define base_image_display_control 0x61

#define ili_panel_interface_control1 0x90
#define ili_panel_interface_control2 0x92
#define ili_panel_interface_control3 0x93
#define ili_panel_interface_control4 0x95
#define ili_panel_interface_control5 0x97
#define ili_panel_interface_control6 0x98

//
// ILI9320 Entry mode
//

// Moves origin address according to the  ID setting when  a window address area is made
#define ILI9320_EM_ORG 0x0080

// Hight-speed write mode
#define ILI9320_EM_HWM 0x0100

// Swap R and B order of written data
#define ILI9320_EM_GBR 0x1000

// 16-bit MCU interface data format (transferring mode)
#define ILI9320_EM_65K     0x0000 // 80-system 16-bit interface (1 transfers/pixel) 65,536 colors
#define ILI9320_EM_65KA    0x4000
#define ILI9320_EM_246K_SL 0x8000 // 80-system 16-bit interface (2 transfers/pixel) 262,144 colors
#define ILI9320_EM_246K_SF 0xc000


//
// ILI9320 RGB Interface control
//

// RGB Data wdth
#define ILI9320_RGB_DATA_WIDTH_18 0x0000
#define ILI9320_RGB_DATA_WIDTH_16 0x0001
#define ILI9320_RGB_DATA_WIDTH_6 0x0002
#define ILI9320_RGB_DATA_WIDTH_DISABLED 0x0003

// ILI9320 Display operation modes
#define ILI9320_DOM_INTERNAL_SYSTEM_CLOCK 0x0000
#define ILI9320_DOM_RGB_INTERFACE 0x0010
#define ILI9320_DOM_VSYNC_INTERFACE 0x0020
#define ILI9320_DOM_DISABLED 0x0030

// Select interface to access GRAM
#define ILI9320_GRAM_ACCESS_SYSTEM 0x0000
#define ILI9320_GRAM_ACCESS_RGB 0x0100

// Set GRAM write cycle through the RGB interface
#define ILI9320_GRAM_WC_RGB_1F 0x0000
#define ILI9320_GRAM_WC_RGB_2F 0x2000
#define ILI9320_GRAM_WC_RGB_3F 0x4000
#define ILI9320_GRAM_WC_RGB_4F 0x6000
#define ILI9320_GRAM_WC_RGB_5F 0x8000
#define ILI9320_GRAM_WC_RGB_6F 0xa000
#define ILI9320_GRAM_WC_RGB_7F 0xc000
#define ILI9320_GRAM_WC_RGB_8F 0xe000

//
// Default colors
//

// Default colors
#define COLOR_RED 0xf800
#define COLOR_GREEN 0x07e0
#define COLOR_BLUE 0x001f
#define COLOR_WHITE 0xffff
#define COLOR_BLACK 0x0000
#define COLOR_YELLOW 0xffe0


//
// Main class
//

class ili9320 {
	private:
        
        uint8_t entryModeHi;
        
        uint8_t entryModeLo;
        
		/**
		 * Write impulse
		 */
		inline void writeImpulse(void)
		{
			ili9320_pin_clr(ili9320_wr);
			ili9320_pin_set(ili9320_wr);
		}
        
		/**
		 * Output register address into address line.
		 */
		inline void writeIndex(uint8_t index)
		{
			this->reg_select();
			ili9320_data(0, index);
			this->writeImpulse();
			this->reg_deselect();
		}
		
		/**
		 * Output data
		 */
		inline void writeData(uint16_t data)
		{
			ili9320_data(ili9320_hbyte(data), ili9320_lbyte(data));
			this->writeImpulse();
		}
		
		/**
		 * Output data
		 */
		inline void writeData(uint8_t hi, uint8_t lo)
		{
			ili9320_data(hi, lo);
			this->writeImpulse();
		}
        
        void initialize_power(void);
        
        void initialize_gamma(void);
        
        void initialize_ports(void);
        
        void initialize_panel_interface(void);
        
        void beginUpdate(void);
        
        void endUpdate(void);
        
        
	public:
		ili9320();
		
		void initialize(void);
		
 		/**
 		 * Register select. Active - LOW.
 		 */
		inline void reg_select(void)
		{
			ili9320_pin_clr(ili9320_rs);
		}
		
		/**
 		 * Register deselect. Active - LOW.
 		 */
		inline void reg_deselect(void)
		{
			ili9320_pin_set(ili9320_rs);
		}
		
		/**
		 * Chip select. Active LOW.
		 */
		inline void chip_select(void)
		{
			ili9320_pin_clr(ili9320_cs);
		}
		
		/**
		 * Chip deselect. Active LOW.
		 */
		inline void chip_deselect(void)
		{
			ili9320_pin_set(ili9320_cs);
		}
		
		/**
		 * Write deselect. Active LOW.
		 */
		inline void write_select(void)
		{
			ili9320_pin_clr(ili9320_wr);
		}
		
		/**
		 * Write deselect. Active LOW.
		 */
		inline void write_deselect(void)
		{
			ili9320_pin_set(ili9320_wr);
		}
		
		/**
		 * Write data into register index
		 */
		inline void writeRegister(uint8_t index, uint16_t data)
		{
			this->writeIndex(index);
			this->writeData(data);
		}
		
		/**
		 * Write data into register index
		 */
		inline void writeRegister(uint8_t index, uint8_t hi, uint8_t lo)
		{
			this->writeIndex(index);
			this->writeData(hi, lo);
		}
		
		/**
 		 * set ili9320 cursor into point(x, y)
		 */
		inline void setCursor(uint16_t x, uint16_t y)
		{
			this->writeRegister(ili_horizontal_gram_address_set, x);
			this->writeRegister(ili_vertical_gram_address_set, y);
		}
		
		/**
		 * Clear screen
		 */
		void clear(void);
        
        /**
         * Reset controller
         */
        void reset(void);
        
        /**
         *
         */
        void putPixel(uint16_t x, uint16_t y, uint16_t color);
        
        /**
         *
         */
        void window(uint16_t xStart, uint16_t yStart, uint16_t xEnd, uint16_t yEnd);
        
        /**
         *
         */
        void lineTo(uint16_t x1, uint16_t y1, uint16_t x2, uint16_t y2, uint16_t color);
        
        void delayMs(uint8_t count);
};

#endif

