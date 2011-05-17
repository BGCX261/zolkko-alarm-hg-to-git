/*
 * 1-wire interface
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

#ifndef _ONEWIRE_H_
#define _ONEWIRE_H_


#define OW_ROM_READ				0x33  // READ ROM command code.
#define OW_ROM_SKIP				0xcc  // SKIP ROM command code.
#define OW_ROM_MATCH			0x55  // MATCH ROM command code.
#define OW_ROM_SEARCH			0xf0  // SEARCH ROM command code.

/*
 * Return codes
 */
#define OW_ROM_SEARCH_FINISHED	0x00    // Search finished return code.
#define OW_ROM_SEARCH_FAILED	0xff    // Search failed return code.

/*
 * UART patterns
 */
#define OW_UART_WRITE1			0xff  // UART Write 1 bit pattern.
#define OW_UART_WRITE0			0x00  // UART Write 0 bit pattern.
#define OW_UART_READ_BIT		0xff  // UART Read bit pattern.
#define OW_UART_RESET			0xf0  // UART Reset bit pattern.

#define OW_UART_2X				1
#define OW_UBRR_115200			8
#define OW_UBRR_9600			103

class OneWire()
{
	public :
		OneWire()
		{
		}
		
		void init(void);
};

#endif
