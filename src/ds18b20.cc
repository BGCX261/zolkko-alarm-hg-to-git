/*
 * Polled UART implementation of reading data from DS18B20 sensor using
 * atxmega128a3 USART module.
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

#ifdef UART_DEBUG
#include <stdio.h>
#include "uart_stdio.h"
#endif

#include <avr/io.h>
#include "onewire.h"
#include "sensor.h"
#include "ds18b20.h"

/*
 * Initialize ds18b20 module
 */
void ds18b20::init(void)
{
	// No need to initialize anything
}

/*
 * Returns last readed value
 */
float ds18b20::get_value(void)
{
    return _value;
}

/*
 * Read value from the sensor
 */
uint8_t ds18b20::read(void)
{
	if (_onewire.reset()) {
		_onewire.skip_rom();
		_onewire.send(DS18B20_START_CONVERSION);
		
		uint16_t timeout = 0;
		do {
			timeout++;
		} while (timeout < 0xffff && !_onewire.touch_bit(OW_UART_READ_BIT));
		
		if (timeout == 0xffff) {
			#ifdef UART_DEBUG
			printf("Temperature converting timeout.\r\n");
			#endif
			return false;
		}
		
		if (_onewire.reset()) {
			_onewire.skip_rom();
			_onewire.send(DS18B20_READ_SCRATCHPAD);
			
			uint8_t data_lo = _onewire.receive();
			uint8_t data_hi = _onewire.receive();
			
			uint16_t temperature_data = (data_hi << 8) | data_lo;
			
			// clear unnesesary data
			temperature_data = temperature_data & 0x0fff;
			
			uint8_t sign_bit = temperature_data & 0x8000;
			if (sign_bit) {
				temperature_data = (temperature_data ^ 0xffff) + 1;
			}
			
			uint16_t frac_part = temperature_data & 0x000f;
			uint16_t int_part  = temperature_data >> 4;
			
			_value = int_part + (frac_part * 0.0625);
			if (sign_bit) {
				_value = _value * -1;
			}
			
			return true;
		} else {
			#ifdef UART_DEBUG
			printf("Failed to reset 1-wire BUS before reading sensor's ROM.\r\n");
			#endif
			return false;
		}
	} else {
		#ifdef UART_DEBUG
		printf("Failed to reset 1-wire BUS.\r\n");
		#endif
		return false;
	}
}
