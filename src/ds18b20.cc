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
#include "uart_stio.h"
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
    _value = 0.0;
#ifdef UART_DEBUG
    if (!_onewire.detect_presents()) {
        printf("Unable to detect 1-wire device on the bus.\r\n");
    } else {
        printf("1-Wire device present.\r\n");
    }
#else
    _onewire.detect_presents();
#endif

  // Match the id found earlier.
    OWI_MatchRom(id, bus);
    // Send start conversion command.
    OWI_SendByte(DS1820_START_CONVERSION, bus);
    // Wait until conversion is finished.
    // Bus line is held low until conversion is finished.
    while (!OWI_ReadBit(bus))
    {
    
    }
    // Reset, presence.
    if(!OWI_DetectPresence(bus))
    {
        return -1000; // Error
    }
    // Match id again.
    OWI_MatchRom(id, bus);
    // Send READ SCRATCHPAD command.
    OWI_SendByte(DS1820_READ_SCRATCHPAD, bus);
    // Read only two first bytes (temperature low, temperature high)
    // and place them in the 16 bit temperature variable.
    temperature = OWI_ReceiveByte(bus);
    temperature |= (OWI_ReceiveByte(bus) << 8);
    
    return temperature;
}

/*
 * Reads sensor`s ROM
 */
void ds18b20::read_rom(void)
{
    _onewire.send_byte(OW_ROM_READ);
    for (uint8_t i = 0; i < OW_ROM_LENGTH; i++) {
        _rom[i] = _onewire.receive();
    }
}

/*
 * Match sensor by ROM.
 */
void ds18b20::match_rom(void)
{
    _onewire.send_byte(OW_ROM_MATCH);
    for (uint8_t i = 0; i < OW_ROM_LENGTH; i++) {
        _onewire.send(_rom[i]);
    }
}

/*
 * Returns last readed value
 */
double ds18b20::get_value(void)
{
    return 0.0;
}

/*
 * Read value from the sensor using sensor
 * specified logic
 */
void ds18b20::read(void)
{
    return;
}

