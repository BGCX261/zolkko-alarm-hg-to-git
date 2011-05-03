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
#endif

#include <avr/io.h>
#include "sensor.h"
#include "ds18b20.h"

/*
 * The UART data format used when generating 1-Wire signals
 * is 8 data bits, no parity and 1 stop byte. One UART data
 * frame is used to generate the waveform for one bit 
 * or one RESET/PRESENCE sequence.
 */
void ds18b20::init(void)
{
    // TODO: Initialize UART module
}

double ds18b20::get_value(void)
{
    return 0.0;
}

uint8_t ds18b20::read(void)
{
    return 0;
}

void ds18b20::write(uint8_t value)
{
    write_bit(value & 1);
    write_bit((value & 2) >> 1);
    write_bit((value & 4) >> 2);
    write_bit((value & 8) >> 3);
    write_bit((value & 16) >> 4);
    write_bit((value & 32) >> 5);
    write_bit((value & 64) >> 6);
    write_bit((value & 128) >> 7);
}

uint8_t ds18b20::read_bit(void)
{
    //
}

void ds18b20::write_bit(uint8_t bit)
{
    //
}
