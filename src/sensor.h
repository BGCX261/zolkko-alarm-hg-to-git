/*
 * Abstract sensor class
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

#ifndef _sensor_h_
#define _sensor_h_

class sensor
{
    public:
        sensor()
        {
        }
        
        virtual void init(void) = 0;
        
        virtual double get_value(void) = 0;
};

#endif

