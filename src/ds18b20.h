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

#ifndef _ds18b20_h_
#define _ds18b20_h_

#ifndef DS18B20_FAMILY_ID
#define DS18B20_FAMILY_ID 			0x10
#endif

#ifndef DS18B20_FAMILY_CODE
#define DS18B20_FAMILY_CODE			0x28
#endif

#ifndef DS18B20_START_CONVERSION
#define DS18B20_START_CONVERSION	0x44
#endif

#ifndef DS18B20_WRITE_SCRACHPAD
#define DS18B20_WRITE_SCRACHPAD		0x4e
#endif  DS18B20_WRITE_SCRACHPAD

#ifndef DS18B20_READ_SCRATCHPAD
#define DS18B20_READ_SCRATCHPAD		0xbe
#endif

#ifndef DS18B20_COPY_SCRATCHPAD
#define DS18B20_COPY_SCRATCHPAD		0x48
#endif

#ifndef DS18B20_RECALL_E
#define DS18B20_RECALL_E			0xb8
#endif

#ifndef DS18B20_POWER_STATUS
#define DS18B20_POWER_STATUS		0xb4
#endif

class ds18b20 : public sensor
{
    private:
        one_wire& _onewire;
		
        float _value;
        
    public:
        ds18b20(one_wire& __onewire) : sensor(),
            _onewire(__onewire)
        {
			_value = 0.0;
        }
        
        void init(void);
        
        float get_value(void);
        
        uint8_t read(void);
};

#endif

