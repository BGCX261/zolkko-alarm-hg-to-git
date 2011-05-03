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

class ds18b20 : public sensor
{
    private:
        USART_t& _uart;
        
        PORT_t& _rx_port;
        
        uint8_t _rx_pin;
        
        PORT_t& _tx_port;
        
        uint8_t _tx_pin;

        /*
         * Read one bit
         */
        uint8_t read_bit(void);
        
        /*
         * Write bit
         */
        void write_bit(uint8_t);
        
        /*
         * Read one byte from sensor
         */
        uint8_t read(void);
        
        /*
         * Write one byte into sensor
         */
        void write(uint8_t value);
        
    public:
        ds18b20(USART_t& __uart,
                PORT_t& __rx_port, uint8_t __rx_pin,
                PORT_t& __tx_port, uint8_t __tx_pin) : sensor(),
            _uart(__uart),
            _rx_port(__rx_port),
            _tx_port(__tx_port)
        {
            _rx_pin = __rx_pin;
            _tx_pin = __tx_pin;
        }
        
        void init(void);
        
        double get_value(void);
};

#endif

