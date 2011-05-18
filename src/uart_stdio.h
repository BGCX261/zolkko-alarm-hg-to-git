/*
 * Interface to printing string via UART
 * atxmega128a3 @ 2MHz 9600 8N1
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
#ifndef _uart_stdio_h_
#define _uart_stdio_h_


#ifndef UART_DEV
#define UART_DEV USARTD0
#endif

#ifndef UART_TX_PORT
#define UART_TX_PORT PORTD
#endif

#ifndef UART_TX_PIN
#define UART_TX_PIN 3
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Initializes an uart module and reassign STDOUT to it
 */
extern void uart_init();

#ifdef __cplusplus
}
#endif

#endif