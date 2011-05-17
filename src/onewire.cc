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

#include <avr/io.h>
#include "onewire.h"

/*
 * Initialize 1-wire
 */
void OneWire::init(void)
{
	UART_TX_PORT.DIRSET = _BV(UART_TX_PIN);
    
    // 8N1
	UART_DEV.CTRLC = (uint8_t) USART_CHSIZE_8BIT_gc | USART_PMODE_DISABLED_gc;
    
    // 9600 baud @ 2Mhz
	UART_DEV.BAUDCTRLA = 12;
	UART_DEV.BAUDCTRLB = 0;
    
	UART_DEV.CTRLB |= USART_TXEN_bm | USART_RXEN_bm;
	
	// Reassign standart output to the UART


    // Choose single or double UART speed.
    OWI_UART_STATCTRL_REG_A = (OWI_UART_2X << OWI_U2X);
	
    // Enable UART transmitter and receiver.
    OWI_UART_STATCTRL_REG_B = (1 << OWI_TXEN) | (1 << OWI_RXEN);
    
    // Set up asynchronous mode, 8 data bits, no parity, 1 stop bit.
    // (Initial value, can be removed)
#ifdef URSEL
    OWI_UART_STATCTRL_REG_C = (1 << OWI_URSEL) | (1 << OWI_UCSZ1) | (1 << OWI_UCSZ0);
#else
    OWI_UART_STATCTRL_REG_C = (1 << OWI_UCSZ1) | (1 << OWI_UCSZ0);
#endif
    OWI_UART_BAUD_RATE_REG_L = OWI_UBRR_115200;
}
