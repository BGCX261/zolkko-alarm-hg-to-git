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

#ifdef UART_DEBUG
#include <stdio.h>
#include "uart_stdio.h"
#endif
 
#include <avr/io.h>
#include "onewire.h"

/*
 * Initialize 1-wire
 */
void one_wire::init(void)
{
	UART_TX_PORT.DIRSET = _BV(UART_TX_PIN);
	
	// 8N1
	_uart.CTRLC = (uint8_t) USART_CHSIZE_8BIT_gc | USART_PMODE_DISABLED_gc; // TODO: USART 2X
	
	// 115200 @ 2Mhz
	#define BAUD 115200
	#include <util/setbaud.h>
	_uart.BAUDCTRLA = UBRRL_VALUE;
	_uart.BAUDCTRLB = UBRRH_VALUE;
    
	_uart.CTRLB |= USART_TXEN_bm | USART_RXEN_bm;
}

/*
uint8_t one_wire::read_bit(void)
{
	// Return 1 if the value received matches the value sent.
	// Return 0 else. (A slave held the bus low).
	return (touch_bit(OW_UART_READ_BIT) == OWI_UART_READ_BIT);
}
*/

/*
 * Detects presents
 */
uint8_t one_wire::detect_presents(void)
{
	// Reset RX receiver
	_uart.CTRLB &= ~(USART_RXEN_bm);
	_uart.CTRLB |= USART_RXEN_bm;
    
	// 9600 baud @ 2Mhz
	#define BAUD 9600
	#include <util/setbaud.h>
	_uart.BAUDCTRLA = UBRRL_VALUE;
	_uart.BAUDCTRLB = UBRRH_VALUE;
    
    // Return 0 if the value received matches the value sent.
    // return 1 else. (Presence detected)
    uint8_t res = touch_bit(OW_UART_RESET) != OW_UART_RESET;
	
	// set 115200 baudrate
	#define BAUD 115200
	#include <util/setbaud.h>
	_uart.BAUDCTRLA = UBRRL_VALUE;
	_uart.BAUDCTRLB = UBRRH_VALUE;
	
	return res;
}

/*
 * Read and write byte through the USART.
 */
uint8_t one_wire::touch_bit(uint8_t value)
{
	_uart.DATA = value;
	
	do {
		// do nothing
	} while ((_uart.STATUS & USART_DREIF_bm) == 0) ;
	
	return _uart.DATA;
}
