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
#include <stdio.h>
#include <avr/io.h>
#include "uart_stdio.h"


static int uart_putchar(char, FILE *);

static FILE _uart_stdout;

/*
 * Prints character 'c' into UART module
 */
static int uart_putchar(char c, FILE * stream)
{
	do { } while ((UART_DEV.STATUS & USART_DREIF_bm) == 0);
	
	UART_DEV.DATA = c;
    
	return 0;
}

/*
 * Initialize uart
 */
void uart_init()
{
	UART_TX_PORT.DIRSET = _BV(UART_TX_PIN);
    
    // 8N1
	UART_DEV.CTRLC = (uint8_t) USART_CHSIZE_8BIT_gc | USART_PMODE_DISABLED_gc;
    
    // 9600 baud @ 2Mhz
	UART_DEV.BAUDCTRLA = 12;
	UART_DEV.BAUDCTRLB = 0;
    
	UART_DEV.CTRLB |= USART_TXEN_bm;
	
	// Reassign standart output to the UART
	_uart_stdout.put = uart_putchar;
	_uart_stdout.get = NULL;
	_uart_stdout.flags = _FDEV_SETUP_WRITE;
	_uart_stdout.udata = 0;
	
	stdout = &_uart_stdout;
}
