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
#include "utils.h"

/*
 * Initialize 1-wire
 */
void one_wire::init(void)
{
    _uart_port.DIRSET = _BV(_uart_tx_pin);
	_uart_port.OUTSET = _BV(_uart_tx_pin);
	_uart_port.DIRCLR = _BV(_uart_rx_pin);
	
	_uart.CTRLB = USART_TXEN_bm | USART_RXEN_bm;
	_uart.CTRLC = (uint8_t) USART_CHSIZE_8BIT_gc | USART_PMODE_DISABLED_gc; // 8n1
	
	_uart.BAUDCTRLB = BAUD_115200_B;
	_uart.BAUDCTRLA = BAUD_115200_A;
}

#ifdef UART_DEBUG
void one_wire::test(void)
{
	uint8_t data = 0;
	uint8_t result = 0;
	while (data < 0xff) {
		result = touch_bit(data);
		printf("send %x receive %x.\r\n", data, result);
		data++;
	}
}
#endif

/*
 * Detects presents
 */
uint8_t one_wire::reset(void)
{
	// Reset RX receiver
	_uart.CTRLB &= ~(USART_RXEN_bm);
	_uart.CTRLB |= USART_RXEN_bm;
    
	_uart.BAUDCTRLB = BAUD_9600_B;
	_uart.BAUDCTRLA = BAUD_9600_A;
    
    // Return 0 if the value received matches the value sent.
    // return 1 else. (Presence detected)
    uint8_t res = touch_bit(OW_UART_RESET) != OW_UART_RESET;
	
	// set 115200 baudrate
	_uart.BAUDCTRLB = BAUD_115200_B;
	_uart.BAUDCTRLA = BAUD_115200_A;
	
	return res;
}

/*
 * Read and write byte through the USART.
 *
 * TODO: normal timeouts
 */
uint8_t one_wire::touch_bit(uint8_t value)
{
	// It is not needed for now.
	_uart.CTRLB = 0;
	_uart.CTRLB = USART_TXEN_bm | USART_RXEN_bm;
	
	_uart.DATA = value;
	
	OW_TIMEOUT_DATA_TYPE timeout = 0;
	do {
		timeout++;
	} while (timeout < OW_TIMEOUT_VALUE && !(_uart.STATUS & USART_DREIF_bm)) ;
	
	if (timeout == OW_TIMEOUT_VALUE) {
#ifdef UART_DEBUG
		printf("1-wire transmition timeout.\r\n");
#endif
		return 0;
	}
	
	timeout = 0;
	do {
		timeout++;
	} while (timeout < OW_TIMEOUT_VALUE && !(_uart.STATUS & USART_RXCIF_bm));
	
	if (timeout == OW_TIMEOUT_VALUE) {
#ifdef UART_DEBUG
		printf("1-wire reception timeout.\r\n");
#endif
		return 0;
	}
	
	return _uart.DATA;
}

/*
 * Returns received byte
 */ 
uint8_t one_wire::receive(void)
{
    uint8_t result = 0x00;
    for (uint8_t i = 0; i < 8; i++) {
        result >>= 1;
		uint8_t bit = touch_bit(OW_UART_READ_BIT);
        if (bit == OW_UART_READ_BIT) {
		
            result |= 0x80;
        }
    }
	return result;
}

/*
 * send one byte into the bus
 */
void one_wire::send(uint8_t value)
{
    uint8_t mask = 0x01;
    do {
        if (value & mask) {
            touch_bit(OW_UART_WRITE1);
        } else {
            touch_bit(OW_UART_WRITE0);
        }
		mask <<= 1;
    } while (mask);
}

void one_wire::read_rom(void)
{
	send(OW_ROM_READ);
	for (uint8_t i = 0; i < OW_ROM_LENGTH; i++) {
		_rom[i] = receive();
	}
}

void one_wire::match_rom(void)
{
    send(OW_ROM_MATCH);
    for (uint8_t i = 0; i < OW_ROM_LENGTH; i++) {
        send(_rom[i]);
    }
}

void one_wire::skip_rom(void)
{
    send(OW_ROM_SKIP);
}

void one_wire::alarm_search(void)
{
	send(OW_ROM_ALARM);
}
