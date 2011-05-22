/*
 * Testing 1-wire BUS through atxmega USART module.
 * atxmega_tx (3.3v) -> 10K -> 2n7002 + 2.7KOhm -> 2n7002 + 2.7KOhm -> atxmega_rx (3.3v)
 */

#include <avr/io.h>
#include <util/delay.h>
#include "utils.h"

int main(void)
{
	enable_32mhz();
	
	PORTF.DIRSET = 0xff;
	PORTF.DIRSET = _BV(3);
	PORTF.OUTSET = _BV(3);
	PORTF.DIRCLR = _BV(2);
	printf("PORTF.DIR=%x, PORTF.OUT=%x, PORTF.IN=%x.\r\n", PORTF.DIR, PORTF.OUT, PORTF.IN);
	
	// 8N1
	USARTF0.CTRLB = USART_TXEN_bm | USART_RXEN_bm;
	USARTF0.CTRLC = (uint8_t) USART_CHSIZE_8BIT_gc | USART_PMODE_DISABLED_gc;
	
	USARTF0.BAUDCTRLB = BAUD_115200_B;
	USARTF0.BAUDCTRLA = BAUD_115200_A;
	
	uint32_t err_count = 0;
	uint8_t data = 0x00;
	while (true) {
		USARTF0.CTRLB = 0;
		USARTF0.CTRLB = USART_TXEN_bm | USART_RXEN_bm;
		USARTF0.DATA = data;
		do { } while ((USARTF0.STATUS & USART_DREIF_bm) == 0) ;
		
		uint32_t i = 0;
		do {
			i++;
			_delay_ms(1);
		} while (i < 0xffff && !(USARTF0.STATUS & USART_RXCIF_bm));
		
		uint8_t result = USARTF0.DATA;
		printf("%02x = %02x\t\t", data, result);
		if (data == result) {
			printf("OK\t\t%u\r\n", i);
		} else {
			err_count++;
			printf("failed\t\t%i\r\n", err_count);
		}
		data++;
		
		_delay_ms(5000);
	}
	return 0;
}