
#include <avr/io.h>
#include "usart_d.h"

#define USART_Rx_Enable(_usart) ((_usart)->CTRLB |= USART_RXEN_bm)

#define USART_Rx_Disable(_usart) ((_usart)->CTRLB &= ~USART_RXEN_bm)

#define USART_Tx_Enable(_usart)	((_usart)->CTRLB |= USART_TXEN_bm)

#define USART_Tx_Disable(_usart) ((_usart)->CTRLB &= ~USART_TXEN_bm)

#define USART_PutChar(_usart, _data) ((_usart)->DATA = _data)

#define USART_IsTXDataRegisterEmpty(_usart) (((_usart)->STATUS & USART_DREIF_bm) != 0)

/*
 * Enable only transive
 */
void usart_init(USART_t * usart, PORT_t * port)
{
    // Port direction
	port->DIRSET = _BV(3);
	// port->.DIRCLR = _BV(2);
    
    // 8N1
	usart->CTRLC = (uint8_t) USART_CHSIZE_8BIT_gc | USART_PMODE_DISABLED_gc;
    
    // 9600 baud @ 2Mhz
	usart->BAUDCTRLA = 12;
	usart->BAUDCTRLB = 0;
    
	// USART_Rx_Enable(usart);
	USART_Tx_Enable(usart);
}

inline void usart_char(USART_t * usart, const char chr)
{
    do {
        // Do nothing
    } while (!USART_IsTXDataRegisterEmpty(usart));
    USART_PutChar(usart, chr);
}

void usart_string(USART_t * usart, const char * str)
{
    while (!(*str != '\0')) {
        usart_char(usart, *str);
        str++;
    }
}

