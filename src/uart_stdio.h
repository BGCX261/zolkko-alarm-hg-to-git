/*
 * Interface to printing string via UART
 * atxmega128a3 @ 2MHz 9600 8N1
 *
 * Copyright (C) 2011, Alex Anisimov <zolkko@gmail.com>
 * GPLv3
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
