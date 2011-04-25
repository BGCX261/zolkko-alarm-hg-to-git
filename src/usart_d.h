
#ifndef _USART_D_H_
#define _USART_D_H_

#ifndef udebug_usart
#define udebug_usart USARTD0
#endif

#ifndef udebug_port
#define udebug_port PORTD
#endif

#define udebug_init usart_init(udebug_usart, udebug_init)

#define udebug_s(str) usart_char(&udebug_usart, '\n'); usart_string(&udebug_usart, str)

#define udebug_c(chr) usart_char(&udebug_usart, chr)

extern void usart_init(USART_t *, PORT_t *);

extern void usart_char(USART_t *, const char);

extern void usart_string(USART_t *, const char *);

#endif

