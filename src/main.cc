/*
 * Smoke House Controlling device firmware main file
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 * GPL v3
 */
#include <stdio.h>
#include <avr/io.h>
#include <util/delay.h>
#include "uart_stdio.h"
#include "spi.h"
#include "net.h"
#include "iface.h"
#include "enc28j60.h"
#include "udp_service.h"


/*
 * Pure virtual function error handler
 */
extern "C" void __cxa_pure_virtual()
{
	printf("FATAL: Failed to invoke pure virtual function");
	do {} while (1) ;
}

// ACS Service ethernet and ip addresses
// const ether_addr_t service_mac = {0x00, 0x1f, 0x5b, 0xeb, 0x7f, 0x75};
// static ip_addr_t service_ip = {192, 168, 55, 1};

// Device ethernet and ip addresses
const static ether_addr_t device_mac = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06};

const static ip_addr_t device_ip = {192, 168, 55, 2};


int main(void)
{
	// Stabilization. This line will help much on schematic SC errors.
	_delay_ms(1000);
	
	uart_init();

	printf("init ");
	_delay_ms(500);
    
    // Initialize spi master driver for enc28j60 module
    spi spi(&SPIE,
            SPI_PRESCALER_DIV4_gc | SPI_MODE_0_gc,
            &PORTE, _BV(7),
            &PORTE, _BV(5),
            &PORTE, &(PORTE.PIN4CTRL), _BV(4));
	    
    enc28j60 iface(spi, device_mac, device_ip);
	
    if (!iface.is_supported()) {
		printf("FATAL: Unsupported ENC28J60 device.\n");
		do { } while (true);
    }
    
    iface.init();
	
    udp_service udp(iface);
	
	do {
		printf("main ");
		udp.send_to(NULL, NULL, 8081, NULL, 0);
		_delay_ms(1000);
	} while (true) ;
	
	return 0;
}

