/*
 * Smoke House Controlling device firmware main file
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 * GPLv3
 */

#define UART_DEBUG 1

#include <avr/io.h>
#include <util/delay.h>

#ifdef UART_DEBUG
#include <stdio.h>
#include "uart_stdio.h"
#endif

#include "spi.h"
#include "net.h"
#include "net_driver.h"
#include "enc28j60.h"
#include "iface.h"


/*
 * Pure virtual function error handler
 */
extern "C" void __cxa_pure_virtual()
{
#ifdef UART_DEBUG
	printf("FATAL: Failed to invoke pure virtual function.\n");
#endif
	do {} while (1) ;
}

// Device ethernet addresses
const static ether_addr_t device_mac = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06};

// Device IP address
const static ip_addr_t device_ip = {192, 168, 55, 2};

// Service mac address
static ether_addr_t service_mac = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

// ACS Service IP address
const static ip_addr_t service_ip = {192, 168, 55, 1};


int main(void)
{
	// Stabilization. This line will help much on schematic SC errors.
	_delay_ms(1000);
	
#ifdef UART_DEBUG
	uart_init();
	printf("\r\n\r\n=================================\r\nUART debugging module has been initialized.\r\n=================================\r\n");
#endif
    
    // Initialize spi master driver for enc28j60 module
    spi spi(&SPIE,
            SPI_PRESCALER_DIV4_gc | SPI_MODE_0_gc,
            &PORTE, _BV(7),
            &PORTE, _BV(5),
            &PORTE, &(PORTE.PIN4CTRL), _BV(4));
    
    enc28j60 drv(spi);
    if (!drv.is_supported()) {
#ifdef UART_DEBUG
        printf("Unable to find ENC28J60 IC connected.\r\n");
#endif
        do {} while (true) ;
        
        return 0;
    }
#ifdef UART_DEBUG
	else {
		printf("ENC28J60 Revision B7 chip found.\r\n");
	}
#endif
    
    iface netif(drv, device_mac, device_ip);
    netif.init();
	
	do {
#ifdef UART_DEBUG
		printf("Resolving ACS Service IP address.\r\n");
#endif
		if (netif.resolve_ip(service_ip, service_mac)) {
#ifdef UART_DEBUG
			printf("ACS Service IP resolved.\r\n");
#endif
			break;
		}
#ifdef UART_DEBUG
		else {
			printf("Failed to resolve ACS Service ethernet address.\r\n");
		}
#endif
	} while (true) ;
	
	uint8_t udp_packet_data[11];
	udp_packet_data[0] = 'H';
	udp_packet_data[1] = 'e';
	udp_packet_data[2] = 'l';
	udp_packet_data[3] = 'l';
	udp_packet_data[4] = 'o';
	udp_packet_data[5] = 'w';
	udp_packet_data[6] = ' ';
	udp_packet_data[7] = 'a';
	udp_packet_data[8] = 'l';
	udp_packet_data[9] = 'l';
	udp_packet_data[10] = '!';
	
#ifdef UART_DEBUG
	printf("Sending UDP datagramm.\r\n");
#endif
	if (netif.send_udp(service_mac, service_ip, 9091, 9092, (uint8_t *) udp_packet_data, 11)) {
#ifdef UART_DEBUG
		printf("UDP datagramm has sended.\r\n");
#endif
	}
#ifdef UART_DEBUG
	else {
		printf("Failed to send UDP datagramm.\r\n");
	}
#endif
	
#ifdef UART_DEBUG
	printf("Entering into main loop.\r\n");
#endif
	
    do {
		// TODO: main loop
    } while (true) ;
	
	return 0;
}
