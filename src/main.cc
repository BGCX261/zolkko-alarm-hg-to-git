/*
 * Smoke House Controlling device firmware main file
 *
 * Copyright (c) 2011 Alex Anisimov aka lx, <zolkko@gmail.com>
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
			printf("ACS Service IP %u.%u.%u.%u resolved into ethernet address %x:%x:%x:%x:%x:%x.\r\n",
				service_ip[0], service_ip[1], service_ip[2], service_ip[3],
				service_mac[0], service_mac[1], service_mac[2], service_mac[3], service_mac[4], service_mac[5]);
#endif
			break;
		}
#ifdef UART_DEBUG
		else {
			printf("Failed to resolve ACS Service ethernet address.\r\n");
		}
#endif
	} while (true) ;
	
	uint8_t udp_packet_data[11] = "Hellow all";
	
	if (netif.send_udp(service_mac, service_ip, 9091, 9092, (uint8_t *) udp_packet_data, 11)) {
#ifdef UART_DEBUG
		printf("UDP datagramm has been sent.\r\n");
#endif
	}
#ifdef UART_DEBUG
	else {
		printf("Failed to sent UDP datagramm.\r\n");
	}
#endif
	
#ifdef UART_DEBUG
	printf("Entering into main loop.\r\n");
#endif
	
    do {
		if (netif.has_packet()) {
			netif.receive();
			
			if (netif.is_arp()) {
#ifdef UART_DEBUG
				printf("ARP request recevied.\r\n");
#endif
				
#ifdef UART_DEBUG
				if (!netif.arp_response()) {
					printf("Failed to sent arp response.\r\n");
				} else {
					printf("Arp response has been sent.\r\n");
				}
#else
				netif.arp_response();
#endif
			} else if (netif.is_udp()) {	
#ifdef UART_DEBUG
				ip_hdr_t * ip_hdr = NULL;
				udp_hdr_t * udp_hdr = NULL;
				ether_frame_t * ether_hdr = NULL;
				
				if (!netif.udp_read(&ether_hdr, &ip_hdr, &udp_hdr)) {
					printf("Invalid UDP packet received.\r\n");
				} else {
					printf("UDP datagramm from udp://%u.%u.%u.%u:%u (%x:%x:%x:%x:%x:%x) to port %u received. Data: [",
						ip_hdr->src_addr[0], ip_hdr->src_addr[1], ip_hdr->src_addr[2], ip_hdr->src_addr[3],
						n_to_uint16(udp_hdr->src_port),
						ether_hdr->src_mac[0], ether_hdr->src_mac[1], ether_hdr->src_mac[2], ether_hdr->src_mac[3], ether_hdr->src_mac[4], ether_hdr->src_mac[5],
						n_to_uint16(udp_hdr->dst_port));
					
					uint8_t dg_len = n_to_uint16(udp_hdr->ulen) - UDP_HDR_LEN;
					uint8_t j;
					for (j = 0; j < dg_len; j++) {
						printf("%c", udp_hdr->data[j]);
					}
					printf("]\r\n");
				}
#else
				netif.udp_read(NULL, NULL);
#endif
			}
#ifdef UART_DEBUG
			else {
				printf("Unknown packet type received.\r\n");
			}
#endif
		}
    } while (true) ;
	
	return 0;
}
