/*
 * Smoke House Controlling device firmware main file
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

#include <stddef.h>
#include <avr/io.h>
#include <util/delay.h>
#include <avr\pgmspace.h>

#ifdef UART_DEBUG
#include <stdio.h>
#include "uart_stdio.h"
#endif

#include "spi.h"
#include "net.h"
#include "net_driver.h"
#include "onewire.h"
#include "sensor.h"
#include "ds18b20.h"
#include "enc28j60.h"
#include "iface.h"
#include "settings.h"
#include "static_settings.h"
#include "udp_service.h"
#include <util/atomic.h>
#include "utils.h"


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

uint8_t ReadCalibrationByte( uint8_t index )
{
	uint8_t result;
	
	/* Load the NVM Command register to read the calibration row. */
	NVM_CMD = NVM_CMD_READ_CALIB_ROW_gc;
	result = pgm_read_byte(index);
	
	/* Clean up NVM Command register. */
	NVM_CMD = NVM_CMD_NO_OPERATION_gc;
	
	return (result);
}

int main(void)
{
	// Stabilization. This line will help much on schematic SC errors.
	_delay_ms(1000);
	
	PORTE.DIRSET = _BV(0);
	PORTE.DIRSET = _BV(1);
	
	enable_32mhz();
	
#ifdef UART_DEBUG
	uart_init();
	printf("\r\n\r\n=================================\r\nUART debugging module has been initialized.\r\n=================================\r\n");
#endif
	
	// configure PORTA as input
	PORTA.DIR = 0x00;
	PORTA.OUT = 0x00;
	
	// Enable ADC and flush pipeline
	ADCA.CALL = ReadCalibrationByte(offsetof(NVM_PROD_SIGNATURES_t, ADCACAL0));
	ADCA.CALH = ReadCalibrationByte(offsetof(NVM_PROD_SIGNATURES_t, ADCACAL1));
	
	ADCA.CTRLA = ADC_ENABLE_bm | ADC_FLUSH_bm;
	
	// 12 bit conversion
	ADCA.CTRLB = ADC_RESOLUTION_12BIT_gc;
	
	// internal 1V bandgap reference
	ADCA.REFCTRL = ADC_REFSEL_INT1V_gc | ADC_BANDGAP_bm;
	
	// peripheral clk/8 (32MHz / 512 = 62.5kHz)
	ADCA.PRESCALER = ADC_PRESCALER_DIV512_gc;
	
	// single ended
	ADCA.CH0.CTRL = ADC_CH_INPUTMODE_SINGLEENDED_gc;
	ADCA.CH1.CTRL = ADC_CH_INPUTMODE_SINGLEENDED_gc;
	
	ADCA.CH0.MUXCTRL = ADC_CH_MUXPOS_PIN0_gc;
	ADCA.CH1.MUXCTRL = ADC_CH_MUXPOS_PIN1_gc;
	
	do {
		ADCA.CTRLA |= ADC_CH0START_bm | ADC_CH1START_bm;
		//ADCA.CH0.CTRL |= ADC_CH_START_bm;
		//ADCA.CH1.CTRL |= ADC_CH_START_bm;
		
		// do {} while(!ADCA.CH0.INTFLAGS) ;
		// do {} while(!ADCA.CH1.INTFLAGS) ;
		do {} while (!(ADCA.INTFLAGS & (ADC_CH0IF_bm | ADC_CH1IF_bm))) ;
		
		uint16_t result0 = ADCA.CH0.RES;
		printf("Reference\t%d\r\n", result0);
		
		uint16_t result1 = ADCA.CH1.RES;
		printf("Ground\t\t%d\r\n", result1);
		
		uint16_t volRng = result0 - result1;
		
		double v = volRng / 4096.0;
		printf("Voltage\t\t%0.4f\r\n", v);
		
		_delay_ms(10000);
	} while (true) ;
	
	/*
	// testing DS18B20 1-wire connection protocol
	one_wire ow(USARTF0, PORTF, 3, 2);
	ow.init();
	
#ifdef UART_DEBUG
	if (ow.reset()) {
		ow.read_rom();
		uint8_t (&rom)[OW_ROM_LENGTH] = (uint8_t (&)[OW_ROM_LENGTH])ow.get_rom();
		
		printf("1-wire ROM has been read: ");
		for (uint8_t i = 0; i < OW_ROM_LENGTH; i++) {
			printf("0x%x ", rom[i]);
		}
		printf("\r\n");
	} else {
		printf("Failed  to reset 1-wire.\r\n");
	}
#endif
	
	if (ow.reset()) {
		ow.skip_rom();
	} else {
		printf("Failed to reset 1-wire BUS.\r\n");
	}
	
	ds18b20 sen(ow);
	sen.init();
	
	if (sen.read()) {
#ifdef UART_DEBUG
		printf("Temperature %0.2f.\r\n", sen.get_value());
#endif
	}
#ifdef UART_DEBUG
	else {
		printf("Failed to read temperature sensor.\r\n");
	}
#endif
	
	static_settings settings;
    
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
    
    iface netif(drv, settings.get_device_eth(), settings.get_device_ip());
    netif.init();
	
	udp_service udpsvc(settings, netif);
	udpsvc.init();
	
	do {
		udpsvc.iterate();
	} while (true);
	*/
	/*
	do {
#ifdef UART_DEBUG
		printf("Resolving ACS Service IP address.\r\n");
#endif
		if (netif.resolve_ip(service_ip, service_mac)) {
#ifdef UART_DEBUG

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
	*/
	return 0;
}
