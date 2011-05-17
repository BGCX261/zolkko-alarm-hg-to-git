/*
 * Smoke House Controlling application protocol.
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
#include <string.h>
#include <util/delay.h>
#include "net.h"
#include "net_driver.h"
#include "iface.h"
#include "settings.h"
#include "udp_service.h"


/*
 * Internal udp service states
 */
#define UDP_SVC_STATE_INIT 0x01
#define UDP_SVC_STATE_AUTH 0x02
#define UDP_SVC_STATE_AUTH_RESPONSE 0x03
#define UDP_SVC_STATE_MAIN 0x04

/*
 * Initialize UDP service
 */
void udp_service::init(void)
{
	_state.state = UDP_SVC_STATE_INIT;
	_state.tries = 0;
}

/*
 * Next step of UDP service
 */
void udp_service::iterate(void)
{
	uint8_t received = false;
	
	// Check ARP Requst packets on each interation
	if (_netif.has_packet()) {
		_netif.receive();
		received = true;
		if (_netif.is_arp()) {
#ifdef UART_DEBUG
			printf("ARP Request packet reseived.\r\n");
#endif
			received = false;
			_netif.arp_response();
		}
	}
	
	if (_state.state == UDP_SVC_STATE_INIT) {
#ifdef UART_DEBUG
		printf("UDP Service state UDP_SVC_STATE_INIT.\r\n");
#endif
		ip_addr_t& service_ip = _settings.get_service_ip();
		if (_netif.resolve_ip(service_ip, _service_eth)) {
#ifdef UART_DEBUG
			
			printf("ACS Service IP %u.%u.%u.%u resolved into ethernet address %x:%x:%x:%x:%x:%x.\r\n",
				service_ip[0], service_ip[1], service_ip[2], service_ip[3],
				_service_eth[0], _service_eth[1], _service_eth[2], _service_eth[3], _service_eth[4], _service_eth[5]);
#endif
			_state.state = UDP_SVC_STATE_AUTH;
		} else {
#ifdef UART_DEBUG
			printf("Failed to resolve ASC service ethernet address.\r\n");
#endif
			_state.state = UDP_SVC_STATE_INIT;
		}
	} else if (_state.state == UDP_SVC_STATE_AUTH) {
#ifdef UART_DEBUG
		printf("UDP service state UDP_SVC_STATE_AUTH.\r\n");
#endif
		_state.state = UDP_SVC_STATE_AUTH_RESPONSE;
	} else if (_state.state == UDP_SVC_STATE_AUTH_RESPONSE) {
#ifdef UART_DEBUG
		printf("UDP service state UDP_SVC_STATE_AUTH_RESPONSE.\r\n");
#endif
		/*if (!received) {
			if (_netif.has_packet()) {
				_netif.receive();
				received = true;
			}
		}
		
		if (received && _netif.is_udp()) {
			_state.state = UDP_SVC_STATE_MAIN;
		}*/
	} else if (_state.state == UDP_SVC_STATE_MAIN) {
		/*if (!received) {
			if (_netif.has_packet()) {
				_netif.receive();
				received = true;
			}
		}
		
		if (received && _netif.is_udp()) {
			process_request();
		}*/
	}
}

/*
 * UDP Service process request
 */
void udp_service::process_request(void)
{
}
