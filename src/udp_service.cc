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
#include "uart_debug.h"
#endif

#include <avr/io.h>
#include <string.h>
#include <util/delay.h>
#include "net.h"
#include "net_driver.h"
#include "iface.h"
#include "udp_service.h"

/*
 * Initialize UDP service
 */
void udp_service::init(void)
{
	_state.state = UDP_SVC_STATE_START;
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
			received = false;
			_netif.arp_response();
		}
	}
	
	if (_state.state == UDP_SVC_STATE_START) {
		if (_netif.resolve_ip(service_ip, service_mac)) {
			_state.state = UDP_SVC_STATE_AUTH;
		}
	} else if (_state.state == UDP_SVC_STATE_AUTH) {
		_state.state = UDP_SVC_STATE_AUTH_RESPONSE;
	} else if (_state.state == UDP_SVC_STATE_AUTH_RESPONSE) {
		if (!received) {
			if (_netif.has_packet()) {
				_netif.receive();
				received = true;
			}
		}
		
		if (received && _netif.is_udp()) {
			_state.state = UDP_SVC_STATE_MAIN;
		}
	} else if (_state.state == UDP_SVC_STATE_MAIN) {
		if (!received) {
			if (_netif.has_packet()) {
				_netif.receive();
				received = true;
			}
		}
		
		if (received && _netif.is_udp()) {
			process_request();
		}
	}
}

/*
 * UDP Service process request
 */
void udp_service::process_request(void)
{
}
