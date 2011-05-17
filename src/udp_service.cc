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
#include "utils.h"
#include "net.h"
#include "net_driver.h"
#include "iface.h"
#include "settings.h"
#include "udp_service.h"


#define UDP_DATA_LEN(x) (n_to_uint16(x.ulen) - UDP_HDR_LEN)
#define PTR_UDP_DATA_LEN(x) (n_to_uint16(x->ulen) - UDP_HDR_LEN)

/*
 * Internal udp service states
 */
#define UDP_SVC_STATE_INIT 0x01
#define UDP_SVC_STATE_AUTH 0x02
#define UDP_SVC_STATE_AUTH_RESPONSE 0x03
#define UDP_SVC_STATE_MAIN 0x04
#define UDP_SVC_STATE_DO_NOTHING 0x10

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
        printf("UDP_SVC_STATE_INIT.\r\n");
#endif
        do_init_state();
	} else if (_state.state == UDP_SVC_STATE_AUTH) {
#ifdef UART_DEBUG
        printf("UDP_SVC_STATE_AUTH.\r\n");
#endif
        do_authentication_state();
	} else if (_state.state == UDP_SVC_STATE_AUTH_RESPONSE) {
#ifdef UART_DEBUG
		printf("UDP_SVC_STATE_AUTH_RESPONSE.\r\n");
#endif
        do_authentication_response_state(received);
	} else if (_state.state == UDP_SVC_STATE_MAIN) {
        //
	} else if (_state.state == UDP_SVC_STATE_DO_NOTHING) {
        // Do nothing
    }
}

/*
 * populate ad encrypt authentication token
 */
void udp_service::encrypt_token(uint16_t id, auth_token_t * out_token)
{
    uint8_t (&secret_key)[AES_BLOCK_LENGTH] = (uint8_t(&)[AES_BLOCK_LENGTH]) _settings.get_secret_key();
    uint8_t (&device_pwd)[DEV_PASSWORD_LENGTH] = (uint8_t(&)[DEV_PASSWORD_LENGTH]) _settings.get_device_password();
    ip_addr_t& src_ip = _settings.get_device_ip();
    
    auth_token_t token;
    assign_ip(token.ip, src_ip);
    memcpy(token.password, device_pwd, DEV_PASSWORD_LENGTH);
    token.id = id;
    
    aes_encrypt((const uint8_t *) secret_key, (const uint8_t *) &token, (uint8_t *) out_token);
}

/*
 * decrypt authentication token
 */
void udp_service::decrypt_token(auth_token_t * token)
{
    uint8_t (&secret_key)[AES_BLOCK_LENGTH] = (uint8_t(&)[AES_BLOCK_LENGTH]) _settings.get_secret_key();
    
    auth_token_t tmp;
    aes_decrypt(&secret_key[0], (const uint8_t *)token, (uint8_t *)&tmp);
    
    memcpy(token, &tmp, sizeof(auth_token_t));
}

/*
 * Initialization state.
 * Sends ARP "Who has" request, await for response.
 */
void udp_service::do_init_state(void)
{
    ip_addr_t& service_ip = _settings.get_service_ip();
    
    if (_netif.resolve_ip(service_ip, _service_eth)) {
#ifdef UART_DEBUG
        printf("ACS Service IP %u.%u.%u.%u resolved into ethernet address %x:%x:%x:%x:%x:%x.\r\n",
                service_ip[0], service_ip[1], service_ip[2], service_ip[3],
                _service_eth[0], _service_eth[1], _service_eth[2], _service_eth[3], _service_eth[4], _service_eth[5]);
#endif
        _state.state = UDP_SVC_STATE_AUTH;
        _state.tries = 0;
    } else {
#ifdef UART_DEBUG
        printf("Failed to resolve ASC service ethernet address.\r\n");
#endif
        _state.state = UDP_SVC_STATE_INIT;
        _state.tries++;
        
        if (_state.tries >= UDP_SVC_MAX_ARP_TRIES) {
            _state.state = UDP_SVC_STATE_DO_NOTHING;
            _state.tries = 0;
        }
    }
}

/*
 * authentication request state
 */
void udp_service::do_authentication_state(void)
{
    pkt_t pkt;
    pkt.type = PKT_TYPE_AUTH;
    pkt.status = PKT_STATUS_OK;
    encrypt_token(0x0000, &pkt.auth_token);
    
    // sending packet into the interface
    ip_addr_t& dst_ip = _settings.get_service_ip();
    if (!_netif.send_udp(_service_eth, dst_ip, _settings.get_service_port(),
                _settings.get_device_port(), (uint8_t *) &pkt, PKT_SIZE_BASE))
    {
        _state.tries++;
        if (_state.tries >= UDP_SVC_MAX_AUTH_TRIES) {
            _state.state = UDP_SVC_STATE_DO_NOTHING;
            _state.tries = 0;
        }
    } else {
        _state.state = UDP_SVC_STATE_AUTH_RESPONSE;
        _state.tries = 0;
    }
}

/*
 * authentication response state
 */
void udp_service::do_authentication_response_state(uint8_t& received)
{
    if (!received) {
        if (_netif.has_packet()) {
            _netif.receive();
            received = true;
        }
    }
    
    if (received && _netif.is_udp()) {
        ether_frame_t * eth_frm;
        udp_hdr_t * udp_hdr;
        ip_hdr_t * ip_hdr;
        
        if (_netif.udp_read(&eth_frm, &ip_hdr, &udp_hdr)) {
            pkt_t& in_pkt = (pkt_t&) udp_hdr->data;
            
            if (in_pkt.type == PKT_TYPE_AUTH_RESPONSE) {
                if (in_pkt.status == PKT_STATUS_OK) {
                    decrypt_token(&in_pkt.auth_token);
                    
                    uint8_t (&device_pwd)[DEV_PASSWORD_LENGTH] = (uint8_t(&)[DEV_PASSWORD_LENGTH]) _settings.get_device_password();
                    if (equal_ip(ip_hdr->src_addr, in_pkt.auth_token.ip) &&
                        memcmp(in_pkt.auth_token.password, device_pwd, DEV_PASSWORD_LENGTH) == 0)
                    {
#ifdef UART_DEBUG
                        printf("Successfully authenticated. Device ID %u.\r\n", in_pkt.auth_token.id);
#endif
                        _device_id = in_pkt.auth_token.id;
                        
                        _state.state = UDP_SVC_STATE_MAIN;
                        _state.tries = 0;
                    } else {
#ifdef UAT_DEBUG
                        printf("Invalid authentication token received.\r\n");
#endif
                        _state.state = UDP_SVC_STATE_AUTH;
                        _state.tries = 0;
                    }
                } else if (in_pkt.status == PKT_STATUS_FAILED) {
#ifdef UART_DEBUG
                    printf("Authentication failed.\r\n");
#endif
                    _state.state = UDP_SVC_STATE_AUTH;
                    _state.tries = 0;
                } else if (in_pkt.status == PKT_STATUS_DENIED) {
#ifdef UART_DEBUG
                    printf("Authentication denied.\r\n");
#endif
                    _state.state = UDP_SVC_STATE_DO_NOTHING;
                    _state.tries = 0;
                } else {
#ifdef UART_DEBUG
                    printf("Unexpcted authentication response status %u.\r\n", in_pkt.status);
#endif
                    _state.state = UDP_SVC_STATE_AUTH;
                    _state.tries = 0;
                }
                return;
            }
#ifdef UART_DEBUG
            else {
                printf("Unexpected packet type received.\r\n");
            }
#endif
        }
#ifdef UART_DEBUG
        else {
            printf("Failed to read UDP packet.\r\n");
        }
#endif
    }
#ifdef UART_DEBUG
    else {
        printf("Unexpected ethernet packet type.\r\n");
    }
#endif
    
    _state.tries++;
    if (_state.tries >= UDP_SVC_MAX_AUTH_RESPONSE_TRIES) {
#ifdef UART_DEBUG
        printf("Authentication response timeout exceeded.\r\n");
#endif
        _state.state = UDP_SVC_STATE_AUTH;
        _state.tries = 0;
    }
}

/*
 * UDP Service process request
 */
void udp_service::process_request(void)
{
}

