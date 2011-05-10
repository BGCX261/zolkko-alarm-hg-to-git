/*
 * Network interface.
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

#ifdef UART_DEBUG
#include <stdio.h>
#include "uart_stdio.h"
#endif

#include <string.h>
#include <util/delay.h>
#include "net.h"
#include "net_driver.h"
#include "iface.h"
#include "utils.h"

void iface::init(void)
{
    _driver.init(_mac);
}

/*
 * Prepares preamble section
 */
void iface::make_preamble(void)
{
    // Network driver have to set preable itself.
	_buf.preamble[0] = 0xaa;
	_buf.preamble[1] = 0xaa;
	_buf.preamble[2] = 0xaa;
	_buf.preamble[3] = 0xaa;
	_buf.preamble[4] = 0xaa;
	_buf.preamble[5] = 0xaa;
	_buf.preamble[6] = 0xaa;
	_buf.start_of_frame = 0xab;
}

/*
 * Sends UDP packet
 */
uint8_t iface::send_udp(const ether_addr_t& dst_eth, const ip_addr_t& dst_ip, const uint16_t dst_port, const uint16_t src_port, uint8_t * data, uint8_t data_len)
{
	make_preamble();
	
	assign_mac(_buf.dst_mac, dst_eth);
    assign_mac(_buf.src_mac, _mac);
	
	uint16_t payload_len = data_len + IP_HDR_LEN + UDP_HDR_LEN;
	// ISSUE: _buf.len_or_type = uint16_to_n(payload_len);
	//
	// This should be an ETHERNET TYPE II packet.
	// Otherwise it would be recognized as an LLC packet.
	//
	_buf.len_or_type = ETHER_TYPE_IP;
	
	ip_hdr_t& ip_hdr = (ip_hdr_t&) _buf.payload;
	
    ip_hdr.version_len = 0x45; // Version: (IPv4:4 << 4 | IHL:5)
    ip_hdr.tos = 0x00; // type of service
    ip_hdr.len = uint16_to_n(payload_len);
	
    ip_hdr.id = uint16_to_n(_ip_identifier);
	_ip_identifier++;
	
    ip_hdr.offset = 0x0040; // In network byte order; High Byte: Reserved(15) Dont Fragment(14) SET, More fragments(13) CLR
    ip_hdr.ttl = IFACE_TTL;
    ip_hdr.proto = IP_PROTO_UDP_V;
    ip_hdr.hdr_crc = 0x0000;
    assign_ip(ip_hdr.src_addr, _ip);
    assign_ip(ip_hdr.dst_addr, dst_ip);
	
	// Calculate IP header check sum
	uint16_t ip_crc = checksum((uint8_t *) &(ip_hdr.version_len), IP_HDR_LEN, 0);
	ip_hdr.hdr_crc = uint16_to_n(ip_crc);
	
	udp_hdr_t& udp_hdr = (udp_hdr_t&) ip_hdr.data;
	udp_hdr.src_port = uint16_to_n(src_port);
	udp_hdr.dst_port = uint16_to_n(dst_port);
	
	uint16_t udp_payload_len = data_len + UDP_HDR_LEN;
	udp_hdr.ulen = uint16_to_n(udp_payload_len);
	udp_hdr.crc = 0x0000;
	
	// Populating UDP datagramm data
	uint8_t i;
	for (i = 0; i < data_len; i++) {
		udp_hdr.data[i] = data[i];
	}
	
	// UDP crc is optional for IPv4 and non optional for IPv6
	uint16_t udp_crc = checksum((uint8_t *) &(ip_hdr.src_addr[0]), data_len + 16, 1);
	udp_hdr.crc = uint16_to_n(udp_crc);
	
	
#ifdef UART_DEBUG
	printf("Sending UDP packet (data length: %d) from udp://%u.%u.%u.%u:%u (%x:%x:%x:%x:%x:%x) to udp://%u.%u.%u.%u:%u (%x:%x:%x:%x:%x:%x).\r\n",
		data_len,
		ip_hdr.src_addr[0], ip_hdr.src_addr[1], ip_hdr.src_addr[2], ip_hdr.src_addr[3],
		n_to_uint16(udp_hdr.src_port),
		_buf.src_mac[0], _buf.src_mac[1], _buf.src_mac[2], _buf.src_mac[3], _buf.src_mac[4], _buf.src_mac[5],
		
		ip_hdr.dst_addr[0], ip_hdr.dst_addr[1], ip_hdr.dst_addr[2], ip_hdr.dst_addr[3],
		n_to_uint16(udp_hdr.dst_port),
		_buf.dst_mac[0], _buf.dst_mac[1], _buf.dst_mac[2], _buf.dst_mac[3], _buf.dst_mac[4], _buf.dst_mac[5]);
#endif
	
	// Sending data
	_driver.send(_buf);
	
	return true;
}

/*
 * Returns udp packet (if applicable)
 */
uint8_t iface::receive(void)
{
	if (!_driver.has_packet()) {
		return false;
	} else {
		_driver.receive(_buf);
		return true;
	}
}

/*
 * Checks if it is an arp request or not.
 *
 * TODO: remove operation check
 */
uint8_t iface::is_arp(void)
{
	if (_buf.len_or_type != ETHER_TYPE_ARP) {
		return false;
	}
	
	arp_hdr_t& arp_req = (arp_hdr_t&) _buf.payload;
	if (n_to_uint16(arp_req.hardware_type) != ARP_HARDWARE_TYPE_ETHERNET ||
		n_to_uint16(arp_req.proto_type) != ARP_PROTO_TYPE_ARP_IPV4 ||
		arp_req.hlen != IF_ETHER_ADDR_LEN ||
		arp_req.plen != IF_IP_ADDR_LEN ||
		n_to_uint16(arp_req.operation) != ARP_OPERATION_ACK ||
		!equal_ip(arp_req.dst_ip, _ip))
	{
		return false;
	}
	
	return true;
}

/*
 * ARP response
 */
uint8_t iface::arp_response(void) {
	if (!is_arp()) {
		return false;
	}
	
	// Fill ethernet frame fields
	assign_mac(_buf.dst_mac, _buf.src_mac);
	assign_mac(_buf.src_mac, _mac);
	
	// Fill arp response
	arp_hdr_t& arp = (arp_hdr_t&) _buf.payload;
	
	arp.operation = uint16_to_n(ARP_OPERATION_ANS);
	
    assign_mac(arp.dst_mac, arp.src_mac);
	assign_mac(arp.src_mac, _mac);
	
    assign_ip(arp.dst_ip, arp.src_ip);
	assign_ip(arp.src_ip, _ip);
	
	_buf.payload[sizeof(arp_hdr_t)]     = 0x00;
    _buf.payload[sizeof(arp_hdr_t) + 1] = 0x00;
    _buf.payload[sizeof(arp_hdr_t) + 2] = 0x00;
    _buf.payload[sizeof(arp_hdr_t) + 3] = 0x00;
	
	_driver.send(_buf);
	
	return true;
}

/*
 * Checks if received packet is an UDP packet
 */
uint8_t iface::is_udp(void)
{
	if (_buf.len_or_type != ETHER_TYPE_IP) {
		return false;
	}
	
	ip_hdr_t& ip_hdr = (ip_hdr_t&) _buf.payload;
	
	// TODO: Verify IP header CRC16
	if (ip_hdr.version_len != 0x45 ||
		ip_hdr.proto != IP_PROTO_UDP_V ||
		!equal_ip(ip_hdr.dst_addr, _ip))
	{
		return false;
	}
	
	return true;
}

/*
 * Returns pointer to the UDP data
 */
uint8_t iface::udp_read(ether_frame_t ** __ether_hdr, ip_hdr_t ** __ip_hdr, udp_hdr_t ** __udp_hdr)
{
	if (!is_udp()) {
		return false;
	}
	
	if (__ether_hdr != NULL) {
		*__ether_hdr = &_buf;
	}
	
	ip_hdr_t& ip_hdr = (ip_hdr_t&) _buf.payload;
	if (__ip_hdr != NULL) {
		*__ip_hdr = &ip_hdr;
	}
	
	udp_hdr_t& udp_hdr = (udp_hdr_t&) ip_hdr.data;
	if (__udp_hdr != NULL) {
		*__udp_hdr = &udp_hdr;
	}
	
	return true;
}

/*
 * Checks if interface has a packet.
 */
uint8_t iface::has_packet(void)
{
	return _driver.has_packet();
}

/*
 * Resolves ethernet address by IP by sending ARP request
 * and returns it in mac variable.
 *
 * TODO: split into two parts.
 */
uint8_t iface::resolve_ip(const ip_addr_t& ip, ether_addr_t& mac)
{
	make_preamble();
	
    set_mac(_buf.dst_mac, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff); // broad-cast address
    assign_mac(_buf.src_mac, _mac);
    _buf.len_or_type = ETHER_TYPE_ARP;
    
    // ARP request
    arp_hdr_t& arp = (arp_hdr_t&) _buf.payload;
    arp.hardware_type = uint16_to_n(ARP_HARDWARE_TYPE_ETHERNET);
    arp.proto_type = uint16_to_n(ARP_PROTO_TYPE_ARP_IPV4);
    arp.hlen = IF_ETHER_ADDR_LEN;
    arp.plen = IF_IP_ADDR_LEN;
    arp.operation = uint16_to_n(ARP_OPERATION_ACK);
    assign_mac(arp.src_mac, _mac);
    assign_ip(arp.src_ip, _ip);
    set_mac(arp.dst_mac, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff); // broad-cast address
    assign_ip(arp.dst_ip, ip);
    
    // Network driver have to calculate ethernet CRC32 itself.
    // Just reset ethernet payload CRC32 field (4 byte)
    _buf.payload[sizeof(arp_hdr_t)]     = 0x00;
    _buf.payload[sizeof(arp_hdr_t) + 1] = 0x00;
    _buf.payload[sizeof(arp_hdr_t) + 2] = 0x00;
    _buf.payload[sizeof(arp_hdr_t) + 3] = 0x00;
	
	_driver.send(_buf);
    
    // Receive will wait until an answer packet.
    // We do not have receiving big receiving buffer for later processing,
    // so just skip wrong packets.
    
    do {
		uint8_t tries = 0;
        while (!_driver.has_packet()) {
			tries++;
			if (tries == IFACE_RECEIVE_COUNT) {
#ifdef UART_DEBUG
				printf("ARP answer timeout.\r\n");
#endif
				return false;
			}
            _delay_ms(IFACE_RECEIVE_TIMEOUT);
        }
		
        _driver.receive(_buf);
		
        if (_buf.len_or_type == ETHER_TYPE_ARP) {
			
            arp_hdr_t& arp_ans = (arp_hdr_t&) _buf.payload;
			
            if (n_to_uint16(arp_ans.operation) == ARP_OPERATION_ANS &&
				equal_ip(arp_ans.dst_ip, _ip) &&
				equal_ip(arp_ans.src_ip, ip))
            {
                assign_mac(mac, arp_ans.src_mac);
#ifdef UART_DEBUG
                printf("Packet accepted. IP address resolved.\r\n");
#endif
                return true;
            }
#ifdef UART_DEBUG
			else {
				printf("Packet rejected. Invalid arp packet received.\r\n");
			}
#endif
        }
#ifdef UART_DEBUG
		else {
			printf("Packet rejected. Unexpected packet type %x received.\r\n", _buf.len_or_type);
		}
#endif
    } while (true);
	
    return false;
}
