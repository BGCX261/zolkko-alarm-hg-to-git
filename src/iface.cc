/*
 * Network interface.
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 * GPLv3
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
#ifdef UART_DEBUG
	printf("Preparing PREAMBLE ethernet`s frame section.");
#endif
	
#ifdef DEBUG
    memset(&_buf, 0, sizeof(ether_frame_t));
#endif
	
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
#ifdef UART_DEBUG
	printf("Sending UDP packet (data length: %d).\r\n", data_len);
#endif
	
	make_preamble();
	
	assign_mac(_buf.dst_mac, dst_eth);
    assign_mac(_buf.src_mac, _mac);
#ifdef UART_DEBUG
	printf("Destination ethernet address %x:%x:%x:%x:%x:%x.\r\n", _buf.dst_mac[0], _buf.dst_mac[1], _buf.dst_mac[2], _buf.dst_mac[3], _buf.dst_mac[4], _buf.dst_mac[5]);
	printf("Source ethernet address %x:%x:%x:%x:%x:%x.\r\n", _buf.src_mac[0], _buf.src_mac[1], _buf.src_mac[2], _buf.src_mac[3], _buf.src_mac[4], _buf.src_mac[5]);
#endif
	
	uint16_t payload_len = data_len + IP_HDR_LEN + UDP_HDR_LEN;
#ifdef UART_DEBUG
	printf("Ethernet payload length: %d.\r\n", payload_len);
#endif
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
	
#ifdef UART_DEBUG
	printf("Source IP address %d.%d.%d.%d.\r\n", ip_hdr.src_addr[0], ip_hdr.src_addr[1], ip_hdr.src_addr[2], ip_hdr.src_addr[3]);
	printf("Destination IP address %d.%d.%d.%d.\r\n", ip_hdr.dst_addr[0], ip_hdr.dst_addr[1], ip_hdr.dst_addr[2], ip_hdr.dst_addr[3]);
#endif
	
	// Calculate IP header check sum
	uint16_t ip_crc = checksum((uint8_t *) &(ip_hdr.version_len), IP_HDR_LEN, 0);
#ifdef UART_DEBUG
	printf("IP header CRC %x.\r\n", ip_crc);
#endif
	ip_hdr.hdr_crc = uint16_to_n(ip_crc);
	
	udp_hdr_t& udp_hdr = (udp_hdr_t&) ip_hdr.data;
	udp_hdr.src_port = uint16_to_n(src_port);
	udp_hdr.dst_port = uint16_to_n(dst_port);
#ifdef UART_DEBUG
	printf("Source port in network byte order: %x.\r\n",  udp_hdr.src_port);
	printf("Destination port in network byte order: %x.\r\n", udp_hdr.dst_port);
#endif
	
	uint16_t udp_payload_len = data_len + UDP_HDR_LEN;
	udp_hdr.ulen = uint16_to_n(udp_payload_len);
#ifdef UART_DEBUG
	printf("UDP payload length %x.\r\n", udp_payload_len);
	printf("UDP payload length in network byte order %x.\r\n", udp_hdr.ulen);
#endif
	udp_hdr.crc = 0x0000; // TODO: calculate check summ
	
	// Populating UDP datagramm data
	uint8_t i;
#ifdef UART_DEBUG
	printf("-- [ UDP datagram ] ---------\r\n");
#endif
	for (i = 0; i < data_len; i++) {
		udp_hdr.data[i] = data[i];
#ifdef UART_DEBUG
		printf("0x%x ", udp_hdr.data[i]);
#endif
	}
#ifdef UART_DEBUG
	printf("\r\n-----------------------------\r\n");
#endif
	
	// UDP crc is optional for IPv4 and non optional for IPv6
	uint16_t udp_crc = checksum((uint8_t *) &(ip_hdr.src_addr[0]), data_len + 16, 1);
#ifdef UART_DEBUG
	printf("UDP CRC %x.\r\n", udp_crc);
#endif
	udp_hdr.crc = uint16_to_n(udp_crc);
	
	_driver.send(_buf);
	
	return true;
}

/*
 * Resolves ethernet address by IP by sending ARP request
 * and returns it in mac variable.
 */
uint8_t iface::resolve_ip(const ip_addr_t& ip, ether_addr_t& mac)
{
#ifdef UART_DEBUG
    printf("Resolving IP address %d.%d.%d.%d.\r\n", ip[0], ip[1], ip[2], ip[3]);
#endif
    
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
#ifdef DEBUG
		memset(&_buf, 0, sizeof(ether_frame_t));
#endif
		uint8_t tries = 0;
        while (!_driver.has_packet()) {
#ifdef UART_DEBUG
			printf("Wait for ARP answer.\r\n");
#endif
			tries++;
			if (tries == IFACE_RECEIVE_COUNT) {
#ifdef UART_DEBUG
				printf("ARP answer timeout.\r\n");
#endif
				return false;
			}
            _delay_ms(IFACE_RECEIVE_TIMEOUT);
        }
		
#ifdef UART_DEBUG
		printf("Data received. Reading ENC28J60 driver`s buffer.\r\n");
#endif
        _driver.receive(_buf);
		
        if (_buf.len_or_type == ETHER_TYPE_ARP) {
            arp_hdr_t& arp_ans = (arp_hdr_t&) _buf.payload;
#ifdef UART_DEBUG
			printf("ETHER_TYPE_ARP packet received.\r\nOperation: %d.\r\n", n_to_uint16(arp_ans.operation));
#endif
			
            if (n_to_uint16(arp_ans.operation) == ARP_OPERATION_ANS &&
				equal_ip(arp_ans.dst_ip, _ip) &&
				equal_ip(arp_ans.src_ip, ip))
            {
				
                assign_mac(mac, arp_ans.src_mac);
#ifdef UART_DEBUG
                printf("IP address %d.%d.%d.%d has been resolved into ethernet address %x:%x:%x:%x:%x:%x.\r\n", ip[0], ip[1], ip[2], ip[3], mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
#endif
                return true;
            }
#ifdef UART_DEBUG
			else {
				printf("Unknown ETHERNET packet received.\r\n");
			}
#endif
        }
#ifdef UART_DEBUG
		else {
			printf("Unknown packet type received %x.\r\n", _buf.len_or_type);
		}
#endif
    } while (true);
	
    return false;
}
