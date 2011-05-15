/*
 * Common network interface structures and types
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

#ifndef _NET_H_
#define _NET_H_

#include <inttypes.h>

#define uint16_to_n(X) (((X & 0x00ff) << 8) | ((X & 0xff00) >> 8))

#define n_to_uint16(X) (((X & 0x00ff) << 8) | ((X & 0xff00) >> 8))

#define set_mac(mac, x0, x1, x2, x3, x4, x5) mac[0] = x0; mac[1] = x1; mac[2] = x2; mac[3] = x3; mac[4] = x4; mac[5] = x5

#define set_ip(mac, x0, x1, x2, x3) ip[0] = x0; ip[1] = x1; ip[2] = x2; ip[3] = x3

#define assign_mac(mac1, mac2) mac1[0] = mac2[0]; mac1[1] = mac2[1]; mac1[2] = mac2[2]; mac1[3] = mac2[3]; mac1[4] = mac2[4]; mac1[5] = mac2[5]

#define assign_ip(ip1, ip2) ip1[0] = ip2[0]; ip1[1] = ip2[1]; ip1[2] = ip2[2]; ip1[3] = ip2[3]

#define equal_ip(ip1, ip2) (ip1[0] == ip2[0] && ip1[1] == ip2[1] && ip1[2] == ip2[2] && ip1[3] == ip2[3])

#define CRC32_LEN 4

/*
 * Ethernet address
 */

#define IF_ETHER_ADDR_LEN 6

typedef uint8_t ether_addr_t[IF_ETHER_ADDR_LEN];

/*
 * IP address
 */

#define IF_IP_ADDR_LEN 4

typedef uint8_t ip_addr_t[IF_IP_ADDR_LEN];

/*
 * Ethernet frame
 *
 * Statically allocate
 */

 // 576 byte minimum IP packet size + 16 byte ether header size + 4byte CRC size
#define IF_PAYLOAD_MAX 600

typedef struct _ether_frame_t {
    uint8_t preamble[7];
    uint8_t start_of_frame;
    ether_addr_t dst_mac;
    ether_addr_t src_mac;
    uint16_t len_or_type;
    uint8_t payload[IF_PAYLOAD_MAX];
    // CRC32 should be located at the actual end of payload data
} ether_frame_t;

/*
 * Ethernet packet types
 */
#define ETHER_TYPE_ARP 0x0608 // Network byte order
#define ETHER_TYPE_IP  0x0008 // Network byte order

/*
 * ARP request header
 */
typedef struct _arp_hdr_t {
    uint16_t hardware_type;
    uint16_t proto_type;
    uint8_t  hlen;
    uint8_t  plen;
    uint16_t operation;
    ether_addr_t src_mac;
    ip_addr_t src_ip; 
    ether_addr_t dst_mac;
    ip_addr_t dst_ip;
} arp_hdr_t;


#define ARP_HARDWARE_TYPE_ETHERNET 0x0001

#define ARP_PROTO_TYPE_ARP_IPV4    0x0800

#define ARP_OPERATION_ACK   0x0001

#define ARP_OPERATION_ANS   0x0002

/*
 * IP Header
 */

#define IP_HDR_LEN 20 

typedef struct _ip_hdr_t {
    uint8_t   version_len;
    uint8_t   tos; // type of service
    uint16_t  len;
    uint16_t  id;
    uint16_t  offset; // High Byte: Reserved(15) Dont Fragment(14), More fragments(13)
    uint8_t   ttl;
    uint8_t   proto;
    uint16_t  hdr_crc;
    ip_addr_t src_addr;
    ip_addr_t dst_addr;
	uint8_t   data[IF_PAYLOAD_MAX - IP_HDR_LEN]; // TODO: data block should be 576 bytes or greater.
} ip_hdr_t;

/*
 * IP protocol version
 */
#define IP_PROTO_ICMP_V 1
#define IP_PROTO_TCP_V 6
#define IP_PROTO_UDP_V 0x11


/*
 * UDP protocol header
 */
#define UDP_HDR_LEN 8

typedef struct _udp_hdr_t {
    uint16_t src_port;
    uint16_t dst_port;
    uint16_t ulen;
    uint16_t crc;
	uint8_t  data[IF_PAYLOAD_MAX - IP_HDR_LEN - UDP_HDR_LEN];
} udp_hdr_t;

#endif

