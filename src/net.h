/*
 * Common network interface structures and types
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 */

#ifndef _NET_H_
#define _NET_H_

#include <inttypes.h>


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
typedef struct _ether_frame {
    uint8_t preamble[7];
    uint8_t start_of_frame;
    ether_addr_t dst_mac;
    ehter_addr_t src_mac;
    uint8_t length;
    uint8_t payload[IF_PAYLOAD_MAX];
};

/*
 * IP Header
 */

typedef struct _ip_hdr
{
    uint8_t   version_len;
    uint8_t   tos; // type of service
    uint16_t  len;
    uint16_t  id;
    uint16_t  offset; // High Byte: Reserved(15) Dont Fragment(14), More fragments(13)
    uint8_t   ttl;
    uint8_t   proto;
    uint16_t  sum;
    ip_addr_t src_addr;
    ip_addr_t dst_addr;
} ip_hdr;

/*
 * UDP protocol header
 */
typedef struct _udp_hdr
{
    uint16_t src_port;
    uint16_t dst_port;
    uint16_t ulen;
    uint16_t sum;
} udp_hdr;

#endif

