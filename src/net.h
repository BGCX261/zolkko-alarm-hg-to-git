/**
 * Common network interface structures and types
 */

#ifndef _NET_H_
#define _NET_H_

#include <inttypes.h>


/*
 * Ethernet structures
 */

#define IF_ETHER_ADDR_LEN 6

typedef uint8_t ether_addr_t[IF_ETHER_ADDR_LEN];

/*
 * IP address
 */

#define IF_IP_ADDR_LEN 4

typedef uint8_t ip_addr_t[IF_IP_ADDR_LEN];

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

