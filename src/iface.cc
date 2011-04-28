/*
 * Network interface.
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 * GPLv3
 */

#ifdef UART_DEBUG
#include <stdio.h>
#include "uart_stdio.h"
#endif

#include <string.h>
#include <util/delay.h>
#include "net.h"
#include "net_driver.h"
#include "iface.h"

void iface::init(void)
{
    _driver.init(_mac);
}

/*
 * Resolves ethernet address by IP by sending ARP request
 * and returns it in mac variable.
 */
uint8_t iface::resolve_ip(const ip_addr_t& ip, ether_addr_t& mac)
{
#ifdef UART_DEBUG
    printf("Resolving IP address %d.%d.%d.%d\n", ip[0], ip[1], ip[2], ip[3]);
#endif
    
#ifdef DEBUG
    memset(&_buf, 0, sizeof(ether_frame_t));
#endif
    // Network driver have to set preable itself.
    // _buf.preamble[0] = 0xaa;
    // _buf.preamble[1] = 0xaa;
    // _buf.preamble[2] = 0xaa;
    // _buf.preamble[3] = 0xaa;
    // _buf.preamble[4] = 0xaa;
    // _buf.preamble[5] = 0xaa;
    // _buf.preamble[6] = 0xaa;
    // _buf.start_of_frame = 0xab;
    set_mac(_buf.dst_mac, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff); // broad-cast address
    assign_mac(_buf.src_mac, _mac);
    _buf.len_or_type = ETHER_TYPE_ARP;
    
    // ARP request
    arp_hdr_t& arp = (arp_hdr_t&) _buf.payload;
    arp.hardware_type = uint16_to_n(1);
    arp.proto_type = uint16_to_n(1);
    arp.hlen = IF_ETHER_ADDR_LEN;
    arp.plen = IF_IP_ADDR_LEN;
    arp.operation = 1;
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
    
    // TODO: add timeout
    do {
#ifdef DEBUG
        memset(&_buf, 0, sizeof(ether_frame_t));
#endif
        do {
            _delay_ms(NET_DRIVER_RECEIVE_TIMEOUT);
        } while (!_driver.has_packet()) ;
        _driver.receive(_buf);
        
        if (_buf.len_or_type == ETHER_TYPE_ARP) {
            arp_hdr_t& arp_ans = (arp_hdr_t&) _buf.payload;
            
            if (arp_ans.operation = 2 && equal_ip(arp_ans.dst_ip, _ip) &&
                    equal_ip(arp_ans.src_ip, ip))
            {
                assign_mac(mac, arp_ans.src_mac);
#ifdef UART_DEBUG
                printf("IP address %d.%d.%d.%d is resolved into ethernet address %x:%x:%x:%x:%x:%x.\n", ip[0], ip[1], ip[2], ip[3], mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
#endif
                return true;
            }
        }
    } while (true);
    
    return false;
}

