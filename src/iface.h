/**
 * (c) copyright Alex Aisimov <zolkko@gmail.com>
 * GPL v3
 *
 * Abstract inet interface
 * Concrete driver should implement this
 */
#ifndef _IFACE_H_
#define _IFACE_H_

#include <inttypes.h>
#include <string.h>
#include "net.h"


class Iface
{
    protected:
        // TODO: change to references
        ether_addr_t _macaddr;
        
        ip_addr_t _ip;
        
    public:
        Iface (const ether_addr_t * macaddr, const ip_addr_t * ip)
        {
            memcpy(&_macaddr, macaddr, IF_ETHER_ADDR_LEN);
            memcpy(&_ip, ip, IF_IP_ADDR_LEN);
        }

        virtual void init(void) = 0;
        
        /**
         * Returns interface mac address
         */
        const ether_addr_t * get_mac_addr(void)
        {
            return &_macaddr;
        }
        
        /**
         * Returns interface ip address
         */
        const ip_addr_t * get_ip_addr(void)
        {
            return &_ip;
        }
        
        /**
         * Send network packet via interface
         */
        virtual void send_packet(uint16_t len, uint8_t * packet) = 0;
};

#endif

