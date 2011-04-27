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


class iface
{
    protected:
        const ether_addr_t& _macaddr;
        
        const ip_addr_t& _ip;
        
    public:
        iface (const ether_addr_t& __macaddr, const ip_addr_t& __ip) :
			_macaddr(__macaddr),
			_ip(__ip)
        {
        }

        virtual void init(void) = 0;
        
        /**
         * Returns interface mac address
         */
        const ether_addr_t& get_mac_addr(void)
        {
            return _macaddr;
        }
        
        /**
         * Returns interface ip address
         */
        const ip_addr_t& get_ip_addr(void)
        {
            return _ip;
        }
        
        /**
         * Send network packet via interface
         */
        virtual void send_packet(uint16_t len, uint8_t * packet) = 0;
};

#endif

