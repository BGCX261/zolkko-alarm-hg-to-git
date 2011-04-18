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

#define IF_MAC_ADDR_LEN 6
#define IF_IP_ADDR_LEN 4

typedef uint8_t mac_addr_t[IF_MAC_ADDR_LEN];

typedef uint8_t ip_addr_t[IF_IP_ADDR_LEN];

class Iface
{
    private :
        mac_addr_t _macaddr;
        
        ip_addr_t _ip;
        
    public :
        Iface (const mac_addr_t macaddr, const ip_addr_t ip)
        {
            memcpy(_macaddr, macaddr, MAC_ADDR_LEN);
            memcpy(_ip, ip, IP_ADDR_LEN);
        }
        
        /**
         * Returns interface mac address
         */
        const mac_addr_t get_mac_addr(void)
        {
            return _macaddr;
        }
        
        /**
         * Returns interface ip address
         */
        const ip_addr_t get_ip_addr(void)
        {
            return _ip;
        }
        
        /**
         * Send network packet via interface
         */
        virtual void send_packet(uint16_t len, uint8_t * packet) = 0;
};

#endif

