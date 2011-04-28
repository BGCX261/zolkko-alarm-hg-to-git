/*
 * Network point interface.
 *
 * Copyright (c) 2011 Alex Aisimov, <zolkko@gmail.com>
 * GPLv3
 */
#ifndef _IFACE_H_
#define _IFACE_H_

class iface
{
    private:
        net_driver& _driver;
        
        ether_frame_t _buf;
        
        const ether_addr_t& _mac;
        
        const ip_addr_t& _ip;
    
    public:
        iface (net_driver& __driver,
               const ether_addr_t& __mac,
               const ip_addr_t& __ip) :
            _driver(__driver),
            _mac(__mac),
            _ip(__ip)
        {
        }
        
        void init(void);
        
        /*
         * Method resolve MAC address by IP address
         */
        uint8_t resolve_ip(const ip_addr_t& ip, ether_addr_t& mac);
};

#endif

