/*
 * Network point interface.
 *
 * Copyright (c) 2011 Alex Aisimov, <zolkko@gmail.com>
 * GPLv3
 */
#ifndef _IFACE_H_
#define _IFACE_H_

#define IFACE_RECEIVE_TIMEOUT 500
#define IFACE_RECEIVE_COUNT 5

#define IFACE_TTL 64

class iface
{
    private:
        net_driver& _driver;
        
        ether_frame_t _buf;
        
        const ether_addr_t& _mac;
        
        const ip_addr_t& _ip;
		
		uint16_t _ip_identifier;
		
		void make_preamble(void);
		
    public:
        iface (net_driver& __driver,
               const ether_addr_t& __mac,
               const ip_addr_t& __ip) :
            _driver(__driver),
            _mac(__mac),
            _ip(__ip)
        {
			_ip_identifier = 0x0000;
        }
        
        void init(void);
        
        /*
         * Method resolve MAC address by IP address
         */
        uint8_t resolve_ip(const ip_addr_t& ip, ether_addr_t& mac);
		
		/*
		 * Sends UDP datagramm
		 */
		uint8_t send_udp(const ether_addr_t& dst_eth,
		                 const ip_addr_t& dst_ip,
						 const uint16_t dst_port,
						 const uint16_t src_port,
						 uint8_t * data, uint8_t data_len);
};

#endif

