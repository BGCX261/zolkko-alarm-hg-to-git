/*
 * Network point interface.
 *
 * Copyright (c) 2011 Alex Anisimov aka lx, <zolkko@gmail.com>
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

#ifndef _IFACE_H_
#define _IFACE_H_

/*
 * Packet receiving timeout in miliseconds
 */
#define IFACE_RECEIVE_TIMEOUT 500

/*
 * Packet receiving retries.
 */
#define IFACE_RECEIVE_COUNT 5

/*
 * Default outgoing TTL
 */
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
		 * Checks packet
		 */
		uint8_t has_packet(void);
        
        /*
         * Method resolve MAC address by IP address
         */
        uint8_t resolve_ip(const ip_addr_t& ip, ether_addr_t& mac);
		
		/*
		 * Checks if last readed packet is an ARP packet.
		 */
		uint8_t is_arp(void);
		
		/*
		 * Answer to the arp request (readed into the buffer).
		 */
		uint8_t arp_response(void);
		
		/*
		 * Checks if it is and UDP packet.
		 */
		uint8_t is_udp(void);
		
		/*
		 * Returns pointer to the UDP datagramm data in the network buffer.
		 */
		uint8_t udp_read(ether_frame_t ** __ether_hdr, ip_hdr_t ** __ip_hdr, udp_hdr_t ** __udp_hdr);
		
		/*
		 * Reads received frame into buffer
		 */
		uint8_t receive(void);
		
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

