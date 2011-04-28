/*
 * Abstract network driver
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 */
#ifndef _net_driver_h_
#define _net_driver_h_

#ifndef NET_DRIVER_RECEIVE_TIMEOUT
#define NET_DRIVER_RECEIVE_TIMEOUT 20
#endif

class net_driver
{
    public:
        net_driver()
        {
        }
        
        virtual void init(const ether_addr_t& mac) = 0;
        
        virtual uint8_t send(ether_frame_t& frame) = 0;
        
        virtual uint8_t receive(ether_frame_t& frame) = 0;
        
        virtual uint8_t is_supported(void) = 0;
        
        virtual uint8_t has_packet(void) = 0;
};

#endif

