/*
 * Smoke House Controlling application protocol.
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
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

#ifndef _udp_service_h_
#define _udp_service_h_

/*
 * Maximum count of authentication packets device will try to send
 * before switching to the do_nothing fall-back.
 */
#ifndef UDP_SVC_MAX_AUTH_TRIES
#define UDP_SVC_MAX_AUTH_TRIES 100
#endif

#ifndef UDP_SVC_MAX_ARP_TRIES
#define UDP_SVC_MAX_ARP_TRIES 100
#endif

#ifndef UDP_SVC_MAX_AUTH_RESPONSE_TRIES
#define UDP_SVC_MAX_AUTH_RESPONSE_TRIES 100
#endif

/*
 * Authentication token
 */
typedef struct _auth_token_t {
    ip_addr_t ip;
    uint8_t password[DEV_PASSWORD_LENGTH];
    uint16_t id;
} auth_token_t;

/*
 * Sensors data
 */
typedef struct _pkt_sensor_t {
	uint8_t hot;
	uint8_t inner;
	uint8_t humid;
	uint8_t digit;
} pkt_sensor_t ;

/*
 * Action request
 */
typedef struct _pkt_t {
    uint8_t type;
    uint8_t status;
    auth_token_t auth_token;
    union {
        pkt_sensor_t sensor;
    } pkt ;
} pkt_t ;

#define PKT_SIZE_BASE (sizeof(auth_token_t) + 2)


#define PKT_TYPE_AUTH 0x01
#define PKT_TYPE_AUTH_RESPONSE 0x02

/*
 * Authentication response status
 */
#define PKT_STATUS_OK     200
#define PKT_STATUS_FAILED 0
#define PKT_STATUS_DENIED 201

class udp_service
{
    private:
		struct {
			uint8_t state;
			uint8_t tries;
		} _state;
		
        iface& _netif;
		
		settings& _settings;
        
        uint16_t _device_id;
		
		/*
		 * Member to store ASC Service mac address
		 */
		ether_addr_t _service_eth;
        
        void decrypt_token(auth_token_t * token);
        
        void encrypt_token(uint16_t id, auth_token_t * out_token);
        
        void do_init_state(void);
        
        void do_authentication_state(void);
        
        void do_authentication_response_state(uint8_t& received);
		
    public:
		udp_service(settings& __settings, iface& __netif) :
            _settings(__settings),
            _netif(__netif)
		{
		}
		
		void init(void);
		
		void iterate(void);
		
		void process_request(void);
};

#endif

