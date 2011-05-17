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
 * Authorization request
 */
typedef struct _smh_auth {
	uint8_t login[6]; // ethernet address / login
	uint8_t password[10];
} smh_auth;

/*
 * ACS Service authentication response
 */
typedef struct _smh_auth_resp {
	uint8_t svc_password[10];
} smh_auth_resp ;

/*
 * Sensors data
 */
typedef struct _smh_sensor {
	uint8_t hot;
	uint8_t inner;
	uint8_t humid;
	uint8_t digit;
} smh_sensor ;

/*
 * Action request
 */
typedef struct _smh_pkt {
	uint8_t type;
	union {
		smh_auth auth;
		smh_auth_resp auth_resp;
		smh_sensor sensor;
	} pkt ;
} smh_pkt ;

class udp_service
{
    private:
		struct {
			uint8_t state;
			uint8_t tries;
		} _state;
		
		smh_pkt _pkt;
		
        iface& _netif;
		
		settings& _settings;
		
		/*
		 * Member to store ASC Service mac address
		 */
		ether_addr_t _service_eth;
		
    public:
		udp_service(settings& __settings, iface& __netif) : _settings(__settings), _netif(__netif)
		{
		}
		
		void init(void);
		
		void iterate(void);
		
		void process_request(void);
};

#endif
