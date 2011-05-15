/*
 * Static settings implementation
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

#ifndef _STATIC_SETTINGS_H_
#define _STATIC_SETTINGS_H_

class StaticSettings : public Settings
{		
	public :
		StaticSettings()
		{
		}
		
		/*
		 * Returns ACS 
		 */
		ether_addr_t& get_local_mac(void);
		
		/*
		 * Returns ACS 
		 */
		ip_addr_t& get_local_addr(void);
		
		/*
		 * Returns ACS service ip address
		 */
		ip_addr_t& get_service_addr(void);
		
		/*
		 * ACS service port
		 */
		uint16_t get_service_port(void);
		
		/*
		 * ACS local port
		 */
		uint16_t get_local_port(void);
};

#endif
