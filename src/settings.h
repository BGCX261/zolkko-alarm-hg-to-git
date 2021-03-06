/*
 * Abstract settings
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
 
#ifndef _SETTINGS_H_
#define _SETTINGS_H_

#ifndef AES_BLOCK_LENGTH
#define AES_BLOCK_LENGTH
#endif

#ifndef DEV_LOGIN_LENGTH
#define DEV_LOGIN_LENGTH 6
#endif

#ifndef DEV_PASSWORD_LENGTH
#define DEV_PASSWORD_LENGTH 10
#endif

class settings
{
	private:
		
	public:
		settings()
		{
		}
        
        /*
         * Returns AES secret key
         */
        virtual const uint8_t (&get_secret_key(void))[AES_BLOCK_LENGTH] = 0;
        
        /*
         * Returns device login
         */
        virtual const uint8_t (&get_device_login(void))[DEV_LOGIN_LENGTH] = 0;
        
        /*
         * Returns device password
         */
        virtual const uint8_t (&get_device_password(void))[DEV_PASSWORD_LENGTH] = 0;
		
		/*
		 * Returns ACS 
		 */
		virtual ether_addr_t& get_device_eth(void) = 0;
		
		/*
		 * Returns ACS 
		 */
		virtual ip_addr_t& get_device_ip(void) = 0;
		
		/*
		 * Returns ACS service ip address
		 */
		virtual ip_addr_t& get_service_ip(void) = 0;
		
		/*
		 * ACS service port
		 */
		virtual uint16_t get_service_port(void) = 0;
		
		/*
		 * ACS local port
		 */
		virtual uint16_t get_device_port(void) = 0;
 };
 
 #endif
 
