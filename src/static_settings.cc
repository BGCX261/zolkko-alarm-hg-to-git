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

#include "net.h"
#include "settings.h"
#include "static_settings.h"

// Device ethernet addresses
const static ether_addr_t device_mac = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06};

// Device IP address
const static ip_addr_t device_ip = {192, 168, 55, 2};

// ACS Service IP address
const static ip_addr_t service_ip = {192, 168, 55, 1};


// Device login
const static uint8_t __device_login[DEV_LOGIN_LENGTH] = {'z', 'o', 'l', 'l', 'k', 'o'};

// Device password
const static uint8_t __device_password[DEV_PASSWORD_LENGTH] = {'^', 'p', 'a', 's', 's', 'w', 'o', 'r', 'd', '$'};

// AES secret key
const static uint8_t __secret_key[AES_BLOCK_LENGTH] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};


/*
 * returns AES secret key
 */
const uint8_t (&static_settings::get_secret_key(void))[AES_BLOCK_LENGTH]
{
    return (const uint8_t (&)[AES_BLOCK_LENGTH]) __secret_key;
}

/*
 * return device login
 */
const uint8_t (&static_settings::get_device_login(void))[DEV_LOGIN_LENGTH]
{
    return (uint8_t (&)[DEV_LOGIN_LENGTH]) __device_login;
}

/*
 * return device password
 */
const uint8_t (&static_settings::get_device_password(void))[DEV_PASSWORD_LENGTH]
{
    return (uint8_t (&)[DEV_PASSWORD_LENGTH]) __device_password;
}

ether_addr_t& static_settings::get_device_eth(void)
{
	return (ether_addr_t&) device_mac;
}

ip_addr_t& static_settings::get_device_ip(void)
{
	return (ip_addr_t&) device_ip;
}

ip_addr_t& static_settings::get_service_ip(void)
{
	return (ip_addr_t&) service_ip;
}

uint16_t static_settings::get_service_port(void)
{
	return 9091;
}

uint16_t static_settings::get_device_port(void)
{
	return 9092;
}

