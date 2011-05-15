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

ether_addr_t& StaticSettings::get_local_mac(void)
{
	return (ether_addr_t&) device_mac;
}

ip_addr_t& StaticSettings::get_local_addr(void)
{
	return (ip_addr_t&) device_ip;
}

ip_addr_t& StaticSettings::get_service_addr(void)
{
	return (ip_addr_t&) service_ip;
}

uint16_t StaticSettings::get_service_port(void)
{
	return 9091;
}

uint16_t StaticSettings::get_local_port(void)
{
	return 9092;
}
