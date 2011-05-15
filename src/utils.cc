/*
 * Utility functions.
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

#include <avr/io.h>
#include "net.h"
#include "utils.h"


uint32_t get_cpu_freq(void)
{
    uint32_t clk = 0;
    
    if (CLK.CTRL & CLK_SCLKSEL_RC32K_gc) {
        clk = 32000UL;    // 32KHz
    } else if (CLK.CTRL & CLK_SCLKSEL_RC32M_gc){
        clk = 32000000UL; // 32MHz
	} else if (CLK_SCLKSEL_RC2M_gc == (CLK.CTRL & CLK_SCLKSEL_gm)) {
        clk = 2000000UL;  // 2MHz
	} else if (CLK.CTRL & CLK_SCLKSEL_PLL_gc) {
		uint8_t factor = ((OSC.PLLCTRL & OSC_PLLFAC_gm) >> OSC_PLLFAC_gp);
        
		if (OSC.PLLCTRL & OSC_PLLSRC_RC2M_gc) {
            clk = 2000000UL * factor;
        } else if (OSC.PLLCTRL & OSC_PLLSRC_RC32M_gc) {
            clk = 32000000UL * factor;
        } else {
            clk = F_CPU * factor;
        }
	} else if (CLK.CTRL & CLK_SCLKSEL_XOSC_gc) {
		clk = (uint32_t)F_CPU;
	}
    
    return clk;
}

uint16_t checksum(uint8_t * buf, uint16_t len, uint8_t type)
{
    uint32_t sum = 0;
    
    if (type == 1) {
        sum += IP_PROTO_UDP_V;
        sum += len - UDP_HDR_LEN; // - udp length
    } else if (type == 2) {
        sum += IP_PROTO_TCP_V; 
        sum += len - 8; // - tcp length
    }
    
    while (len > 1) {
        sum += 0xffff & (*buf << 8 | *(buf + 1));
        buf += 2;
        len -= 2;
    }
    
    if (len) {
        sum += (0xff & *buf) << 8;
    }
    
    while (sum >> 16) {
        sum = (sum & 0xffff) + (sum >> 16);
    }
    
    return ((uint16_t) sum ^ 0xffff);
}

