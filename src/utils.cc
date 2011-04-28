/**
 * (c) copyright Alex Anisimov <zolkko@gmail.com>
 * GPL v3
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
        sum += len - 8; //  - udp length
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

