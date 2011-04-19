/**
 * (c) copyright Alex Anisimov <zolkko@gmail.com>
 * GPL v3
 */
#include <avr/io.h>
#include <util/delay_basic.h>
#include "utils.h"

inline uint32_t get_cpu_freq(void)
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

inline void xdelay_ms(const double ms)
{
    uint32_t cpu_freq = get_cpu_freq();
    uint16_t ticks;
    double tmp = ((cpu_freq) / 4e3) * ms;
    
    if (tmp < 1.0) {
        ticks = 1;
    } else if (tmp > 65535) {
        ticks = (uint16_t) (ms * 10.0);
        while(ticks) {
            _delay_loop_2(((cpu_freq) / 4e3) / 10);
            ticks --;
        }
        return;
    } else {
        ticks = (uint16_t)tmp;
    }
	_delay_loop_2(ticks);
}

inline void xdelay_us(const double us)
{
    uint32_t cpu_freq = get_cpu_freq();
    
    uint8_t ticks;
    double tmp = ((cpu_freq) / 3e6) * us;
    if (tmp < 1.0) {
        ticks = 1;
    } else if (tmp > 255) {
        xdelay_ms(us / 1000.0);
        return;
    } else {
        ticks = (uint8_t)tmp;
    }
	_delay_loop_1(ticks);
}

