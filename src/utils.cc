/**
 * (c) copyright Alex Anisimov <zolkko@gmail.com>
 * GPL v3
 */

#include <avr/io.h>
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

inline void _delay_operations(uint32_t operations)
{
    uint32_t times = operations / 4;
    
    if (times > 1) {
        if (times > 0xffff) {
            PORTD.OUTTGL = _BV(5);
            while (times >= 0xffff) {
                _delay_loop_2(0xffff);
                times -= 0xffff;
            }
        }
        
        if (times != 0) {
            PORTD.OUTTGL = _BV(6);
            _delay_loop_2((uint16_t)(times & 0xffff));
        }
    }
}

