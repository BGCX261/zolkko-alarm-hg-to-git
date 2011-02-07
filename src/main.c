#define F_CPU 25000000U
/**
 * TODO: I should not use F_CPU flag, because of switching
 * from OSC duiring runtime.
 */
#define F_CPU 32000000U

#include <avr/io.h> 
#include <util/delay.h> 
#include "rtc_if.h"


void osc_init(void);
void osc_stop(void);

/**
 * As far as I use internal 32MHz OSC I should remove it from circuit.
 *
 * At the very beginning of application 2MHz clock is selected
 * Using 32MHz clock as main source.
 */
void osc_init(void)
{
    // TODO: Switch to internal 32MHz ring osc
    OSC.PLLCTRL = (uint_8)((OSC_PLLFAC_gm & 16) << OSC_PLLFAC_gp | OSC_PLLSRC_RC2M_gc);
    CLK.PSCTRL = CLK_PSADIV_1_gc | CLK_PSBCDIV_1_1_gc;
    OSC.CTRL |= OSC_PLLEN_bm;
    do {} while (OSC.Status & OSC_PLLRDY_bm == 0);
    
    CLKSYS_PLL_Config(OSC_PLLSRC_RC2M_gc, 30);
    CLKSYS_Enable(OSC_PLLEN_bm);
    CLKSYS_Prescalers_Config( CLK_PSADIV_1_gc, CLK_PSBCDIV_1_2_gc );
    do {} while ( CLKSYS_IsReady( OSC_PLLRDY_bm ) == 0 );
    CLKSYS_Main_ClockSource_Select( CLK_SCLKSEL_PLL_gc );
    CLKSYS_Disable( OSC_XOSCEN_bm );
}

/**
 * TODO: Use internal 32kHz as a main source.
 * Switch back to the 32MHz on external interrupt.
 * On clock changes & (to redraw the screen).
 */
void osc_stop(void)
{
    do {} while ( CLKSYS_IsReady( OSC_RC2MRDY_bm ) == 0 );
    CLKSYS_Main_ClockSource_Select( CLK_SCLKSEL_RC2M_gc );
    CLKSYS_Disable(OSC_PLLEN_bm);
}


int main(void)
{
    // TODO: Initialize external osc.
    rtc_init();
    return 0;
}

