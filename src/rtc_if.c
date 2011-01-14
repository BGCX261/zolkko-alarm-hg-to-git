/**
 * Clock interface
 */
#include "rtc_if.h"

/**
 * Turn on internal 32kHz osc
 */
void rtc_init(void)
{
    OSC.CTRL |= OSC_RC32KEN_bm;
    
    do { 
        // Wait until internal 32kHz OSC ready
    } while ((OSC.STATUS & OSC_RC32KRDY_bm) == 0);
    
    // RTC is a clock source for RTC
    CLK.RTCCTRL = CLK_RTCSRC_RCOSC_gc | CLK_RTCEN_bm;
    
    // RTC prescaller
    RTC.CNT  = 0;
    RTC.COMP = 0; // ???
    RTC.PER  = 0; // ???
    RTC.CTRL = (RTC.CTRL & ~RTC_PRESCALER_gm) | RTC_PRESCALER_DIV1_gc;

    // Setting RTC Interrupt overflow level
    // TODO: RTC_OFVINTLVL_gm | RTC_COMPINTLVL_gm
    RTC.INTCTRL = (RTC.INTCTRL & ~RTC_OVFINTLVL_gm) | 1;
    RTC.INTCTRL = (RTC.INTCTRL & ~RTC_COMPINTLVL_gm) | 0; // Do not use this kind of interupt
}

