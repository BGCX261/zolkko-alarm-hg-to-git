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
    RTC.CTRL = (RTC.CTRL & ~RTC_PRESCALER_gm) | RTC_PRESCALER_DIV1_gc;
}

