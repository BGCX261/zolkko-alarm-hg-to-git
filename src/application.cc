
#ifdef UART_DEBUG
#include <stdio.h>
#include "uart_stdio.h"
#endif
#include <avr/io.h>
#include <inttypes.h>
#include "application.h"


void Application::init_rtc(void)
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
    RTC.INTCTRL = (RTC.INTCTRL & ~RTC_OVFINTLVL_gm)  | 1;
    RTC.INTCTRL = (RTC.INTCTRL & ~RTC_COMPINTLVL_gm) | 0; // Do not use this kind of interupt
}

void Application::run(void)
{
    //
    // Initialize all infrastructure
    //
    
    init_rtc();
    
    do {
        // Do nothing
    } while (true) ;
}

