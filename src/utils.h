/**
 * (C) copyright Alex Anisimov <zolkko@gmail.com>
 * GPL v3
 */
#ifndef _UTILS_H_
#define _UTILS_H_

#include <util/delay.h>
#include <util/delay_basic.h>

#ifdef __cplusplus
extern "C" {
#endif


/*
 * Returns CPU freq according to current
 * clock source.
 */
extern uint32_t get_cpu_freq(void);

/*
 * Delay for ms.
 * This function takes into account the CPU freqency.
 */

/*
extern inline void xdelay_ms(uint16_t ms)
{
    _delay_ms(ms);
}
*/

/*
 * Delay for us.
 * This function takes into account the CPU freqency.
 */

/*
extern inline void xdelay_us(uint8_t us)
{
    _delay_us(us);
}
*/

#ifdef __cplusplus
}
#endif

#endif

