/**
 * (C) copyright Alex Anisimov <zolkko@gmail.com>
 * GPL v3
 *
 * Delay functions are based on avr-gcc-lib utils delay
 * implementation.
 */
#ifndef _UTILS_H_
#define _UTILS_H_

/*
 * Returns CPU freq according to current
 * clock source.
 */
inline uint32_t get_cpu_freq(void);

/*
 * Delay for ms.
 * This function takes into account the CPU freqency.
 */
inline void xdelay_ms(const double ms);

/*
 * Delay for us.
 * This function takes into account the CPU freqency.
 */
inline void xdelay_us(const double us);

#endif

