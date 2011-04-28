/*
 * Utility functions.
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 * GPL v3
 */
#ifndef _UTILS_H_
#define _UTILS_H_

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Returns CPU freq according to current
 * clock source.
 */
extern uint32_t get_cpu_freq(void);

/*
 * Calculate CRC32 for IP header
 *
 * len - proto data length + proto header len
 *
 * type == 1 - for TCP packets
 * type == 2 - for UDP packets
 */
extern uint16_t checksum(uint8_t * buf, uint16_t len, uint8_t type);

#ifdef __cplusplus
}
#endif

#endif

