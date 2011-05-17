/*
 * Utility functions.
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 *
 * This file is part of the SmokeHouseCTRL Firmware.
 *
 * The SmokeHouseCTRL Firmware is free software: you can redistribute it
 * and/or modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * The SmokeHouseCTRL Firmware is distributed in the hope that it will be
 * useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with SmokeHouseCTRL Firmware.  If not, see
 * <http://www.gnu.org/licenses/>.
 */
#ifndef _UTILS_H_
#define _UTILS_H_

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Interface for XMega's AES module
 */
#ifndef AES_BLOCK_LENGTH
#define AES_BLOCK_LENGTH 16
#endif

/*
 * Encrypt data using AES
 */
extern uint8_t aes_encrypt(const uint8_t * key, const uint8_t * data, uint8_t * encdata);

/*
 * Decrypt data using AES
 */
extern uint8_t aes_decrypt(const uint8_t * key, const uint8_t * data, uint8_t * out_data);

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

