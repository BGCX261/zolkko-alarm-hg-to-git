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
 * From XMega manual: 
 * System critical I/O register settings are protected from accidental modification.
 * The SPM instruction is protected from accidental execution, and the LPM instruction is
 * protected when reading the fuses and signature row.
 * This is handled globally by the Configuration Change Protection(CCP) register.
 * Changes to the protected I/O registers or bit, or execution of the protected instructions
 * are only possible after the CPU writes a signature to the CCP register. The different
 * signatures is described the register description.
 *
 * Within 4 instruction cycles, the application code must write the appropriate
 * data to the protected register.
 */
extern inline void critical_write(volatile uint8_t * address, uint8_t value);

#if F_CPU==32000000UL
#	define BAUD_9600_B   0xcc
#	define BAUD_9600_A   0xf5
#	define BAUD_115200_B 0x98
#	define BAUD_115200_A 0x2e
#elseif F_CPU=9000000UL
#	define BAUD_9600_B   0xd7
#	define BAUD_9600_A   0x33
#	define BAUD_115200_B 0xf1
#	define BAUD_115200_A 0xf1
#else // 32MHz
#	define BAUD_9600_B   0xcc
#	define BAUD_9600_A   0xf5
#	define BAUD_115200_B 0x98
#	define BAUD_115200_A 0x2e
#endif

extern void enable_pll();

extern void enable_32mhz();


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

