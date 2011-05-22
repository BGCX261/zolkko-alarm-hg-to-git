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

#include <avr/io.h>
#include <util/delay.h>
#include <util/atomic.h>
#include "net.h"
#include "utils.h"

void critical_write(volatile uint8_t * address, uint8_t value)
{
	volatile uint8_t * tmpAddr = address;
		
	RAMPZ = 0;
		
	asm volatile(
		"movw r30,  %0"	      "\n\t"
		"ldi  r16,  %2"	      "\n\t"
		"out   %3, r16"	      "\n\t"
		"st     Z,  %1"       "\n\t"
		:
		: "r" (tmpAddr), "r" (value), "i"(CCP_IOREG_gc), "i" (&CCP)
		: "r16", "r30", "r31"
	);
}

void enable_pll()
{
	ATOMIC_BLOCK(ATOMIC_RESTORESTATE) {
		OSC.CTRL &= ~OSC_PLLEN_bm; // disable PLL
		OSC.PLLCTRL = OSC_PLLSRC_RC2M_gc | (8 << OSC_PLLFAC_gp); // 2Mhz * (16 << 0) == 32MHz
		OSC.CTRL |= OSC_PLLEN_bm;
		do {} while (!(OSC.STATUS & OSC_PLLRDY_bm)) ;
		critical_write(&(CLK.PSCTRL), CLK_PSADIV_1_gc | CLK_PSBCDIV_1_1_gc);
		critical_write(&(CLK.CTRL), CLK_SCLKSEL_PLL_gc);
	}
}

void enable_32mhz()
{
	ATOMIC_BLOCK(ATOMIC_RESTORESTATE) {
		OSC.CTRL |= OSC_RC32MEN_bm;       
		do {} while(!(OSC.STATUS & OSC_RC32MRDY_bm ));
		critical_write(&(CLK.CTRL), CLK_SCLKSEL_RC32M_gc);
	}
}

inline void aes_load_key(const uint8_t * key)
{
    for (uint8_t i = 0; i < AES_BLOCK_LENGTH; i++) AES.KEY = key[i];
}

/*
 * Encrypts data
 */
uint8_t aes_encrypt(const uint8_t * key, const uint8_t * data, uint8_t * encdata)
{
    // Reset AES module
    AES.CTRL = AES_RESET_bm;
    
    aes_load_key(key);
    
	// Load data into AES state memory
	for (uint8_t i = 0; i < AES_BLOCK_LENGTH; i++) {
		AES.STATE = data[i];
	}
    
	// Set AES in encryption mode and start AES
	AES.CTRL = (AES.CTRL & (~AES_DECRYPT_bm)) | AES_START_bm;
	do {
		// Wait until AES is finished or an error occurs
	} while ((AES.STATUS & (AES_SRIF_bm|AES_ERROR_bm) ) == 0);
    
	// If not error
	if ((AES.STATUS & AES_ERROR_bm) == 0) {
		for (uint8_t i = 0; i < AES_BLOCK_LENGTH; i++) {
            encdata[i] = AES.STATE;
		}
        return true;
	} else {
        return false;
	}
}

/*
 * Decrypts data
 */
uint8_t aes_decrypt(const uint8_t * key, const uint8_t * data, uint8_t * out_data)
{
    AES_CTRL = AES_RESET_bm;
    
    // Calculate key base on KEY variable
    aes_load_key(key);
    
    // Load dummy data into AES state memory
	for (uint8_t i = 0; i < AES_BLOCK_LENGTH; i++) {
        AES.STATE = 0x00;
    }
    
    // Set AES in encryption mode and start AES
    AES.CTRL = (AES.CTRL & (~AES_DECRYPT_bm)) | AES_START_bm;
    do {
        // Wait until AES is finished or an error occurs
    } while ((AES.STATUS & (AES_SRIF_bm|AES_ERROR_bm) ) == 0);
    
    uint8_t subkey[AES_BLOCK_LENGTH];
    
    // If not error
    if ((AES.STATUS & AES_ERROR_bm) == 0) {
        // Store the last subkey
		for (uint8_t i = 0; i < AES_BLOCK_LENGTH; i++) {
            subkey[i] = AES.KEY;
        }
		AES.STATUS = AES_SRIF_bm;
    } else {
        AES.STATUS = AES_ERROR_bm;
        return false;
    }
    
    // Decrypt
    aes_load_key(subkey);
    
    // Load data into AES state memory
    for (uint8_t i = 0; i < AES_BLOCK_LENGTH; i++) {
        AES.STATE = data[i];
    }
    
    // Set AES in decryption mode and start the AES
    AES.CTRL |= (AES_START_bm | AES_DECRYPT_bm);
    
    do {
        // Wait until AES is finished or an error occurs
    } while ((AES.STATUS & (AES_SRIF_bm|AES_ERROR_bm)) == 0);
    
    // If not error
    if ((AES.STATUS & AES_ERROR_bm) == 0) {
        for (uint8_t i = 0; i < AES_BLOCK_LENGTH; i++) {
            out_data[i] = AES.STATE;
		}
    } else {
        return false;
    }
    
    return true;
}

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

uint16_t checksum(uint8_t * buf, uint16_t len, uint8_t type)
{
    uint32_t sum = 0;
    
    if (type == 1) {
        sum += IP_PROTO_UDP_V;
        sum += len - UDP_HDR_LEN; // - udp length
    } else if (type == 2) {
        sum += IP_PROTO_TCP_V; 
        sum += len - 8; // - tcp length
    }
    
    while (len > 1) {
        sum += 0xffff & (*buf << 8 | *(buf + 1));
        buf += 2;
        len -= 2;
    }
    
    if (len) {
        sum += (0xff & *buf) << 8;
    }
    
    while (sum >> 16) {
        sum = (sum & 0xffff) + (sum >> 16);
    }
    
    return ((uint16_t) sum ^ 0xffff);
}

