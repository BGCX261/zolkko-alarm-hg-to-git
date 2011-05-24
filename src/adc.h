/*
 * Polled interface to ADC
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

#ifndef _adc_h_
#define _adc_h_

class adc_channel
{
    private:
        ADC_CH_t& _adc_ch;
        
        uint16_t& _adc_res;
        
    public:
        adc_channel(ADC_CH_t& __adc_ch, uint16_t& __adc_res)
            : _adc_ch(__adc_ch),
              _adc_res(__adc_res)
        {
        }
        
        inline uint8_t convert(void)
        {
            // 
        }
        
        uint16_t get_value(void)
        {
            return (uint16_t) _adc_res;
        }
};

class adc
{
    private:
        ADC_t& _adc;
    
    public:
        adc(ADC_t& __adc)
            : _adc(__adc)
        {
        }
        
        void init(uint8_t __ctrlb)
        {
            _adc.CTRLB = __ctrlb;
        }
        
        inline void flush(void)
        {
            _adc.CTRLA |= ADC_FLUSH_bm;
        }
        
        inline void enable(void)
        {
            _adc.CTRLA |= ADC_ENABLE_bm;
        }
        
        inline void disable(void)
        {
            _adc.CTRLA &= ~ADC_ENABLE_bm;
        }
        
        inline uint8_t is_enabled(void)
        {
            return (_adc.CTRLA & ADC_ENABLE_bm) == ADC_ENABLE_bm;
        }
};

#endif

