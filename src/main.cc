
#include <avr/io.h> 
#include <avr/pgmspace.h>
#include <util/delay_basic.h>
#include <util/delay.h>
#include "ili9320.h"
#include "sound.h"
#include "enc28j60.h"

#define DACW(DATA) while (!(DACB.STATUS & DAC_CH0DRE_bm)) ; DACB.CH0DATA = DATA;


int main(void)
{
    PORTE.DIRSET = _BV(1);
    PORTE.OUTCLR = _BV(1);
    
    ili9320 display;
    display.initialize();
    
    display.chip_select();
    display.clear();
    display.chip_deselect();
    
#define wi 2
#define he 2
    for (uint16_t x = 0; x < 32; x++) {
        for (uint16_t y = 0; y < 24; y++) {
            uint16_t x_tmp = x * 10;
            uint16_t y_tmp = y * 10;
            
            display.chip_select();
            display.fill(y_tmp, x_tmp, wi, he, 0xf400);
            display.chip_deselect();
        }
    }
    
    /*
    for (uint16_t x = 0; x < 32; x++) {
        for (uint16_t y = 0; y < 15; y++) {
            uint16_t x_tmp = x * 10;
            uint16_t y_tmp = y * 10;
            
            display.chip_select();
            display.fill(y_tmp, x_tmp, wi, he, 0xf000);
            display.chip_deselect();
        }
    }
    
    for (uint16_t x = 0; x < 32; x++) {
        for (uint16_t y = 0; y < 5; y++) {
            uint16_t x_tmp = x * 10;
            uint16_t y_tmp = y * 10;
            
            display.chip_select();
            display.fill(y_tmp, x_tmp, wi, he, 0x00f4);
            display.chip_deselect();
        }
    }
    */
    
    PORTE.OUTTGL  = _BV(1);
    // Enable DAC
    
    // PORTB.DIRSET = _BV(2);
    // PORTB.OUTSET = _BV(2);
    
    DACB.EVCTRL = 0x00;
    DACB.TIMCTRL = 0x00;
    // DACB.GAINCAL = 0x00;
    // DACB.OFFSETCAL = 0x00;
    
    DACB.CTRLC = DAC_REFSEL_INT1V_gc;
    DACB.CTRLB = DAC_CHSEL_SINGLE_gc;
    DACB.CTRLA = DAC_CH0EN_bm | DAC_ENABLE_bm;

    while (true) {
        for (prog_uint8_t * ptr = (prog_uint8_t*)sound_data; ptr <= (sound_data + sound_data_length); ptr++) {
            PORTE.OUTTGL = _BV(1);
            
            uint8_t byte = (uint8_t)pgm_read_byte_far(ptr);
            DACW(byte);
            _delay_us(100);
        }
    }
    
    while (true) {
        // Do nothing
    }
    
    return 0;
}

