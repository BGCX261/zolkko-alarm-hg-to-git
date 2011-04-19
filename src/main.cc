
#include <avr/io.h> 
#include <avr/pgmspace.h>
#include <util/delay_basic.h>
#include <util/delay.h>
#include "ili9320.h"
// L: #include "sound.h"
#include "enc28j60.h"
#include "spi.h"

#define DACW(DATA) while (!(DACB.STATUS & DAC_CH0DRE_bm)) ; DACB.CH0DATA = DATA;

/*
 * Pure virtual function error handler
 * TODO: reset microcontroller
 */
extern "C" void __cxa_pure_virtual()
{
    while (1) ;
}

static ether_addr_t device_mac = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

static ip_addr_t device_ip = {0x00, 0x00, 0x00, 0x00};

int main(void)
{
    // Debug pin direction
    PORTD.DIRSET = _BV(6);
    PORTD.DIRSET = _BV(5);

    // TODO: increase controller speed upto 12 MHz
    // TODO: change MISO / MOSI
    
    // Initialize SPI
    Spi spi(&SPIE,              // Spi module
            SPI_PIN(PORTE, 5),  // MOSI
            SPI_PIN(PORTE, 6),  // MISO
            SPI_PIN(PORTE, 7),  // Clock
            SPI_PIN(PORTE, 4)); // Chip Select
    
    // Initialize enc28j60 interface
    enc28j60 iface(spi, &device_mac, &device_ip);
    iface.init();
    _delay_ms(5);

    uint8_t v = iface.test();
    if (v == 0b00000010 ||
        v == 0b00000100 ||
        v == 0b00000101 ||
        v == 0b00000110 ||
        v == 0b00100000 ||
        v == 0b01000000 ||
        v == 0b01010000 ||
        v == 0b01100000)
    {
        PORTD.OUTSET = _BV(5);
    } else if (v == 0) {
        PORTD.OUTSET = _BV(6);
    } else {
        PORTD.OUTSET = _BV(6);
        PORTD.OUTSET = _BV(5);
    }
    
    // Setup debug pin
    
    /* L:
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
    */
    
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
    
    //L: PORTE.OUTTGL  = _BV(1);
    
    // Enable DAC
    
    // PORTB.DIRSET = _BV(2);
    // PORTB.OUTSET = _BV(2);
    
    /* L:
    DACB.EVCTRL = 0x00;
    DACB.TIMCTRL = 0x00;
     */
    
    // DACB.GAINCAL = 0x00;
    // DACB.OFFSETCAL = 0x00;
    
    /* L:
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
    }*/
    
    while (true) {
        // Do nothing
    }
    
    return 0;
}

