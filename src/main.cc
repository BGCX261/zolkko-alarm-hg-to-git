/*
 * Smoke House ACS main file
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 * GPL v3
 */
#include <avr/io.h> 
#include <util/delay.h>
#include "ili9320.h"
#include "spi.h"
#include "iface.h"
#include "enc28j60.h"
#include "udp_service.h"

#define DACW(DATA) while (!(DACB.STATUS & DAC_CH0DRE_bm)) ; DACB.CH0DATA = DATA;

/*
 * Pure virtual function error handler
 * TODO: reset microcontroller
 */
extern "C" void __cxa_pure_virtual()
{
    while (1) ;
}

// ACS Service ethernet and ip addresses
const ether_addr_t service_mac = {0x00, 0x1f, 0x5b, 0xeb, 0x7f, 0x75};

const ip_addr_t service_ip = {192, 168, 55, 1};

// Device ethernet and ip addresses
const ether_addr_t device_mac = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06};

const ip_addr_t device_ip = {192, 168, 55, 2};


int main(void)
{
    // Debug pin direction
    PORTE.DIRSET = _BV(0);
    PORTE.DIRSET = _BV(1);
    
    PORTE.OUTCLR = _BV(0);
    PORTE.OUTCLR = _BV(1);
    
    // Stabilize reset of enc28j60 & etc
    _delay_ms(100);
    
    // Initialize spi master driver for enc28j60 module
    Spi spi(&SPIE,
            SPI_PRESCALER_DIV4_gc | SPI_MODE_0_gc,
            &PORTE, _BV(7),
            &PORTE, _BV(5),
            &PORTE, &(PORTE.PIN4CTRL), _BV(4));
    
    enc28j60 iface(spi, (ether_addr_t *) &device_mac, (ip_addr_t *) &device_ip);
    
    if (iface.is_supported()) {
        PORTE.OUTSET = _BV(0);
    }
    
    iface.init();
    
    UdpService udp(&iface);
    
    uint8_t counter = 0;
    
    uint8_t data[2];
    
    while (true) {
        _delay_ms(500);
        
        data[0] = 0;
        data[1] = counter;
        
        udp.send_to((ether_addr_t *)&service_mac, (ip_addr_t *)&service_ip, 8081, data, 2);
        
        if (counter == 0xff) {
            counter = 0;
        } else {
            counter++;
        }
    }
    
    return 0;
}

