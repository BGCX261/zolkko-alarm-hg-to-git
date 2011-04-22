/*
 * Interface master XMega SPI module.
 *
 * TODO: Add interrupt support, async bufferred transfer
 * and receiving, DMA slave mode, acting in slave mode,
 * switching into slave mode and other XMega SPI module features.
 *
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 * GPL v3
 */

#ifndef _SPI_H_
#define _SPI_H_

class Spi
{
    private :
        SPI_t * _spi;
        
        PORT_t * _ss_port;
        
        uint8_t _ss_bm;
        
    public:
        
        /*
         * Initialization in constructor garanties that
         * SPI module would be initialized.
         */
        Spi(SPI_t * spi,
                uint8_t module_ctrl,
                PORT_t * sck_port, uint8_t sck_bm,
                PORT_t * mosi_port, uint8_t mosi_bm,
                PORT_t * ss_port, register8_t * ss_pinctrl, uint8_t ss_bm)
        {
            _spi = spi;
            _ss_port = ss_port;
            _ss_bm = ss_bm;
            
            // Initialize chip select line.
            // Output pull-up is set to prevent swiching into slave mode.
            ss_port->DIRSET = ss_bm;
            *ss_pinctrl = PORT_OPC_WIREDANDPULL_gc;
            ss_port->OUTSET = ss_bm;
            
            // Initialize SPI module in master mode
            spi->CTRL = SPI_ENABLE_bm | SPI_MASTER_bm | module_ctrl;
            spi->INTCTRL = SPI_INTLVL_OFF_gc;
            
            // Initialize SCK and MOSI pins
            mosi_port->DIRSET = mosi_bm;
            sck_port->DIRSET = sck_bm;
            
            // Other PIN paramters SPI module sets itself in mastermode.
        }
        
        inline void write(uint8_t data)
        {
            _spi->DATA = data;
        }
        
        inline uint8_t read(void)
        {
            return _spi->DATA;
        }
        
        inline void wait(void)
        {
            while ((_spi->STATUS & SPI_IF_bm) == 0) ;
        }
        
        inline void select(void)
        {
            _ss_port->OUTCLR = _ss_bm;
        }
        
        inline void deselect(void)
        {
            _ss_port->OUTSET = _ss_bm;
        }
};

#endif

