/**
 * (c) copyright Alex Anisimov <zolkko@gmail.com>
 * GPL v3
 */

#ifndef _SPI_H_
#define _SPI_H_

class Spi {
    private :
        const SPI_t * _spi;
        
        PORT_t * _ssPort;
        
        uint8_t _ssPinMask;
        
    public :
        Spi(const SPI_t * spi, PORT_t * ssPort, uint8_t ssPinMask)
        {
            this->_spi = spi;
            this->_ssPort = ssPort;
            this->_ssPinMask = ssPinMask;
        }
        
        inline void write(uint8_t data)
        {
        }
        
        inline uint8_t read(void)
        {
            return 0;
        }
        
        inline void wait(void)
        {
            while ((this->_spi->STATUS & SPI_IF_bm) == 0) ;
        }
        
        inline void select(void)
        {
            this->_ssPort->OUTCLR = this->_ssPinMask;
        }
        
        inline void deselect(void)
        {
            this->_ssPort->OUTSET = this->_ssPinMask;
        }
};

#endif

