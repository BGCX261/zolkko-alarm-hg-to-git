/**
 * (c) copyright Alex Anisimov <zolkko@gmail.com>
 * GPL v3
 */

#ifndef _SPI_H_
#define _SPI_H_

#define SPI_PIN(port, pin) &port, _BV(pin)

class Spi {
    private :
        SPI_t * _spi;
        
        PORT_t * _ss_port;
        
        uint8_t _ss_bm;
        
    public:
        
        /*
         * Initialization in constructor garanties that
         * SPI module would be initialized
         *
         * TODO: pass SPI module configuration parameters into constructor
         */
        Spi(SPI_t * spi,
            PORT_t * mosi_port, uint8_t mosi_bm,
            PORT_t * miso_port, uint8_t miso_bm,
            PORT_t * clk_port,  uint8_t clk_bm,
            PORT_t * ss_port,   uint8_t ss_bm)
        {
            _spi = spi;
            _ss_port = ss_port;
            _ss_bm = ss_bm;
            
            // Initialize GPIO
            clk_port->DIRSET = clk_bm; // Clock - output low
            clk_port->OUTCLR = clk_bm;
            
            miso_port->DIRCLR = miso_bm; // MISO - input nevermind
            
            mosi_port->DIRSET = mosi_bm; // MOSI - output low
            mosi_port->OUTCLR = mosi_bm;
            
            _ss_port->DIRSET = _ss_bm; // Chip select - output hi
            _ss_port->OUTSET = _ss_bm;
            
            // Initialize SPI module
            // for enc28j60 max speed should be no longer than 10Mbit/s
            _spi->CTRL = SPI_ENABLE_bm |
                SPI_PRESCALER_DIV4_gc |
                SPI_DORD_bm   |
                SPI_MODE_0_gc |
                SPI_CLK2X_bm  |
                SPI_MASTER_bm ;
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

