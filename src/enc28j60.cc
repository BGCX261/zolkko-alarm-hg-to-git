/**
 * Based on the enc28j60.c file from the AVRlib library by Pascal Stang.
 */

#include <avr/io.h>
#include <util/delay_basic.h>
#include "spi.h"
#include "enc28j60.h"


enc28j60::enc28j60(Spi * spi)
{
    this->_spi = spi;
}

void enc28j60::delayUs(uint16_t count)
{
}

/**
 * Delay at least count miliseconds
 *
 * TODO: extract into external library
 */
void enc28j60::delayMs(uint16_t count)
{
    uint32_t c = 0;
    
    if (CLK.CTRL & CLK_SCLKSEL_RC32K_gc) {
        c = 9;
	} else if (CLK.CTRL & CLK_SCLKSEL_RC32M_gc){
        c = 8001;
	} else if (CLK_SCLKSEL_RC2M_gc == (CLK.CTRL & CLK_SCLKSEL_gm)) {
        c = 501;
	} else if (CLK.CTRL & CLK_SCLKSEL_PLL_gc) {
		float factor = ((OSC.PLLCTRL & OSC_PLLFAC_gm) >> OSC_PLLFAC_gp) * 1.0;
        
		if (OSC.PLLCTRL & OSC_PLLSRC_RC2M_gc) {
            c = ((2000.0 * (factor / 1000.0)) / 4) + 1;
        } else if (OSC.PLLCTRL & OSC_PLLSRC_RC32M_gc) {
            count = ((32000.0 * (factor / 1000.0)) / 4) + 1;
        } else {
            count = (((F_CPU * factor) / 1000.0) / 4) + 1;
        }
	} else if (CLK.CTRL & CLK_SCLKSEL_XOSC_gc) {
		c = (uint32_t)(F_CPU / 1000.0 / 4) + 1;
	}
    
    while (count > 0) {
        uint32_t i = c;
        while (i > 0xffff) {
            _delay_loop_2(0xffff);
            i -= 0xffff;
        }
        _delay_loop_2(i);
        count--;
    }
}

void enc28j60::initialize_io(void)
{
    // Initialize IO Ports
}

/**
 * Initialize SPI interface
 * master mode and Fosc/2 clock:
 */
void enc28j60::initialize_spi(void)
{
    // this->_spi
    SPCR  = (1 << SPE) | ( 1 << MSTR);
    SPSR |= (1 << SPI2X);
}

/**
 * Initialize enc28j60 with mac address macaddr
 */
void enc28j60::initialize(const uint8_t macaddr[6])
{
    /*
    // initialize I/O
    // ss as output:
	pinMode(ENC28J60_CONTROL_CS, OUTPUT);
	CSPASSIVE; // ss=0
    	
	pinMode(SPI_MOSI, OUTPUT);
	
	pinMode(SPI_SCK, OUTPUT);
	
	pinMode(SPI_MISO, INPUT);
	*/
	
	digitalWrite(SPI_MOSI, LOW);
	
	digitalWrite(SPI_SCK, LOW);
	
	/*DDRB  |= 1<<PB3 | 1<<PB5; // mosi, sck output
	cbi(DDRB,PINB4); // MISO is input
        //
        cbi(PORTB,PB3); // MOSI low
        cbi(PORTB,PB5); // SCK low
  */
    
    this->initialize_spi();
    
    this->softReset();
    
	// Do bank 0 stuff
	// initialize receive buffer
	// 16-bit transfers, must write low byte first
	// set receive buffer start address
	this->nextPacketPtr = RXSTART_INIT;
    
    // Rx start
	this->write(ERXSTL, RXSTART_INIT &  0xFF);
    this->write(ERXSTH, RXSTART_INIT >> 8);
    
	// Set receive pointer address
	this->write(ERXRDPTL, RXSTART_INIT &  0xFF);
	this->write(ERXRDPTH, RXSTART_INIT >> 8);
    
	// RX end
	this->write(ERXNDL, RXSTOP_INIT &  0xFF);
	this->write(ERXNDH, RXSTOP_INIT >> 8);
    
	// TX start
	this->write(ETXSTL, TXSTART_INIT &  0xFF);
	this->write(ETXSTH, TXSTART_INIT >> 8);
    
	// TX end
    this->write(ETXNDL, TXSTOP_INIT &  0xFF);
    this->write(ETXNDH, TXSTOP_INIT >> 8);
    
    // Do bank 1 stuff, packet filter:
    // For broadcast packets we allow only ARP packtets
    // All other packets should be unicast only for our mac (MAADR)
    //
    // The pattern to match on is therefore
    // Type     ETH.DST
    // ARP      BROADCAST
    // 06 08 -- ff ff ff ff ff ff -> ip checksum for theses bytes = f7f9
    // in binary these poitions are:11 0000 0011 1111
    // This is hex 303F -> EPMM0 = 0x3f, EPMM1 = 0x30
    this->write(ERXFCON, ERXFCON_UCEN | ERXFCON_CRCEN | ERXFCON_PMEN);
    this->write(EPMM0, 0x3f);
    this->write(EPMM1, 0x30);
    this->write(EPMCSL, 0xf9);
    this->write(EPMCSH, 0xf7);
    
	// Do bank 2 stuff
	// enable MAC receive
    this->write(MACON1, MACON1_MARXEN | MACON1_TXPAUS | MACON1_RXPAUS);
    
	// bring MAC out of reset
	this->write(MACON2, 0x00);
    
	// enable automatic padding to 60bytes and CRC operations
	this->writeOp(ENC28J60_BIT_FIELD_SET, MACON3, MACON3_PADCFG0 | MACON3_TXCRCEN | MACON3_FRMLNEN);
    
	// set inter-frame gap (non-back-to-back)
	this->write(MAIPGL, 0x12);
	this->write(MAIPGH, 0x0C);
    
	// set inter-frame gap (back-to-back)
    this->write(MABBIPG, 0x12);
    
	// Set the maximum packet size which the controller will accept
    // Do not send packets longer than MAX_FRAMELEN:
    this->write(MAMXFLL, MAX_FRAMELEN &  0xFF);	
	this->write(MAMXFLH, MAX_FRAMELEN >> 8);
    
    // write mac address
    // mac address is byte-backward
    this->write(MAADR5, macaddr[0]);
    this->write(MAADR4, macaddr[1]);
    this->write(MAADR3, macaddr[2]);
    this->write(MAADR2, macaddr[3]);
    this->write(MAADR1, macaddr[4]);
    this->write(MAADR0, macaddr[5]);
    
	// no loopback of transmitted frames
	this->phyWrite(PHCON2, PHCON2_HDLDIS);
    
	// switch to bank 0
    this->setBank(ECON1);
    
	// enable interrutps
    this->writeOp(ENC28J60_BIT_FIELD_SET, EIE, EIE_INTIE | EIE_PKTIE);
    
	// enable packet reception
    this->writeOp(ENC28J60_BIT_FIELD_SET, ECON1, ECON1_RXEN);
}

/**
 *
 */
void enc28j60::writeOp(uint8_t op, uint8_t address, uint8_t data)
{
    this->_spi->select();
    
    // issue write command
    this->_spi->write(op | (address & ADDR_MASK));
    this->_spi->wait();
    
    // write data
    this->_spi->write(data);
    this->_spi->wait();
    
    this->_spi->deselect();
}

/**
 *
 */
uint8_t enc28j60::readOp(uint8_t op, uint8_t address)
{
    this->_spi->select();

    // issue read command
    this->_spi->write(op | (address & ADDR_MASK));
    this->_spi->wait();
    
    // read data
    this->_spi->write(0x00);
    this->_spi->wait();
    
    // do dummy read if needed (for mac and mii, see datasheet page 29)
    if (address & 0x80) {
        this->_spi->write(0x00);
        this->_spi->wait();
    }
    
    this->_spi->deselect();

    return this->_spi->read();
}

/**
 *
 */
void enc28j60::readBuffer(uint16_t len, uint8_t* data)
{
    this->_spi->select();
    
    // issue read command
    this->_spi->write(ENC28J60_READ_BUF_MEM);
    this->_spi->wait();
    
    while (len) {
        len--;
        
        // read data
        this->_spi->write(0x00);
        this->_spi->wait();
        
        *data = this->_spi->read();
        data++;
    }
    
    *data='\0';
    
    this->_spi->deselect();
}

/**
 *
 */
void enc28j60::writeBuffer(uint16_t len, uint8_t* data)
{
    this->_spi->select();
    
    // issue write command
    this->_spi->write(ENC28J60_WRITE_BUF_MEM);
    this->_spi->wait();
    
    while (len) {
        len--;
        
        // write data
        this->_spi->write(*data);
        data++;
        
        this->_spi->wait();
    }
    this->deselect();
}

/**
 * Read data from enc28j60 register
 */
uint8_t enc28j60::read(uint8_t address)
{
    this->setBank(address);
    return this->readOp(ENC28J60_READ_CTRL_REG, address);
}

/**
 * Write data into enc28j60 register
 */
void enc28j60::write(uint8_t address, uint8_t data)
{
    this->setBank(address);
    this->writeOp(ENC28J60_WRITE_CTRL_REG, address, data);
}

void enc28j60::phyWrite(uint8_t address, uint16_t data)
{
    // set the PHY register address
    this->write(MIREGADR, address);
    
    // write the PHY data
    this->write(MIWRL, data);
    this->write(MIWRH, data >> 8);
    
    // wait until the PHY write completes
    while (this->read(MISTAT) & MISTAT_BUSY) {
        this->delayMs(15);
    }
}

uint16_t enc28j60::phyReadH(uint8_t address)
{
	// Set the right address and start the register read operation
    this->write(MIREGADR, address);
    this->write(MICMD, MICMD_MIIRD);
    this->delayUs(15);
    
    // wait until the PHY read completes
	while (this->read(MISTAT) & MISTAT_BUSY) ;
    
    // reset reading bit
    this->write(MICMD, 0x00);
    
    return this->read(MIRDH);
}


/**
 *
 */
void enc28j60::packetSend(uint16_t len, uint8_t* packet)
{
    // Check no transmit in progress
    while (this->readOp(ENC28J60_READ_CTRL_REG, ECON1) & ECON1_TXRTS) {
        // Reset the transmit logic problem.
        // See Rev. B4 Silicon Errata point 12.
        if (this->read(EIR) & EIR_TXERIF) {
            this->writeOp(ENC28J60_BIT_FIELD_SET, ECON1, ECON1_TXRST);
            this->writeOp(ENC28J60_BIT_FIELD_CLR, ECON1, ECON1_TXRST);
        }
    }
    
	// Set the write pointer to start of transmit buffer area
	this->write(EWRPTL, TXSTART_INIT &  0xFF);
    this->write(EWRPTH, TXSTART_INIT >> 8);
    
	// Set the TXND pointer to correspond to the packet size given
	this->write(ETXNDL, (TXSTART_INIT + len) & 0xff);
	this->write(ETXNDH, (TXSTART_INIT + len) >> 8);
    
	// write per-packet control byte (0x00 means use macon3 settings)
    this->writeOp(ENC28J60_WRITE_BUF_MEM, 0, 0x00);
    
	// copy the packet into the transmit buffer
    this->writeBuffer(len, packet);
    
	// send the contents of the transmit buffer onto the network
	this->writeOp(ENC28J60_BIT_FIELD_SET, ECON1, ECON1_TXRTS);
    
    // Reset the transmit logic problem.
    // See Rev. B4 Silicon Errata point 12.
    // if (this->read(EIR) & EIR_TXERIF) {
    //      this->writeOp(ENC28J60_BIT_FIELD_CLR, ECON1, ECON1_TXRTS);
    // }
}

uint16_t enc28j60::packetReceive(uint16_t maxlen, uint8_t* packet)
{
	uint16_t rxstat;
	uint16_t len;
    
	// check if a packet has been received and buffered
	// if( !(enc28j60Read(EIR) & EIR_PKTIF) ){
    // The above does not work. See Rev. B4 Silicon Errata point 6.
    if (this->read(EPKTCNT) == 0) {
        return 0;
    }

	// Set the read pointer to the start of the received packet
    this->write(ERDPTL, this->nextPacketPtr &  0xff);
	this->write(ERDPTH, this->nextPacketPtr >> 8);
    
	// read the next packet pointer
	this->nextPacketPtr  = this->readOp(ENC28J60_READ_BUF_MEM, 0);
	this->nextPacketPtr |= this->readOp(ENC28J60_READ_BUF_MEM, 0) << 8;
    
	// read the packet length (see datasheet page 43)
	len  = this->readOp(ENC28J60_READ_BUF_MEM, 0);
	len |= this->readOp(ENC28J60_READ_BUF_MEM, 0) << 8;
    len -= 4; //remove the CRC count
    
	// read the receive status (see datasheet page 43)
	rxstat  = this->readOp(ENC28J60_READ_BUF_MEM, 0);
	rxstat |= ((uint16_t)this->readOp(ENC28J60_READ_BUF_MEM, 0)) << 8;
    
	// limit retrieve length
    if (len > maxlen - 1) {
        len = maxlen - 1;
    }
    
    // check CRC and symbol errors (see datasheet page 44, table 7-3):
    // The ERXFCON.CRCEN is set by default.
    // Normally we should not need to check this.
    if ((rxstat & 0x80) == 0) {
        // invalid
        len = 0;
    } else {
        // copy the packet from the receive buffer
        this->readBuffer(len, packet);
    }
    
	// Move the RX read pointer to the start of the next received packet
	// This frees the memory we just read out
    this->write(ERXRDPTL, this->nextPacketPtr & 0xFF);
    this->write(ERXRDPTH, this->nextPacketPtr >> 8);
    
    // Move the RX read pointer to the start of the next received packet
    // This frees the memory we just read out.
    // However, compensate for the errata point 13,
    // rev B4: enver write an even address!
    if ((this->nextPacketPtr - 1 < RXSTART_INIT) ||
        (this->nextPacketPtr - 1 > RXSTOP_INIT))
    {
        this->write(ERXRDPTL, (RXSTOP_INIT) &  0xff);
        this->write(ERXRDPTH, (RXSTOP_INIT) >> 8);
    } else {
        this->write(ERXRDPTL, (this->nextPacketPtr - 1) & 0xff);
        this->write(ERXRDPTH, (this->nextPacketPtr - 1) >> 8);
    }
    
	// decrement the packet counter indicate we are done with this packet
    this->writeOp(ENC28J60_BIT_FIELD_SET, ECON2, ECON2_PKTDEC);
    return len;
}

/**
 * set the bank (if needed)
 */
void enc28j60::setBank(uint8_t address)
{
    if ((address & BANK_MASK) != this->bank) {
        this->writeOp(ENC28J60_BIT_FIELD_CLR, ECON1, (ECON1_BSEL1|ECON1_BSEL0));
        this->writeOp(ENC28J60_BIT_FIELD_SET, ECON1, (address & BANK_MASK) >> 5);
        this->bank = (address & BANK_MASK);
    }
}       

/**
 * Setup clock out
 * 2 is 12.5MHz
 */
void enc28j60::clkout(uint8_t clk)
{
    this->write(ECOCON, clk & 0x7);
}

/**
 *
 */
uint8_t enc28j60::getrev(void)
{
    return (this->read(EREVID));
}

/**
 * Has RX packet
 */
uint8_t enc28j60::hasRxPkt(void)
{
    if (this->read(EPKTCNT) == 0) {
        return 0;
    } else {
        return 1;
    }
}

/**
 * Link status
 */
uint8_t enc28j60::linkup(void)
{
    return this->phyReadH(PHSTAT2) && 4;
}

