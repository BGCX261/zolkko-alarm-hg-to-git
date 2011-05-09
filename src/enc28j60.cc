/*
 * Network driver for ENC28J60 IC
 * 
 * Copyright (c) 2011 Alex Anisimov, <zolkko@gmail.com>
 * GPLv3
 */

#define UART_DEBUG 1

#ifdef UART_DEBUG
#include <stdio.h>
#include "uart_stdio.h"
#endif

#include <stdio.h>
#include <avr/io.h>
#include <util/delay.h>
#include <inttypes.h>
#include "spi.h"
#include "net.h"
#include "net_driver.h"
#include "enc28j60.h"


#define ENC28J60_ETHER_HDR_LEN 14


void enc28j60::init(const ether_addr_t& mac)
{
    // At this point Spi module have to be initialized
    soft_reset();
    
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
	this->write_op(ENC28J60_BIT_FIELD_SET, MACON3, MACON3_PADCFG0 | MACON3_TXCRCEN | MACON3_FRMLNEN);
    
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
    this->write(MAADR5, mac[0]);
    this->write(MAADR4, mac[1]);
    this->write(MAADR3, mac[2]);
    this->write(MAADR2, mac[3]);
    this->write(MAADR1, mac[4]);
    this->write(MAADR0, mac[5]);
    
	// no loopback of transmitted frames
	this->phy_write(PHCON2, PHCON2_HDLDIS);
    
	// switch to bank 0
    this->set_bank(ECON1);
    
	// enable interrutps
    this->write_op(ENC28J60_BIT_FIELD_SET, EIE, EIE_INTIE | EIE_PKTIE);
    
	// enable packet reception
    this->write_op(ENC28J60_BIT_FIELD_SET, ECON1, ECON1_RXEN);
}

/*
 * TODO: replay _delay_ms call to call that takes current
 * CPU freqency in account
 */
void enc28j60::soft_reset(void)
{
	write_op(ENC28J60_SOFT_RESET, 0, ENC28J60_SOFT_RESET);
	
	// Chip requires at least 1ms delay for stabilization.
	_delay_ms(ENC28J60_MIN_RESET_DELAY); 
	bank = 0;
    
	// while (!(read(ESTAT) & ESTAT_CLKRDY)) ;
}

void enc28j60::write_op(uint8_t op, uint8_t address, uint8_t data)
{
    _spi.select();
    
    // issue write command
    _spi.write(op | (address & ADDR_MASK));
    _spi.wait();
    
    // write data
    _spi.write(data);
    _spi.wait();
    
    _spi.deselect();
}

uint8_t enc28j60::read_op(uint8_t op, uint8_t address)
{
    _spi.select();

    // issue read command
    _spi.write(op | (address & ADDR_MASK));
    _spi.wait();
    
    // read data
    _spi.write(0x00);
    _spi.wait();
    
    // do dummy read if needed (for mac and mii, see datasheet page 29)
    if (address & 0x80) {
        _spi.write(0x00);
        _spi.wait();
    }
    
    _spi.deselect();
    
    return _spi.read();
}

void enc28j60::read_buffer(uint16_t len, uint8_t* data)
{
    _spi.select();
    
    // issue read command
    _spi.write(ENC28J60_READ_BUF_MEM);
    _spi.wait();
    
    while (len) {
        len--;
        
        // read data
        _spi.write(0x00);
        _spi.wait();
        
        *data = _spi.read();
        data++;
    }
    
    *data = '\0';
    
    _spi.deselect();
}

void enc28j60::write_buffer(uint16_t len, uint8_t* data)
{
    _spi.select();
    
    // issue write command
    _spi.write(ENC28J60_WRITE_BUF_MEM);
    _spi.wait();
    
    while (len) {
        len--;
        
        // write data
        _spi.write(*data);
        data++;
        
        _spi.wait();
    }
    _spi.deselect();
}

uint8_t enc28j60::read(uint8_t address)
{
    this->set_bank(address);
    return this->read_op(ENC28J60_READ_CTRL_REG, address);
}

void enc28j60::write(uint8_t address, uint8_t data)
{
    this->set_bank(address);
    this->write_op(ENC28J60_WRITE_CTRL_REG, address, data);
}

void enc28j60::phy_write(uint8_t address, uint16_t data)
{
    // set the PHY register address
    this->write(MIREGADR, address);
    
    // write the PHY data
    this->write(MIWRL, data);
    this->write(MIWRH, data >> 8);
    
    // wait until the PHY write completes
    while (this->read(MISTAT) & MISTAT_BUSY) {
        _delay_ms(15);
    }
}

uint16_t enc28j60::phy_read_h(uint8_t address)
{
	// Set the right address and start the register read operation
    this->write(MIREGADR, address);
    this->write(MICMD, MICMD_MIIRD);
    _delay_us(15);
    
    // wait until the PHY read completes
	while (this->read(MISTAT) & MISTAT_BUSY) ;
    
    // reset reading bit
    this->write(MICMD, 0x00);
    
    return this->read(MIRDH);
}

void enc28j60::set_bank(uint8_t address)
{
    if ((address & BANK_MASK) != this->bank) {
        this->write_op(ENC28J60_BIT_FIELD_CLR, ECON1, (ECON1_BSEL1|ECON1_BSEL0));
        this->write_op(ENC28J60_BIT_FIELD_SET, ECON1, (address & BANK_MASK) >> 5);
        this->bank = (address & BANK_MASK);
    }
}       

void enc28j60::clkout(uint8_t clk)
{
    this->write(ECOCON, clk & 0x7);
}

uint8_t enc28j60::linkup(void)
{
    return this->phy_read_h(PHSTAT2) && 4;
}

uint8_t enc28j60::is_supported(void)
{
    return read(EREVID) == ENC28J60_REV_B7;
}

uint8_t enc28j60::has_packet(void)
{
    if (this->read(EPKTCNT) == 0) {
        return 0;
    } else {
        return 1;
    }
}

/*
 * Sends data though the driver network buffer
 */
uint8_t enc28j60::send(ether_frame_t& frame)
{
#ifdef UART_DEBUG
    printf("Send network packet via ENC28J60 driver.\r\n");
#endif
    // Check no transmit in progress
    while (this->read_op(ENC28J60_READ_CTRL_REG, ECON1) & ECON1_TXRTS) {
        // Reset the transmit logic problem.
        // See Rev. B4 Silicon Errata point 12.
        if (this->read(EIR) & EIR_TXERIF) {
            this->write_op(ENC28J60_BIT_FIELD_SET, ECON1, ECON1_TXRST);
            this->write_op(ENC28J60_BIT_FIELD_CLR, ECON1, ECON1_TXRST);
        }
    }
    
	// Set the write pointer to start of transmit buffer area
	this->write(EWRPTL, TXSTART_INIT &  0xFF);
    this->write(EWRPTH, TXSTART_INIT >> 8);    
    
	// Write per-packet control byte (0x00 means use macon3 settings)
    this->write_op(ENC28J60_WRITE_BUF_MEM, 0, 0x00);
    
    // Determine actual data to send without 
    uint16_t data_len = frame.len_or_type;
	
    if (data_len == ETHER_TYPE_ARP) {
        data_len = sizeof(arp_hdr_t) + ENC28J60_ETHER_HDR_LEN;
    } else if (data_len == ETHER_TYPE_IP) {
        data_len = 100;
    } else {
        // payload length + ethernet header without preamble section
        data_len = n_to_uint16(data_len) + ENC28J60_ETHER_HDR_LEN;
    }
    
	// Set the TXND pointer to correspond to the packet size given
	this->write(ETXNDL, (TXSTART_INIT + data_len) & 0xff);
	this->write(ETXNDH, (TXSTART_INIT + data_len) >> 8);
    
	// Copy the packet into the transmit buffer
#ifdef UART_DEBUG
	printf("Writing data (count %u) into enc28j60 buffer.\r\n", data_len);
#endif
    this->write_buffer(data_len, (uint8_t*) &frame.dst_mac);
    
	// Send the contents of the transmit buffer onto the network
	this->write_op(ENC28J60_BIT_FIELD_SET, ECON1, ECON1_TXRTS);
    
    // Reset the transmit logic problem.
    // See Rev. B4 Silicon Errata point 12.
    // if (this->read(EIR) & EIR_TXERIF) {
    //    this->write_op(ENC28J60_BIT_FIELD_CLR, ECON1, ECON1_TXRTS);
    // }
    return 1;
}

uint8_t enc28j60::receive(ether_frame_t& frame)
{
#ifdef UART_DEBUG
    printf("Receiving packet from ENC28J60.\n");
#endif
    
	uint16_t rxstat;
	uint16_t len;
    
	// check if a packet has been received and buffered
    // The above does not work. See Rev. B4 Silicon Errata point 6.
    if (this->read(EPKTCNT) == 0) {
        return 0;
    }
    
	// Set the read pointer to the start of the received packet
    this->write(ERDPTL, this->nextPacketPtr &  0xff);
	this->write(ERDPTH, this->nextPacketPtr >> 8);
    
	// read the next packet pointer
	this->nextPacketPtr  = this->read_op(ENC28J60_READ_BUF_MEM, 0);
	this->nextPacketPtr |= this->read_op(ENC28J60_READ_BUF_MEM, 0) << 8;
    
	// read the packet length (see datasheet page 43)
	len  = this->read_op(ENC28J60_READ_BUF_MEM, 0);
	len |= this->read_op(ENC28J60_READ_BUF_MEM, 0) << 8;
    
    // Remove the CRC count
    len -= 4;
    
	// read the receive status (see datasheet page 43)
	rxstat  = this->read_op(ENC28J60_READ_BUF_MEM, 0);
	rxstat |= ((uint16_t)this->read_op(ENC28J60_READ_BUF_MEM, 0)) << 8;
    
	// limit retrieve length
    if (len > (IF_PAYLOAD_MAX + ENC28J60_ETHER_HDR_LEN)) {
        len = (IF_PAYLOAD_MAX + ENC28J60_ETHER_HDR_LEN);
    }
    
    // Check CRC and symbol errors (see datasheet page 44, table 7-3):
    // The ERXFCON.CRCEN is set by default.
    // Normally we should not need to check this.
    if ((rxstat & 0x80) == 0) {
        len = 0;
    } else {
        // copy the packet from the receive buffer
        this->read_buffer(len, (uint8_t *)&frame.dst_mac);
    }
    
	// Move the RX read pointer to the start of the next received packet
	// This frees the memory we just read out
    this->write(ERXRDPTL, this->nextPacketPtr & 0xff);
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
    this->write_op(ENC28J60_BIT_FIELD_SET, ECON2, ECON2_PKTDEC);
    
    return len;
}

