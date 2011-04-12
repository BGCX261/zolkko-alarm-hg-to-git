/**
 * ENC28J60 Driver for XMega
 */

#ifndef _ENC28J60_H_
#define _ENC28J60_H_

class enc28j60 {
    private:
        uint8_t[6] _mac_addr;
        uint8_t[4] _ip_addr;

    public:
        enc28j60();
        
        void initialize(void);

        uint8_t readOp(uint8_t op, uint8_t addr);
        void writeOp(uint8_t op, uint8_t addr, uint8_t * data);
        void readBuffer(uint16_t len, uint8_t * buff);
        void writeBuffer(uint16_t len, uint8_t * buff);
        void setBank(uint8_t addr);
        uint8_t read(uint8_t addr);
        void write(uint8_t write, uint8_t data);
        void phyWrite(uint8_t addr, uint16_t data);
};

#endif

