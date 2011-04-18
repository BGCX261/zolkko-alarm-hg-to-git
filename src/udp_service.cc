
#include <string.h>
#include "iface.h"
#include "udp_service.h"


Udp::Udp(void)
{
}

/**
 * Format ethernet frame and populate destination mac address
 */
void Udp::make_eth(uint8_t * buf, const mac_addr_t dest_addr)
{
    const mac_addr_t src_addr = this->_iface->get_mac_addr();
    
    memcpy(&buf[ETH_DST_MAC], dest_addr, IF_MAC_ADDR_LEN);
    memcpy(&buf[ETH_SRC_MAC], src_addr, IF_MAC_ADDR_LEN);
}

/**
 * Format IP packet
 */
void Udp::make_ip(uint8_t * buf, const ip_addr_t dest_addr)
{
    const ip_addr_t src_addr = this->_iface->get_ip();
    
    memcpy(&buf[IP_DST_P], dest_addr, IF_IP_ADDR_LEN);
    memcpy(&buf[IP_SRC_P], source_ip, IF_IP_ADDR_LEN);
    
    // Clear the 2 byte checksum
    buf[IP_CHECKSUM_P] = 0;
    buf[IP_CHECKSUM_P + 1] = 0;
    
    buf[IP_FLAGS_P] = 0x40; // don't fragment
    buf[IP_FLAGS_P + 1] = 0;  // fragement offset
    buf[IP_TTL_P] = 64; // ttl
    
    // calculate the checksum:
    uint16_t ck = Udp::checksum(&buf[IP_P], IP_HEADER_LEN, 0);
    buf[IP_CHECKSUM_P] = ck >> 8;
    buf[IP_CHECKSUM_P + 1] = ck & 0xff;
}

uint8_t Udp::is_arp_and_myip(uint8_t * buf, uint8_t len)
{
    if (len < 41) {
        return 0;
    }
    
    if(buf[ETH_TYPE_H_P] != ETHTYPE_ARP_H_V ||
            buf[ETH_TYPE_L_P] != ETHTYPE_ARP_L_V)
    {
        return 0;
    }
    
    ip_addr_t src_addr = this->_iface->get_ip_addr();
    
    uint8_t i = 0;
    while (i < IF_IP_ADDR_LEN) {
        if (buf[ETH_ARP_DST_IP_P + i] != src_addr[i]) return 0;
        i++;
    }
    
    return 1;
}

/**
 * First verify that buf is an ip packet
 * Then compare packet IP address
 */
uint8_t Udp::is_ip_and_myip(uint8_t * buf, uint8_t len)
{
    //eth + ip + udp header is 42
    if (len < 42) {
        return 0;
    }
    
    if (buf[ETH_TYPE_H_P] != ETHTYPE_IP_H_V || buf[ETH_TYPE_L_P] != ETHTYPE_IP_L_V) {
        return 0;
    }
    
    uint8_t i = 0;
    
    ip_addr_t src_addr = this->_iface->get_ip_addr();
    while (i < IF_IP_ADDR_LEN) {
        if (buf[IP_DST_P + i] != src_addr[i]) {
            return 0;
        }
        i++;
    }
    
    return 1;
}

void Udp::arp_answer_from_request(uint8_t * buf, uint8_t len)
{
    this->make_eth(buf);
    
    buf[ETH_ARP_OPCODE_H_P] = ETH_ARP_OPCODE_REPLY_H_V;
    buf[ETH_ARP_OPCODE_L_P] = ETH_ARP_OPCODE_REPLY_L_V;
    
    // fill the mac addresses:
    mac_addr_t src_mac = this->_iface->get_mac_addr();
    
    memcpy(&buf[ETH_ARP_DST_MAC_P], &buf[ETH_ARP_SRC_MAC_P], IF_MAC_ADDR_LEN);
    memcpy(&buf[ETH_ARP_SRC_MAC_P], src_mac, IF_MAC_ADDR_LEN);
    
    ip_addr_t src_ip = this->_iface->get_ip_addr();

    memcpy(&buf[ETH_ARP_DST_IP_P], &buf[ETH_AR_SRC_IP_P], IF_IP_ADDR_LEN);
    memcpy(&buf[ETH_ARP_SRC_IP_P], src_ip, IF_IP_ADDR_LEN);
    
    // eth + arp is 42 bytes:
    this->_iface->send_packet(42, buf);
}

/**
 * Echo (ICMP) replay from request (buf)
 */
void Udp::echo_reply_from_request(uint8_t * buf, uint8_t len)
{
    this->make_eth(buf);
    this->make_ip(buf);
    
    buf[ICMP_TYPE_P] = ICMP_TYPE_ECHOREPLY_V;
    
    // we changed only the icmp.type field from request(=8) to reply(=0).
    // we can therefore easily correct the checksum:
    if (buf[ICMP_CHECKSUM_P] > (0xff - 0x08)) {
        buf[ICMP_CHECKSUM_P + 1]++;
    }
    buf[ICMP_CHECKSUM_P] += 0x08;
    
    this->_iface->send_packet(len, buf);
}

/**
 * UDP replay to received packet.
 *
 * port - source/sender port (our port),
 *        destination port is taken fom original packet.
 */
void Udp::reply_from_request(uint8_t * buf, char * data, uint8_t datalen, uint16_t port)
{
    uint8_t i = 0;
    
    this->make_eth(buf);
    
    if (datalen > 220) {
        datalen = 220;
    }
    
    // total length field in the IP header must be set:
    buf[IP_TOTLEN_H_P] = 0;
    buf[IP_TOTLEN_L_P] = IP_HEADER_LEN + UDP_HEADER_LEN + datalen;
    
    this->make_ip(buf);
    
    // Sent to port of sender and use "port" as own source:
    buf[UDP_DST_PORT_H_P] = buf[UDP_SRC_PORT_H_P];
    buf[UDP_DST_PORT_L_P] = buf[UDP_SRC_PORT_L_P];
    
    // Source port does not matter and is what the sender used.
    buf[UDP_SRC_PORT_H_P] = port >> 8;
    buf[UDP_SRC_PORT_L_P] = port & 0xff;
    
    // calculte the udp length:
    buf[UDP_LEN_H_P] = 0;
    buf[UDP_LEN_L_P] = UDP_HEADER_LEN + datalen;
    
    // zero the checksum
    buf[UDP_CHECKSUM_H_P] = 0;
    buf[UDP_CHECKSUM_L_P] = 0;
    
    // copy the data:
    while (i < datalen) {
        buf[UDP_DATA_P+i] = data[i];
        i++;
    }
    
    // calculate IP checksum
    uint16_t ck = Udp::checksum(&buf[IP_SRC_P], 16 + datalen, 1);
    buf[UDP_CHECKSUM_H_P] = ck >> 8;
    buf[UDP_CHECKSUM_L_P] = ck & 0xff;
    
    this->_iface.packet_send(UDP_HEADER_LEN + IP_HEADER_LEN + ETH_HEADER_LEN + datalen, buf);
}

/**
 * Calculate checksum
 * len - proto data length + proto header len
 */
static uint16_t Udp::checksum(uint8_t * buf, uint16_t len, uint8_t type)
{
    uint32_t sum = 0;
    
    if (type == 1) {
        sum += IP_PROTO_UDP_V;
        sum += len - 8; //  - udp length
    } else if (type == 2) {
        sum += IP_PROTO_TCP_V; 
        sum += len - 8; // - tcp length
    }
    // IP packet otherwise
    
    while (len > 1) {
        sum += 0xffff & (*buf << 8 | *(buf + 1));
        buf += 2;
        len -= 2;
    }
    
    if (len) {
        sum += (0xff & *buf) << 8;
    }
    
    while (sum >> 16) {
        sum = (sum & 0xffff) + (sum >> 16);
    }
    
    // build 1's complement:
    return ((uint16_t) sum ^ 0xffff);
}

