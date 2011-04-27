/**
 * Based on the net.h file from the AVRlib library by Pascal Stang.
 */

#ifndef _udp_service_h_
#define _udp_service_h_

#define ETH_HEADER_LEN	14

// Values of certain bytes:
#define ETHTYPE_ARP_H_V 0x08
#define ETHTYPE_ARP_L_V 0x06
#define ETHTYPE_IP_H_V  0x08
#define ETHTYPE_IP_L_V  0x00

// Ethernet type field (2bytes):
#define ETH_TYPE_H_P 12
#define ETH_TYPE_L_P 13

//
#define ETH_DST_MAC 0
#define ETH_SRC_MAC 6

// ******* ARP *******
#define ETH_ARP_OPCODE_REPLY_H_V 0x0
#define ETH_ARP_OPCODE_REPLY_L_V 0x02

#define ETHTYPE_ARP_L_V 0x06

#define ETH_ARP_DST_IP_P 0x26

#define ETH_ARP_OPCODE_H_P 0x14
#define ETH_ARP_OPCODE_L_P 0x15

#define ETH_ARP_SRC_MAC_P 0x16
#define ETH_ARP_SRC_IP_P 0x1c
#define ETH_ARP_DST_MAC_P 0x20
#define ETH_ARP_DST_IP_P 0x26

// ******* IP *******
#define IP_HEADER_LEN	20
#define IP_SRC_P 0x1a
#define IP_DST_P 0x1e
#define IP_CHECKSUM_P 0x18
#define IP_TTL_P 0x16
#define IP_FLAGS_P 0x14
#define IP_P 0xe
#define IP_TOTLEN_H_P 0x10
#define IP_TOTLEN_L_P 0x11

#define IP_PROTO_P 0x17  

#define IP_PROTO_ICMP_V 1
#define IP_PROTO_TCP_V 6
#define IP_PROTO_UDP_V 17

// ******* ICMP *******
#define ICMP_TYPE_ECHOREPLY_V 0
#define ICMP_TYPE_ECHOREQUEST_V 8

#define ICMP_TYPE_P 0x22
#define ICMP_CHECKSUM_P 0x24

// ******* UDP *******
#define UDP_HEADER_LEN	8

#define UDP_SRC_PORT_H_P 0x22
#define UDP_SRC_PORT_L_P 0x23
#define UDP_DST_PORT_H_P 0x24
#define UDP_DST_PORT_L_P 0x25

#define UDP_LEN_H_P 0x26
#define UDP_LEN_L_P 0x27
#define UDP_CHECKSUM_H_P 0x28
#define UDP_CHECKSUM_L_P 0x29
#define UDP_DATA_P 0x2a


class udp_service
{
    private:
        iface& _iface;
        
        void make_eth(uint8_t * buf, const ether_addr_t * dst_addr);
        
        void make_ip(uint8_t * buf, const ip_addr_t * dst_addr);
        
        static uint16_t checksum(uint8_t * buf, uint16_t len, uint8_t type);
        
    public:
        udp_service(iface& __iface) : _iface(__iface)
        {
        }
        
        inline uint8_t is_echo(uint8_t * buf)
        {
            return buf[IP_PROTO_P] == IP_PROTO_ICMP_V &&
                   buf[ICMP_TYPE_P] == ICMP_TYPE_ECHOREQUEST_V;
        }
        
        inline uint8_t is_udp(uint8_t * buf)
        {
            return buf[IP_PROTO_P] == IP_PROTO_UDP_V;
        }
        
        uint8_t is_arp_and_myip(uint8_t * buf, uint8_t len);
        
        uint8_t is_ip_and_myip(uint8_t * buf, uint8_t len);
        
        void arp_answer_from_request(uint8_t * buf, uint8_t len);
        
        void echo_reply_from_request(uint8_t * buf, uint8_t len);
        
        void reply_from_request(uint8_t * buf, char * data, uint8_t datalen, uint16_t port);
        
        void send_to(ether_addr_t * dst_mac, ip_addr_t * dst_ip, uint16_t dst_port, uint8_t * data, uint8_t data_len);
};

#endif

