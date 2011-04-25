
SRC=utils.cc main.cc ili9320.cc spi.cc enc28j60.cc udp_service.cc usart_d.c
CPU=atxmega128a3
DEV=x128a3
FREQ=2000000UL
DEF_PAR=-Wall -gdwarf-2 -Os -funsigned-char -funsigned-bitfields -fpack-struct -fshort-enums

main: $(SRC)
	avr-gcc $(DEF_PAR) -DF_CPU=$(FREQ)  -mmcu=$(CPU) $(SRC) -o out.elf

asm: $(SRC)
	avr-gcc $(DEF_PAR) -DF_CPU=$(FREQ)  -mmcu=$(CPU) $(SRC) -S

hex: out.elf
	avr-objcopy -j .text -O ihex out.elf out.hex

test:
	avrdude -c avrispmkII -P usb -p x32a4 -n -v

upload: hex
	avrdude -c avrispmkII -P usb -p $(DEV)

all: main hex
	

clean:
	rm ./*.elf
	rm ./*.hex

