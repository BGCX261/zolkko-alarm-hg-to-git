
SRC=main.cc ili9320.cc enc28j60.cc utils.cc spi.cc udp_service.cc utils.cc
CPU=atxmega128a3
DEV=x128a3
FREQ=2000000UL
DEF_PAR=-Wall -gdwarf-2 -Os -funsigned-char -funsigned-bitfields -fpack-struct -fshort-enums

main: $(SRC)
	avr-gcc $(DEF_PAR) -DF_CPU=$(FREQ)  -mmcu=$(CPU) $(SRC) -o zolkko-alarm.elf

asm: $(SRC)
	avr-gcc $(DEF_PAR) -DF_CPU=$(FREQ)  -mmcu=$(CPU) $(SRC) -S

hex: zolkko-alarm.elf
	avr-objcopy -j .text -O ihex zolkko-alarm.elf zolkko-alarm.hex

test:
	avrdude -c avrispmkII -P usb -p x32a4 -n -v

upload: hex
	avrdude -c avrispmkII -P usb -p $(DEV)

all: main hex
	

clean:
	unlink zolkko-alarm.elf
	unlink zolkko-alarm.hex

