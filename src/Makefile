
SRC=main.c
CPU=atxmega32a4
DEV=x32a4
FREQ=25000000UL
DEF_PAR=-Wall -std=gnu99 -gdwarf-2 -Os -funsigned-char -funsigned-bitfields -fpack-struct -fshort-enums

main: $(SRC)
	avr-gcc $(DEF_PAR) -DF_CPU=$(FREQ)  -mmcu=$(CPU) $(SRC) -o zolkko-alarm.elf

hex: zolkko-alarm.elf
	avr-objcopy -j .text -O ihex zolkko-alarm.elf zolkko-alarm.hex

test:
	avrdude -c avrispmkII -P usb -p x32a4 -n -v

upload: hex
	avrdude -c avrispmkII -P usb -p $(DEV)

clean:
	unlink zolkko-alarm.elf
	unlink zolkko-alarm.hex

