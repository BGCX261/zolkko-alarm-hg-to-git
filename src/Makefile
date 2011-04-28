## General Flags
PROJECT = smokectrl
MCU = atxmega128a3
FREQ = 2000000UL
BUILD_DIR = build
TARGET = $(BUILD_DIR)/smokectrl.elf
CC = avr-gcc

CPP = avr-g++

## Options common to compile, link and assembly rules
COMMON = -mmcu=$(MCU) -DF_CPU=$(FREQ)

## Compile options common for all C compilation units.
CFLAGS = $(COMMON)
CFLAGS += -Wall -gdwarf-2 -DF_CPU=2000000UL -Os -funsigned-char -funsigned-bitfields -fpack-struct -fshort-enums
CFLAGS += -MD -MP -MT $(BUILD_DIR)/$(*F).o -MF $(BUILD_DIR)/dep/$(@F).d -o $(BUILD_DIR)/$(*F).o

## Assembly specific flags
ASMFLAGS = $(COMMON)
ASMFLAGS += $(CFLAGS)
ASMFLAGS += -x assembler-with-cpp -Wa,-gdwarf2

## Linker flags
LDFLAGS = $(COMMON)
LDFLAGS +=  -Wl,-Map=$(BUILD_DIR)/smokectrl.map


## Intel Hex file production flags
HEX_FLASH_FLAGS = -R .eeprom -R .fuse -R .lock -R .signature

HEX_EEPROM_FLAGS = -j .eeprom
HEX_EEPROM_FLAGS += --set-section-flags=.eeprom="alloc,load"
HEX_EEPROM_FLAGS += --change-section-lma .eeprom=0 --no-change-warnings


## Objects that must be built in order to link
OBJECTS = $(BUILD_DIR)/main.o \
		  $(BUILD_DIR)/uart_stdio.o \
          $(BUILD_DIR)/net_driver.o \
		  $(BUILD_DIR)/enc28j60.o \
		  $(BUILD_DIR)/ili9320.o \
		  $(BUILD_DIR)/spi.o \
		  $(BUILD_DIR)/udp_service.o \
          $(BUILD_DIR)/iface.o \
		  $(BUILD_DIR)/utils.o 

## Objects explicitly added by the user
LINKONLYOBJECTS = 

## Build
all: $(TARGET) $(BUILD_DIR)/smokectrl.hex $(BUILD_DIR)/smokectrl.eep $(BUILD_DIR)/smokectrl.lss size

## Compile
$(BUILD_DIR)/uart_stdio.o: ./uart_stdio.c
	$(CPP) $(INCLUDES) $(CFLAGS) -c  $<

$(BUILD_DIR)/%.o: ./%.cc
	$(CPP) $(INCLUDES) $(CFLAGS) -c  $<

##Link
$(TARGET): $(OBJECTS)
	 $(CPP) $(LDFLAGS) $(OBJECTS) $(LINKONLYOBJECTS) $(LIBDIRS) $(LIBS) -o $(TARGET)

%.hex: $(TARGET)
	avr-objcopy -O ihex $(HEX_FLASH_FLAGS)  $< $@

%.eep: $(TARGET)
	-avr-objcopy $(HEX_EEPROM_FLAGS) -O ihex $< $@ || exit 0

%.lss: $(TARGET)
	avr-objdump -h -S $< > $@

size: ${TARGET}
	@echo
	@avr-size -C --mcu=${MCU} ${TARGET}

## Clean target
.PHONY: clean
clean:
	-rm -rf $(OBJECTS) $(BUILD_DIR)/smokectrl.elf $(BUILD_DIR)/dep/* $(BUILD_DIR)/smokectrl.hex $(BUILD_DIR)/smokectrl.eep $(BUILD_DIR)/smokectrl.lss $(BUILD_DIR)/smokectrl.map


## Other dependencies
-include $(shell mkdir $(BUILD_DIR)/dep 2>/dev/null) $(wildcard $(BUILD_DIR)/dep/*)

