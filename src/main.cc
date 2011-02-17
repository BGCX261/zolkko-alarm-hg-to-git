
#include <avr/io.h> 
#include <util/delay_basic.h>
#include "ili9320.h"


int main(void)
{
    PORTE.DIRSET = _BV(1);
    
    ili9320 display;
    display.initialize();
    
    while (true) {
        for (uint16_t i = 0; i < 0xffff; i++) {
            //display.chip_select();
            //display.clear();
            //display.chip_deselect();
            //PORTE.OUTTGL = _BV(1);
            //_delay_loop_2(20000);

            display.chip_select();
            display.window(10, 10, 100, 100);
            display.clear();
            display.chip_deselect();
            PORTE.OUTTGL = _BV(1);
            _delay_loop_2(20000);

            display.chip_select();
            display.window(100, 100, 200, 200);
            display.clear();
            display.chip_deselect();
            PORTE.OUTTGL = _BV(1);
            _delay_loop_2(20000);
        }
    }
    
    return 0;
}

