
#include <avr/io.h> 
#include "ili9320.h"

int main(void)
{
    ili9320 display;
    display.initialize();
    
    display.chip_select();
    display.clear();
    display.chip_deselect();
    
    while (true) ;
    
    return 0;
}

