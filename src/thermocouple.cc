
#ifdef UART_DEBUG
#include <stdio.h>
#endif
#include <avr/io.h>
#include "termocouple.h"

void termocouple::init(void)
{
    // TODO: Initialize ADC
}

double termocouple::get_value(void)
{
    // TODO: Read ADC value, copare it with base temperature
    return 0.0;
}
