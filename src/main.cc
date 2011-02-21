
#include <avr/io.h> 
#include <util/delay_basic.h>
#include <util/delay.h>
#include "ili9320.h"


/*! \brief Enable Singe Channel.
 *
 *  This function configures the DAC to enable channel 0 output only,
 *  in single channel operation. It apply configuration parameters, save old
 *  values, clear affected bit field and set appropriate values.
 *
 *  \param  dac         Pointer to DAC module register section.
 *  \param  convRef     Selects reference voltage for all conversions.
 *  \param  leftAdjust  Set to true to make data registers left adjusted.
 */
void DAC_SingleChannel_Enable( volatile DAC_t * dac,
                              DAC_REFSEL_t convRef,
                              bool leftAdjust )
{
	dac->CTRLB = ( dac->CTRLB & ~DAC_CHSEL_gm ) | DAC_CHSEL_SINGLE_gc;
	dac->CTRLC = ( dac->CTRLC & ~(DAC_REFSEL_gm | DAC_LEFTADJ_bm) ) |
	             convRef | ( leftAdjust ? DAC_LEFTADJ_bm : 0x00 );
	dac->CTRLA = ( dac->CTRLA & ~DAC_CH1EN_bm ) |
	             DAC_CH0EN_bm | DAC_ENABLE_bm;
}

/*! \brief  Write data to selected channel.
 *
 *  This function writes data to the selected channel data register. The program
 *  should wait for the data register to be ready if necessary. This ensures
 *  that no intermediate values are lost with very high update rates.
 *
 *  \note  The data must be in the format currently configured for the DAC,
 *         right or left adjusted.
 *
 *  \param  dac     Pointer to DAC module register section.
 *  \param  data    Data to be converted.
 *  \param  channel Selected channel in the DAC module, either CH0 or CH1.
 */
void DAC_Channel_Write( volatile DAC_t * dac, uint16_t data)
{
    dac->CH0DATA = data;
}

/*! \brief Check if channel data register is empty.
 *
 *  This function return the status of the data register empty flag for
 *  the selected channel in the given DAC module. This can be used to get the
 *  status of the register before writing a new value to it.
 *
 *  \param  dac     Pointer to DAC module register section.
 *  \param  channel Selected channel in the DAC module, either CH0 or CH1.
 *
 *  \retval dacStatus True if data register is empty.
 *  \retval dacStatus False if data register is not empty.
 */
bool DAC_Channel_DataEmpty(volatile DAC_t * dac)
{
	bool dacStatus = (dac->STATUS & DAC_CH0DRE_bm);
	return dacStatus;
}

/*! \brief Configure event actions.
 *
 *  This function configures the event action for the DAC module. It clears
 *  both control bits and set required bits.
 *
 *  \note  There is no checking if the event line number is valid. The value
 *         is simply truncated to fit the bit field in the corresponding register.
 *
 *  \param  dac         Pointer to DAC module register section.
 *  \param  trigChannel The channels to be triggered by events. Values can be
 *                      DAC_TRIG_0_0, DAC_TRIG_0_1, DAC_TRIG_1_0 or DAC_TRIG_0_0.
 *  \param  eventLine   Event line (0..7) to use for triggering conversions.
 */
/*
void DAC_EventAction_Set( volatile DAC_t * dac,
                          DAC_TRIG_t trigChannel,
                          uint8_t eventLine )
{
	dac->CTRLB = ( dac->CTRLB & ~DAC_TRIG_1_1 ) | trigChannel;
	dac->EVCTRL = eventLine & DAC_EVSEL_gm;
}
*/

int main(void)
{
    PORTE.DIRSET = _BV(1);
    
    ili9320 display;
    display.initialize();

    display.chip_select();
    display.clear();
    display.chip_deselect();

#define wi 2
#define he 2

    while (true) {
    for (uint16_t x = 0; x < 32; x++) {
        for (uint16_t y = 0; y < 24; y++) {
            uint16_t x_tmp = x * 10;
            uint16_t y_tmp = y * 10;
            
            display.chip_select();
            display.fill(y_tmp, x_tmp, wi, he, 0xf400);
            display.chip_deselect();
            _delay_ms(5);
        }
        _delay_ms(10);
    }

   for (uint16_t x = 0; x < 32; x++) {
        for (uint16_t y = 0; y < 15; y++) {
            uint16_t x_tmp = x * 10;
            uint16_t y_tmp = y * 10;
            
            display.chip_select();
            display.fill(y_tmp, x_tmp, wi, he, 0xf000);
            display.chip_deselect();
            _delay_ms(10);
        }
        _delay_ms(20);
    }

     for (uint16_t x = 0; x < 32; x++) {
        for (uint16_t y = 0; y < 5; y++) {
            uint16_t x_tmp = x * 10;
            uint16_t y_tmp = y * 10;
            
            display.chip_select();
            display.fill(y_tmp, x_tmp, wi, he, 0x00f4);
            display.chip_deselect();
            _delay_ms(30);
        }
        _delay_ms(40);
    }
    }
    
    /*
    display.chip_select();
    display.fill(0, 0, 50, 50, 0xffff);
    display.chip_deselect();
    
    display.chip_select();
    display.fill(101, 50, 239, 200, 0x0000);
    display.chip_deselect();
    
    display.chip_select();
    display.fill(200, 200, 220, 220, 0xffff);
    display.chip_deselect();
    */
    
    // TODO: Enable DAC
    /*
    DAC_SingleChannel_Enable(&DACB, DAC_REFSEL_AVCC_gc, true); // AREFA_gc, true);
    
    uint16_t data[31] = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0
    };
    
    while (true) {
        for (uint8_t d = 2; d < 10; d++) {
            PORTE.OUTTGL = _BV(1);
            for (uint8_t j = 0; j < 0x1f; j++) {
            for (uint16_t i = 0; i < 31; i++) {
	            while (DAC_Channel_DataEmpty(&DACB) == false) ;
                    DAC_Channel_Write(&DACB, data[i]);
                    _delay_ms(5);
             }
            }

       }
    }
    */

    while (true) ;

    
    return 0;
}

