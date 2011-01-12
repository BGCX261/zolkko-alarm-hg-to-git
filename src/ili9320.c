/**
 * AVR XMega Interface for ILI9320 based display
 */

void ili9320_init()
{
    ili9320_reset_low();
    // TODO: Delay 10000 ms x2
    ili9320_reset_hi();
    // TODO: Delay 10000 ms
    
    //initializing funciton 1
        LCD_WR_REG(0xe5,0x8000);
        LCD_WR_REG(0x00,0x0001);
        LCD_WR_REG(0x2b,0x0010);
        LCD_WR_REG(0x01,0x0100);
        LCD_WR_REG(0x02,0x0700);
        LCD_WR_REG(0x03,0x1230);
        LCD_WR_REG(0x04,0x0000);
        LCD_WR_REG(0x08,0x0202);
        LCD_WR_REG(0x09,0x0000);
        LCD_WR_REG(0x0a,0x0000);
        LCD_WR_REG(0x0c,0x0000);
        LCD_WR_REG(0x0d,0x0000);
        LCD_WR_REG(0x0f,0x0000);
        LCD_WR_REG(0x50,0x0000);
        LCD_WR_REG(0x51,0x00ef);
        LCD_WR_REG(0x52,0x0000);
        LCD_WR_REG(0x53,0x013f);
        LCD_WR_REG(0x60,0x2700);
        LCD_WR_REG(0x61,0x0001);
        LCD_WR_REG(0x6a,0x0000);
        LCD_WR_REG(0x80,0x0000);
        LCD_WR_REG(0x81,0x0000);
        LCD_WR_REG(0x82,0x0000);
        LCD_WR_REG(0x83,0x0000);
        LCD_WR_REG(0x84,0x0000);
        LCD_WR_REG(0x85,0x0000);
        LCD_WR_REG(0x90,0x0010);
        LCD_WR_REG(0x92,0x0000);
        LCD_WR_REG(0x93,0x0003);
        LCD_WR_REG(0x95,0x0110);
        LCD_WR_REG(0x97,0x0000);
        LCD_WR_REG(0x98,0x0000);

        //power setting function
        LCD_WR_REG(0x10,0x0000);
        LCD_WR_REG(0x11,0x0000);
        LCD_WR_REG(0x12,0x0000);
        LCD_WR_REG(0x13,0x0000);
        //_delay_loop_2 (10000);
        
        LCD_WR_REG(0x10,0x17b0);
        LCD_WR_REG(0x11,0x0004);
        //_delay_loop_2 (10000);
        
        LCD_WR_REG(0x12,0x013e);
        //_delay_loop_2 (10000);
        
        LCD_WR_REG(0x13,0x1f00);
        LCD_WR_REG(0x29,0x000f);
        _delay_loop_2 (0xffff); 
        
        LCD_WR_REG(0x20,0x0000);
        LCD_WR_REG(0x21,0x0000);

        //initializing function 2

        LCD_WR_REG(0x30,0x0204);
        LCD_WR_REG(0x31,0x0001);
        LCD_WR_REG(0x32,0x0000);
        LCD_WR_REG(0x35,0x0206);
        LCD_WR_REG(0x36,0x0600);
        LCD_WR_REG(0x37,0x0500);
        LCD_WR_REG(0x38,0x0505);
        LCD_WR_REG(0x39,0x0407);
        LCD_WR_REG(0x3c,0x0500);
        LCD_WR_REG(0x3d,0x0503);

        //display on
        LCD_WR_REG(0x07,0x0173);
}

void LCD_WR_Start()
{
        CS_L;
        RS_L;
        
        DATA_L = 0x22;
        DATA_H = 0x00;
        WR_L;
        nop;
        nop;
        WR_H;
        RS_H;
}
void LCD_WR_Data(uint val)
{
        DATA_L = (uchar)val;
        DATA_H = (uchar)(val>>8);
        WR_L;
        //nop;
        //nop;
        WR_H;
}

void LCD_WR_End()
{
        CS_H;
}

void LCD_WR_REG(uchar index,uint val)
{
        CS_L;
        RS_L;
        
        DATA_L=index;
        DATA_H=0;
        
        WR_L;
        //nop;
        //nop;
        WR_H;
        RS_H;
        DATA_L=(uchar)val;
        DATA_H=(uchar)(val>>8);
        
        WR_L;
        //nop;
        //nop;
        WR_H;
        CS_H;
}

