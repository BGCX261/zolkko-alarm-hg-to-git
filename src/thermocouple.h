/*
 * Class reads temperature from termocouple
 */
#ifndef _termocouple_h_
#define _termocouple_h_

class termocouple : public sensor
{
    private:
        sensor& _base;
    
    public:
        termocouple(sensor& __base) :
            sensor(),
            _base(__base)
        {
        }
        
        void init(void);
        
        double get_value(void);
};

#endif

