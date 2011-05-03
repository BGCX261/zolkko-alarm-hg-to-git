/*
 *
 */
#ifndef _application_h_
#define _application_h_


typedef struct _time_t {
    uint8_t second;
    uint8_t minute;
    uint8_t hour;
} time_t;


class Application
{
    private:
        /*
         * Construct instance at start-up time
         */
        static Application _app;
        
        time_t _time;
        
        /*
         * Initialize RTC
         */
        void init_rtc(void);
        
        Application()
        {
            _time.second = 0;
            _time.minute = 0;
            _time.hour   = 0;
        }
        
    public:
        static Application& Instance()
        {
            return _app;
        }
        
        /*
         * Main loop
         */
        void run(void);
};

#endif

